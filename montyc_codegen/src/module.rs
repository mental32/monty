use std::{
    convert::TryInto,
    io,
    path::{Path, PathBuf},
    rc::Rc,
};

use ahash::AHashMap;
use cranelift_codegen::{
    binemit,
    ir::{self, ExternalName, Function, Signature},
    isa::{CallConv, TargetIsa},
    verify_function, Context,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use montyc_core::{patma, TypeId};
use montyc_hlir::{
    glue::HostGlue,
    typing::{PythonType, TypingContext},
    value_store::ValueGraphIx,
    Const, RawInst,
};

use crate::lower::BuilderContext;

#[derive(Debug)]
pub(crate) struct Func {
    pub(crate) hlir: Rc<montyc_hlir::Function>,
    pub(crate) clir: Function,
    pub(crate) signature: Signature,
}

#[derive(Debug, Default)]
pub struct CodegenModule {
    pub(crate) types: AHashMap<TypeId, ir::types::Type>,
    pub(crate) functions: AHashMap<ValueGraphIx, Rc<Func>>,
}

impl CodegenModule {
    #[inline]
    pub(crate) fn scalar_type_of(&self, tid: &TypeId) -> ir::Type {
        if tid.is_builtin() {
            // builtin types probably have a scalar representation.
            match self.types.get(&tid) {
                Some(ty) => ty.clone(),
                None => unimplemented!("no scalar representation for builtin {:?}", tid),
            }
        } else {
            // everything else is just a reference.
            ir::types::R64
        }
    }

    fn build_functions(
        &mut self,
        host: &mut dyn HostGlue,
        isa: Box<dyn TargetIsa>,
    ) -> ObjectModule {
        let fisa = isa.flags().clone();
        let mut object_module = ObjectModule::new({
            let name = String::from("<empty>");
            let libcall_names = cranelift_module::default_libcall_names();

            ObjectBuilder::new(isa, name, libcall_names).unwrap()
        });

        let (functions, stubbed) = {
            let mut f = AHashMap::<ValueGraphIx, Rc<Func>>::default();
            let mut s = AHashMap::<ValueGraphIx, Rc<Func>>::default();

            for (ix, func) in self.functions.iter() {
                match func.hlir.code.inst() {
                    [] => unreachable!(),

                    [one] if matches!(one.op, RawInst::Const(Const::Ellipsis)) => {
                        s.insert(*ix, func.clone());
                    }

                    _ => {
                        f.insert(*ix, func.clone());
                    }
                };
            }

            (f, s)
        };

        let external_names = {
            let mut names = AHashMap::with_capacity(functions.len());

            for (ix, func) in self.functions.iter() {
                names.insert(
                    *ix,
                    ExternalName::User {
                        namespace: func.hlir.mref.0,
                        index: ix.index().try_into().unwrap(),
                    },
                );
            }

            names
        };

        let fids = {
            let mut fids = AHashMap::with_capacity(self.functions.len());
            let mut tcx = host.tcx().borrow_mut();

            for (key, func) in self.functions.iter() {
                let f_type = tcx.get_type_mut(func.hlir.type_id).unwrap();
                let f_linkage = f_type.layout.linkage.unwrap_or(Linkage::Local);
                let f_name = dbg!(host.get_qualname(func.hlir.value_ix).join("_"));

                let fid = object_module
                    .declare_function(&f_name, f_linkage, &func.clir.signature)
                    .unwrap();

                fids.insert(*key, fid);
            }

            fids
        };

        for (func_ix, func) in functions.iter() {
            let mut func = func.clir.clone();
            let fid = fids.get(func_ix).unwrap();

            let object_module = &mut object_module;

            let bx = BuilderContext {
                host,
                cg_module: self,
                object_module,
                func_ix: *func_ix,
                fid: *fid,
                fids: &fids,
            };

            bx.build(&mut func);

            if let Err(err) = verify_function(&func, &fisa) {
                panic!("{}", err);
            }

            let mut ctx = Context::for_function(func);
            let mut ts = binemit::NullTrapSink {};
            let mut ss = binemit::NullStackMapSink {};

            object_module
                .define_function(*fid, &mut ctx, &mut ts, &mut ss)
                .unwrap();

            // let _ = std::mem::replace(&mut self.functions.get_mut(func_ix).unwrap().clir, func);
        }

        object_module
    }
}

impl CodegenModule {
    /// Get a new, blank, codegen module.
    pub fn new() -> Self {
        // builtin tscalar types and their CL equivalents.
        let types = {
            let mut map = AHashMap::with_capacity(64);

            map.insert(TypingContext::Int, ir::types::I64);
            map.insert(TypingContext::Bool, ir::types::B64);
            map.insert(TypingContext::Float, ir::types::F64);
            map.insert(TypingContext::TSelf, ir::types::R64);

            map.insert(TypingContext::I8, ir::types::I8);
            map.insert(TypingContext::U8, ir::types::I8);

            map.insert(TypingContext::I16, ir::types::I16);
            map.insert(TypingContext::U16, ir::types::I16);

            map.insert(TypingContext::U32, ir::types::I32);
            map.insert(TypingContext::I32, ir::types::I32);

            map.insert(TypingContext::U64, ir::types::I64);
            map.insert(TypingContext::I64, ir::types::I64);

            map
        };

        Self {
            types,
            ..Default::default()
        }
    }

    /// Include a `hlir::Function` for codegen.
    #[inline]
    pub fn include_function(&mut self, host: &mut dyn HostGlue, func: montyc_hlir::Function) {
        log::trace!(
            "[CodegenModule::include_function] Adding function for codegen: {{ name: {:?}, val: {:?}, module: {:?}, type: {:?} }}",
            host.spanref_to_str(func.name),
            func.value_ix,
            func.mref,
            func.type_id,
        );

        let name = ExternalName::User {
            namespace: func.mref.0,
            index: func.value_ix.index().try_into().unwrap(),
        };

        let sig = {
            let mut tcx = host.tcx().borrow_mut();

            let f_type = tcx.get_type_mut(func.type_id).unwrap();
            let f_callconv = f_type.layout.callcov.unwrap_or(CallConv::SystemV);

            let mut sig = Signature::new(f_callconv);

            let (ret, args) =
                patma!((*ret, args.as_ref()), PythonType::Callable { ret, args } in &f_type.kind)
                    .unwrap();

            if ret != TypingContext::None {
                let ty = self.scalar_type_of(&ret);

                sig.returns.push(ir::AbiParam::new(ty));
            }

            for param in args.unwrap_or(&vec![]).iter() {
                let ty = self.scalar_type_of(param);

                sig.params.push(ir::AbiParam::new(ty));
            }

            sig
        };

        self.functions.insert(
            func.value_ix,
            Rc::new(Func {
                hlir: Rc::new(func),
                clir: ir::Function::with_name_signature(name, sig.clone()),
                signature: sig,
            }),
        );
    }

    /// Submit all included functions for codegen and write the output object to the specified `output` argument or a temporary file.
    pub fn finish<P>(
        mut self,
        host: &mut dyn HostGlue,
        output: Option<P>,
        isa: Box<dyn TargetIsa>,
    ) -> io::Result<PathBuf>
    where
        P: AsRef<Path>,
    {
        let object_module = self.build_functions(host, isa);

        let product = object_module.finish();
        let bytes = product.emit().unwrap();

        let path = match output {
            Some(path) => {
                std::fs::write(&path, bytes)?;
                path.as_ref().to_owned()
            }

            None => {
                let mut file = tempfile::NamedTempFile::new().unwrap();

                std::io::Write::write_all(&mut file, &bytes).unwrap();

                let path = file.path().clone().to_owned();

                file.persist(&path).unwrap();

                path
            }
        };

        Ok(path)
    }
}
