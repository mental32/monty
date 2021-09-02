use std::{
    convert::TryInto,
    io,
    path::{Path, PathBuf},
    rc::Rc,
};

use ahash::AHashMap;
use cranelift_codegen::{
    ir::{self, ExternalName, Function, Signature},
    isa::{CallConv, TargetIsa},
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use montyc_core::{patma, TypeId};
use montyc_hlir::{
    glue::HostGlue,
    typing::{PythonType, TypingContext},
    value_store::ValueGraphIx,
};

#[derive(Debug)]
struct Func {
    hlir: Rc<montyc_hlir::Function>,
    clir: Function,
    signature: Signature,
}

#[derive(Debug, Default)]
pub struct CodegenModule {
    types: AHashMap<TypeId, ir::types::Type>,
    functions: AHashMap<ValueGraphIx, Func>,
}

impl CodegenModule {
    #[inline]
    fn scalar_type_of(&self, tid: &TypeId) -> ir::Type {
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
        let mut object_module = ObjectModule::new({
            let name = String::from("<empty>");
            let libcall_names = cranelift_module::default_libcall_names();

            ObjectBuilder::new(isa, name, libcall_names).unwrap()
        });

        let mut tcx = host.tcx().borrow_mut();

        let external_names = {
            let mut names = AHashMap::with_capacity(self.functions.len());

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

        todo!();

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
            map.insert(TypingContext::Bool, ir::types::I64);
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
            "[CodegenModule::include_function] Adding function for codegen: {{ name: {:?} module: {:?} type: {:?} }}",
            func.name,
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
            Func {
                hlir: Rc::new(func),
                clir: ir::Function::with_name_signature(name, sig.clone()),
                signature: dbg!(sig),
            },
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
