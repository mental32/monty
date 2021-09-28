use std::{
    convert::TryFrom,
    io,
    num::TryFromIntError,
    path::{Path, PathBuf},
};

use ahash::AHashMap;
use cranelift_codegen::{
    binemit,
    ir::{self, ExtFuncData, ExternalName, Function, Signature},
    isa::{CallConv, TargetIsa},
    settings::Flags,
    verify_function, Context,
};

use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Linkage, Module, ModuleResult};
use cranelift_object::{ObjectBuilder, ObjectModule};
use montyc_core::{patma, MapT, PythonType, TypeId, TypingConstants, Value, ValueId};

use montyc_flatcode::FlatSeq;
use montyc_query::Queries;
use tempfile::NamedTempFile;

use crate::lower::LoweringContext;

// use crate::lower::BuilderContext;

#[derive(Debug, Clone)]
struct Func {
    hlir: montyc_core::Function,
    code: FlatSeq,
    name: ExternalName,
    sig: Signature,
}

#[derive(Debug)]
pub(crate) struct CgData {
    /// A mapping of function value indecies to `Func`.
    funcs: MapT<ValueId, Func>,

    // HACK: The docs for `ExternalName` lie! The inner values are not arbitrary and do have special meaning when using `cranelift_object` wrt FuncIds.
    //
    // We need to keep the order of functions when we include them so that the declare, define, and importing logic works correctly.
    // At the moment we can simply store a tuple of `(name, linkage, signature)`
    _ordered_by_definition: Vec<(Box<str>, Linkage, Signature, Option<ValueId>)>,
    // TODO: include data storage here.
}

#[derive(Debug)]
pub struct CodegenModule {
    types: MapT<TypeId, ir::types::Type>,
    pub(crate) data: CgData,
}

// impl CodegenModule {
//     fn define_main(
//         &self,
//         entry: (ExternalName, Signature),
//         fisa: &Flags,
//         object_module: &mut ObjectModule,
//     ) {
//         use cranelift_codegen::ir::InstBuilder;

//         let mut main_fn = Function::with_name_signature(
//             ExternalName::User {
//                 namespace: 0,
//                 index: self.data._ordered_by_definition.len() as u32 + 1,
//             },
//             signature!(CallConv::SystemV, &[], ir::types::I64),
//         );

//         let (entry_name, entry_sig) = entry;

//         let fid = object_module
//             .declare_function(&"main", Linkage::Export, &entry_sig)
//             .unwrap();

//         {
//             let mut cx = FunctionBuilderContext::new();
//             let mut fx = FunctionBuilder::new(&mut main_fn, &mut cx);

//             let data = ExtFuncData {
//                 name: entry_name.clone(),
//                 signature: fx.import_signature(entry_sig.clone()),
//                 colocated: true,
//             };

//             let entry_ref = fx.import_function(data);

//             let start = fx.create_block();

//             fx.switch_to_block(start);

//             let ret = fx.ins().call(entry_ref, &[]);
//             let ret = fx.inst_results(ret)[0];
//             fx.ins().return_(&[ret]);

//             fx.seal_all_blocks();
//         }

//         if let Err(err) = verify_function(&main_fn, fisa) {
//             panic!("{}", err);
//         }

//         let mut ctx = Context::for_function(main_fn);
//         let mut ts = binemit::NullTrapSink {};
//         let mut ss = binemit::NullStackMapSink {};

//         object_module
//             .define_function(fid, &mut ctx, &mut ts, &mut ss)
//             .unwrap();
//     }

//     fn define_builtins(
//         &self,
//         mut stubbed: MapT<ValueId, &Func>,
//         host: &dyn Queries,
//         object_module: &mut ObjectModule,
//         fisa: &Flags,
//         fids: &MapT<ValueId, cranelift_module::FuncId>,
//     ) -> Result<(), String> {
//         let bltns = crate::builtins::create_builtins(self, object_module);

//         for (ix, f) in stubbed.drain() {
//             let fid = fids[&ix];
//             let value_t = host.get_type_of(ix).unwrap();
//             let tcx = host.tcx();

//             if let PythonType::Callable { args, ret } = tcx.get_python_type_of(value_t).unwrap() {
//                 let klass_t = value_t;

//                 let name = match name
//                     .clone()
//                     .map(|sref| host.spanref_to_str(sref).unwrap().to_owned())
//                 {
//                     Ok(st) => st,
//                     Err(st) => st,
//                 };

//                 let key = (klass_t, name.as_str());

//                 let tcx = host.tcx();

//                 let mut func = match bltns.get(&key) {
//                     Some(func) => func.clone(),
//                     None => {
//                         panic!(
//                             "Missing builtin function for type {:?} method {}",
//                             tcx.get_python_type_of(klass_t).unwrap(),
//                             name
//                         )
//                     }
//                 };

//                 func.name = f.name.clone();

//                 if let Err(err) = verify_function(&func, fisa) {
//                     let err =
//                         cranelift_codegen::print_errors::pretty_verifier_error(&func, None, err);

//                     return Err(err);
//                 }

//                 let mut ctx = Context::for_function(func.clone());
//                 let mut ts = binemit::NullTrapSink {};
//                 let mut ss = binemit::NullStackMapSink {};

//                 log::info!(
//                     "Defining {:?} {} -> {}",
//                     tcx.get_python_type_of(klass_t),
//                     name,
//                     fid
//                 );

//                 object_module
//                     .define_function(fid, &mut ctx, &mut ts, &mut ss)
//                     .unwrap();
//             } else {
//                 unimplemented!()
//             }
//         }

//         Ok(())
//     }

//     fn build_func<'a>(
//         &self,
//         func: &Func,
//         fids: &MapT<ValueId, FuncId>,
//         func_ix: ValueId,
//         object_module: &mut ObjectModule,
//         host: &dyn Queries,
//         fisa: &Flags,
//         builder_cx: &'a mut FunctionBuilderContext,
//     ) -> ModuleResult<&'a mut FunctionBuilderContext> {
//         let Func {
//             name,
//             sig,
//             hlir,
//             code,
//         } = func;

//         let mut func = Function::with_name_signature(name.clone(), sig.clone());

//         let fid = match fids.get(&func_ix) {
//             Some(fid) => fid,
//             None => panic!("Missing FuncId for a Func ValueIx {:?}", func_ix),
//         };

//         let object_module = object_module;

//         LoweringContext::new(host, self, &mut func, hlir, code)
//             .declare_func_refs(object_module, fids)
//             .build(builder_cx);

//         if let Err(err) = verify_function(&func, fisa) {
//             let err = cranelift_codegen::print_errors::pretty_verifier_error(&func, None, err);

//             panic!("{}", err);
//         }

//         log::info!("Defining user function {:?} -> {}", func_ix, fid);
//         log::info!("{:?}", func);

//         let mut ctx = Context::for_function(func);
//         let mut ts = binemit::NullTrapSink {};
//         let mut ss = binemit::NullStackMapSink {};

//         object_module.define_function(*fid, &mut ctx, &mut ts, &mut ss)?;

//         Ok(builder_cx)
//     }

//     fn build_all_functions(
//         &mut self,
//         host: &dyn Queries,
//         isa: Box<dyn TargetIsa>,
//         entry_ix: ValueId,
//     ) -> Result<ObjectModule, String> {
//         let fisa = isa.flags().clone();
//         let mut object_module = ObjectModule::new({
//             let name = String::from("<empty>");
//             let libcall_names = cranelift_module::default_libcall_names();

//             ObjectBuilder::new(isa, name, libcall_names).unwrap()
//         });

//         let (stubbed, defined) = self
//             .data
//             .funcs
//             .iter()
//             .map(|(ix, func)| (*ix, func))
//             .partition::<AHashMap<_, _>, _>(|(_, func)| func.code.is_stubbed());

//         let fids = {
//             let mut fids = AHashMap::with_capacity(self.data._ordered_by_definition.len());

//             for (f_name, f_linkage, signature, ix) in self.data._ordered_by_definition.iter() {
//                 let fid = object_module
//                     .declare_function(&f_name, f_linkage.clone(), &signature)
//                     .unwrap();

//                 log::info!("Declaring function {:?} -> {}", f_name, fid);

//                 if let Some(ix) = ix {
//                     fids.insert(*ix, fid);
//                 }
//             }

//             fids
//         };

//         let _ = self.define_builtins(stubbed, host, &mut object_module, &fisa, &fids)?;

//         let entry = {
//             let Func { name, sig, .. } = defined[&entry_ix];
//             (name.clone(), sig.clone())
//         };

//         let _ = self.define_main(entry, &fisa, &mut object_module);

//         let mut builder_cx = FunctionBuilderContext::new();

//         for (func_ix, func) in defined {
//             if let Err(err) = self.build_func(
//                 func,
//                 &fids,
//                 func_ix,
//                 &mut object_module,
//                 host,
//                 &fisa,
//                 &mut builder_cx,
//             ) {
//                 todo!("{:?}", err);
//             }
//         }

//         Ok(object_module)
//     }
// }

impl CodegenModule {
    //     /// Construct a new, blank, codegen module.
    //     pub fn new() -> Self {
    //         // builtin tscalar types and their CL equivalents.
    //         let types = {
    //             let mut map = AHashMap::with_capacity(64);

    //             map.insert(TypingConstants::Int, ir::types::I64);
    //             map.insert(TypingConstants::Bool, ir::types::I64);
    //             map.insert(TypingConstants::Float, ir::types::F64);
    //             map.insert(TypingConstants::TSelf, ir::types::I64);

    //             map.insert(TypingConstants::I8, ir::types::I8);
    //             map.insert(TypingConstants::U8, ir::types::I8);

    //             map.insert(TypingConstants::I16, ir::types::I16);
    //             map.insert(TypingConstants::U16, ir::types::I16);

    //             map.insert(TypingConstants::U32, ir::types::I32);
    //             map.insert(TypingConstants::I32, ir::types::I32);

    //             map.insert(TypingConstants::U64, ir::types::I64);
    //             map.insert(TypingConstants::I64, ir::types::I64);

    //             map
    //         };

    //         Self {
    //             types,
    //             data: CgData {
    //                 funcs: Default::default(),
    //                 _ordered_by_definition: vec![(
    //                     "malloc".into(),
    //                     Linkage::Import,
    //                     signature!(CallConv::SystemV, &[ir::types::I64], ir::types::I64),
    //                     None,
    //                 )],
    //             },
    //         }
    //     }

    /// Set a specific scalar type representation for some TypeId.
    #[inline]
    pub fn define_scalar_for_type(&mut self, tid: TypeId, cl_ty: ir::types::Type) {
        let _ = self.types.insert(tid, cl_ty);
    }

    /// Get the scalar type representation for some TypeId, or R64 if not defined.
    #[inline]
    pub fn get_scalar_type_of(&self, tid: TypeId) -> ir::Type {
        self.types.get(&tid).cloned().unwrap_or(ir::types::R64)
    }

    /// Include a `hlir::Function` for codegen.
    #[inline]
    pub fn include_function(&mut self, host: &dyn Queries, func: montyc_core::Function) {
        log::trace!(
            "[CodegenModule::include_function] Adding function for codegen: {{ name: {:?}, val: {:?}, module: {:?}, type: {:?} }}",
            host.spanref_to_str(func.name),
            func.value_id,
            func.mref,
            func.type_id,
        );

        let signature = {
            let mut sig = Signature::new(CallConv::SystemV);

            let kind = host.tcx().get_python_type_of(func.type_id).unwrap();

            let (ret, args) =
                patma!((*ret, args.as_ref()), PythonType::Callable { ret, args } in &kind).unwrap();

            if ret != TypingConstants::None {
                let ty = self.get_scalar_type_of(ret);

                sig.returns.push(ir::AbiParam::new(ty));
            }

            for param in args.unwrap_or(&vec![]).iter() {
                let ty = self.get_scalar_type_of(*param);

                sig.params.push(ir::AbiParam::new(ty));
            }

            sig
        };

        let linkage = Linkage::Local;
        let stringy_name = host.get_qualname_of(func.value_id).join("_");

        let code = host.get_function_flatcode(func.value_id).unwrap();

        self.data
            .insert_function(func, code, linkage, signature, &stringy_name)
            .unwrap();
    }

    /// Submit all included functions for codegen and write the output object to the specified `output` argument or a temporary file.
    pub fn finish<P>(
        mut self,
        host: &dyn Queries,
        output: Option<P>,
        isa: Box<dyn TargetIsa>,
        entry: ValueId,
    ) -> io::Result<PathBuf>
    where
        P: AsRef<Path>,
    {
        let object_module = self.build_all_functions(host, isa, entry).map_err(|st| {
            log::error!("{}", st);
            io::ErrorKind::Other
        })?;

        let product = object_module.finish();
        let bytes = product.emit().map_err(|why| {
            log::warn!("{}", why);
            io::ErrorKind::Other
        })?;

        let path = match output {
            Some(path) => {
                std::fs::write(&path, bytes)?;
                path.as_ref().to_owned()
            }

            None => {
                let mut file = NamedTempFile::new()?;

                io::Write::write_all(&mut file, &bytes)?;

                let path = file.path().clone().to_owned();

                file.persist(&path)?;

                path
            }
        };

        Ok(path)
    }
}
