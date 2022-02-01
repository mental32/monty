use std::path::PathBuf;

use cranelift_codegen::ir::{self, ExternalName, InstBuilder, Signature};
use cranelift_codegen::isa::{self, CallConv, TargetIsa};
use cranelift_codegen::settings::{self, Configurable, Flags};
use cranelift_codegen::{binemit, Context};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use montyc_core::ast::Constant;
use tempfile::NamedTempFile;

use montyc_core::codegen::{CgBlockId, CgInst};
use montyc_core::opts::CompilerOptions;
use montyc_core::{
    BuiltinType, MapT, MontyResult, PythonType, TaggedValueId, TypeId, ValueId, FUNCTION,
};
use montyc_query::Queries;

#[macro_use]
pub(crate) mod macros {
    macro_rules! signature {
        ($cc:expr, $args:expr, $ret:expr) => {{
            let args: &[::cranelift_codegen::ir::Type] = $args;
            let ret: ::cranelift_codegen::ir::Type = $ret;

            let mut sig = ::cranelift_codegen::ir::Signature::new($cc);

            sig.returns
                .push(::cranelift_codegen::ir::AbiParam::new(ret));

            for arg in args {
                sig.params
                    .push(::cranelift_codegen::ir::AbiParam::new(arg.clone()));
            }

            sig
        }};
    }
}

pub(crate) mod builder;
pub(crate) mod builtins;
pub(crate) mod data;

use data::CgFuncData;

fn ir_type_of(tcx: &dyn montyc_core::TypingContext, type_id: TypeId) -> ir::Type {
    match tcx.get_python_type_of(type_id).unwrap() {
        montyc_core::PythonType::Builtin { inner } => match inner {
            BuiltinType::Bytes => ir::types::I64,
            BuiltinType::U8 => ir::types::I8,
            BuiltinType::U16 => ir::types::I16,
            BuiltinType::U32 => ir::types::I32,
            BuiltinType::U64 => ir::types::I64,
            BuiltinType::I8 => ir::types::I8,
            BuiltinType::I16 => ir::types::I16,
            BuiltinType::I32 => ir::types::I32,
            BuiltinType::I64 => ir::types::I64,
            _ => ir::types::I64,
        },

        _ => ir::types::I64,
    }
}

pub(crate) struct BackendImpl {
    /// cranelift settings.
    settings: settings::Flags,

    /// Codegen data like functions and static data.
    data: data::CgData,

    /// The path to the C compiler used to link the objects together.
    cc: PathBuf,

    /// The path to where we should write the output to.
    output: PathBuf,
}

impl core::fmt::Debug for BackendImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BackendImpl").finish()
    }
}

impl BackendImpl {
    fn codegen_flags(opts: &CompilerOptions) -> settings::Flags {
        let settings = match opts {
            CompilerOptions::Build {
                cranelift_settings, ..
            } => cranelift_settings,

            _ => unreachable!("cant generate codegen settings for a non-build option."),
        };

        let mut flags_builder = cranelift_codegen::settings::builder();

        let default_settings = ["opt_level=none", "is_pic=no", "enable_verifier=yes"]
            .iter()
            .map(ToString::to_string);

        for setting in default_settings.chain(settings.iter().cloned()) {
            let mut it = setting.split("=").take(2);

            let (name, value) = (it.next().unwrap(), it.next().unwrap());

            flags_builder.set(name, value).unwrap();
        }

        settings::Flags::new(flags_builder)
    }

    fn codegen_settings(&self) -> Box<dyn TargetIsa> {
        isa::lookup(target_lexicon::Triple::host())
            .unwrap()
            .finish(self.settings.clone())
    }

    fn define_main(
        &self,
        entry: (ExternalName, Signature),
        fisa: &Flags,
        object_module: &mut ObjectModule,
    ) {
        let mut main_fn = ir::Function::with_name_signature(
            ExternalName::User {
                namespace: 0,
                index: self.data.funcs_ordered().count() as u32 + 1,
            },
            signature!(CallConv::SystemV, &[], ir::types::I64),
        );

        let (entry_name, entry_sig) = entry;

        let fid = object_module
            .declare_function(&"main", Linkage::Export, &entry_sig)
            .unwrap();

        {
            let mut cx = FunctionBuilderContext::new();
            let mut fx = cranelift_frontend::FunctionBuilder::new(&mut main_fn, &mut cx);

            let data = ir::ExtFuncData {
                name: entry_name.clone(),
                signature: fx.import_signature(entry_sig.clone()),
                colocated: true,
            };

            let entry_ref = fx.import_function(data);

            let start = fx.create_block();

            fx.switch_to_block(start);

            let ret = fx.ins().call(entry_ref, &[]);
            let ret = fx.inst_results(ret)[0];
            fx.ins().return_(&[ret]);

            fx.seal_all_blocks();
        }

        if let Err(err) = cranelift_codegen::verify_function(&main_fn, fisa) {
            panic!("{}", err);
        }

        let mut ctx = Context::for_function(main_fn);
        let mut ts = binemit::NullTrapSink {};
        let mut ss = binemit::NullStackMapSink {};

        object_module
            .define_function(fid, &mut ctx, &mut ts, &mut ss)
            .unwrap();
    }

    fn define_builtins(
        &self,
        stubbed: &MapT<ValueId, &CgFuncData>,
        queries: &dyn Queries,
        object_module: &mut ObjectModule,
        fisa: &Flags,
        fids: &MapT<ValueId, cranelift_module::FuncId>,
    ) -> MontyResult<()> {
        let tcx = queries.tcx();
        let bltns = builtins::create_builtins(self, object_module);

        for (ix, f) in stubbed.iter() {
            let fid = fids[ix];
            let func_t = queries.get_type_of(*ix).unwrap();

            if let PythonType::Callable { ret, .. } = tcx.get_python_type_of(func_t).unwrap() {
                let name = queries.spanref_to_str(queries.get_function(*ix)?.name)?;
                let key = (ret, name);

                let tcx = queries.tcx();

                let mut func = match bltns.get(&key) {
                    Some(func) => func.clone(),
                    None => {
                        log::error!(
                            "Missing builtin function for type {:?} method {}",
                            tcx.display_type(ret, &|v| queries.get_type_of(v).ok())
                                .unwrap(),
                            name
                        );

                        continue;
                    }
                };

                func.name = f.name.clone();

                if let Err(err) = cranelift_codegen::verify_function(&func, fisa) {
                    let err =
                        cranelift_codegen::print_errors::pretty_verifier_error(&func, None, err);

                    todo!("{:#?}", err);
                }

                let mut ctx = Context::for_function(func.clone());
                let mut ts = binemit::NullTrapSink {};
                let mut ss = binemit::NullStackMapSink {};

                log::info!(
                    "Defining {:?} {} -> {}",
                    tcx.get_python_type_of(func_t),
                    name,
                    fid
                );

                object_module
                    .define_function(fid, &mut ctx, &mut ts, &mut ss)
                    .unwrap();
            } else {
                unimplemented!()
            }
        }

        Ok(())
    }

    fn build_func<'a>(
        &self,
        queries: &dyn Queries,
        object_module: &mut ObjectModule,
        builder_cx: &'a mut FunctionBuilderContext,
        fids: &MapT<ValueId, FuncId>,
        func: &CgFuncData,
        fisa: &Flags,
    ) -> cranelift_module::ModuleResult<&'a mut FunctionBuilderContext> {
        log::info!("[build_func] building func {:?}", func.value_id);
        log::trace!("    with cfg: {:#?}", func.cfg.raw_nodes());

        let CgFuncData { name, sig, cfg, .. } = func;

        let mut clir = ir::Function::with_name_signature(name.clone(), sig.clone());
        let fid = match fids.get(&func.value_id.0) {
            Some(fid) => fid,
            None => panic!("Missing FuncId for a Func ValueIx {:?}", func.value_id),
        };

        {
            let mut fx = builder::Builder {
                inner: FunctionBuilder::new(&mut clir, builder_cx),
                values: MapT::with_capacity(cfg.raw_nodes().iter().map(|n| n.weight.len()).sum()),
                locals: MapT::new(),

                f_refs: MapT::new(),
                host: queries,
                cfg,
            };

            // Allocate the blocks to be used:
            let start = fx.create_block();
            let blocks = cfg
                .raw_nodes()
                .iter()
                .enumerate()
                .map(|(ix, _)| (CgBlockId(ix), fx.create_block()))
                .collect::<MapT<_, _>>();

            // Set the parameters for the starting block and switch to it to load in arguments.
            fx.append_block_params_for_function_params(start);
            fx.switch_to_block(start);

            let func_params = match fx
                .host
                .tcx()
                .get_python_type_of(fx.host.get_type_of(func.value_id.0).unwrap())
                .unwrap()
            {
                PythonType::Callable { params, .. } => params,
                _ => unreachable!(),
            };

            if let Some(params) = func_params {
                let entry_block_params = fx.block_params(start).to_owned().into_boxed_slice();

                let param_names = match fx
                    .host
                    .get_function_flatcode(func.value_id)
                    .unwrap()
                    .ast
                    .unwrap()
                {
                    montyc_parser::AstNode::FuncDef(f) => f.args.unwrap_or_default(),
                    _ => unreachable!(),
                };

                for (((param_name, _), param_t), param_v) in param_names
                    .iter()
                    .zip(params.iter())
                    .zip(entry_block_params.iter())
                {
                    let param_ss = fx.stack_alloc(*param_t);

                    ir::InstBuilder::stack_store(fx.inner.ins(), *param_v, param_ss, 0);

                    fx.locals.insert(
                        param_name.group(),
                        (param_ss, *param_t, ir_type_of(fx.host.tcx(), *param_t)),
                    );
                }
            }

            fx.ins().jump(blocks[&CgBlockId(0)], &[]);
            fx.seal_block(start);

            // Now finish lowering the rest of the code.
            fx.lower(blocks, object_module, fids, self.codegen_settings());
        }

        if let Err(err) = cranelift_codegen::verify_function(&clir, fisa) {
            let err = cranelift_codegen::print_errors::pretty_verifier_error(&clir, None, err);

            panic!("{}", err);
        }

        log::info!("Defining user function {:?} -> {}", func.value_id, fid);
        log::trace!("{:?}", clir);

        let mut ctx = Context::for_function(clir);
        let mut ts = binemit::NullTrapSink {};
        let mut ss = binemit::NullStackMapSink {};

        object_module.define_function(*fid, &mut ctx, &mut ts, &mut ss)?;

        Ok(builder_cx)
    }

    fn build_all_functions(
        &self,
        queries: &dyn Queries,
        entry_ix: ValueId,
    ) -> MontyResult<ObjectModule> {
        let fisa = self.codegen_settings().flags().clone();

        let mut object_module = ObjectModule::new({
            let name = String::from("<empty>");
            let libcall_names = cranelift_module::default_libcall_names();

            ObjectBuilder::new(self.codegen_settings(), name, libcall_names).unwrap()
        });

        fn is_stubbed(func: &&CgFuncData) -> bool {
            match func.cfg.raw_nodes() {
                [] => true,

                [block] => matches!(
                    block.weight.as_slice(),
                    [CgInst::Const {
                        cst: Constant::Ellipsis,
                        ..
                    }]
                ),

                _ => false,
            }
        }

        let (stubbed, defined) = self
            .data
            .iter_func_data()
            .map(|(ix, func)| (*ix, func))
            .partition::<MapT<_, _>, _>(|(_, func)| is_stubbed(func));

        log::trace!("{:#?}", stubbed);

        let fids = {
            let mut fids = MapT::new();

            for (f_name, f_linkage, signature, ix) in self.data.funcs_ordered() {
                let fid = object_module
                    .declare_function(&f_name, f_linkage.clone(), &signature)
                    .unwrap();

                log::info!("Declaring function {:?} -> {}", f_name, fid);

                if let Some(ix) = ix {
                    fids.insert(*ix, fid);
                }
            }

            fids
        };

        let _ = self.define_builtins(&stubbed, queries, &mut object_module, &fisa, &fids)?;

        let entry = {
            let CgFuncData { name, sig, .. } = defined[&entry_ix];
            (name.clone(), sig.clone())
        };

        let _ = self.define_main(entry, &fisa, &mut object_module);

        let mut builder_cx = FunctionBuilderContext::new();

        for (_, func) in defined {
            if let Err(err) = self.build_func(
                queries,
                &mut object_module,
                &mut builder_cx,
                &fids,
                func,
                &fisa,
            ) {
                todo!("{:?}", err);
            }
        }

        Ok(object_module)
    }
}

impl BackendImpl {
    pub fn new(opts: &CompilerOptions) -> Self {
        // generate settings from provided options.
        let settings = Self::codegen_flags(opts);

        let cc = match opts {
            CompilerOptions::Build { cc, .. } => cc.clone(),
            _ => None,
        }
        .unwrap_or_else(|| PathBuf::from("cc"));

        let output = match opts {
            CompilerOptions::Build { output, .. } => output.clone(),
            _ => unreachable!(),
        };

        Self {
            settings,
            output,

            cc,
            data: data::CgData::default(),
        }
    }

    pub fn include_function(
        &mut self,
        queries: &dyn Queries,
        value_id: TaggedValueId<{ FUNCTION }>,
    ) -> MontyResult<()> {
        let func = queries.get_function(value_id.0)?;

        log::trace!(
            "[cranelift::BackendImpl::include_function] adding function {:?} from module {:?} with type {}",
            func.value_id,
            func.mref,
            queries.tcx().display_type(func.type_id, &|v| queries.get_type_of(v).ok()).unwrap_or_else(|| "<error>".into()),
        );

        let cg_cfg = queries.get_function_cg_cfg(value_id)?;
        let tcx = queries.tcx();

        let signature = match tcx.get_python_type_of(queries.get_type_of(value_id.0)?) {
            Some(kind) => match kind {
                montyc_core::PythonType::Callable { params, ret } => {
                    let ret = ir_type_of(tcx, ret);

                    let params = params
                        .unwrap_or_default()
                        .iter()
                        .map(|t| ir_type_of(tcx, *t))
                        .collect::<Vec<_>>();

                    signature!(CallConv::SystemV, params.as_slice(), ret)
                }

                _ => panic!("..."),
            },

            None => unreachable!(),
        };

        self.data.insert_function(value_id, cg_cfg, signature);

        Ok(())
    }

    pub fn finish(mut self, queries: &dyn Queries) -> MontyResult<PathBuf> {
        let entry_path = queries
            .entry_path()
            .expect("when building an entry path should always be present.");

        let (mut funcs, entry) = queries.call_graph_of(&entry_path)?;

        for func in funcs.drain(..) {
            self.include_function(queries, func)?;
        }

        let object_module = self
            .build_all_functions(queries, entry)
            .map_err(|st| {
                log::error!("{}", st);
                std::io::ErrorKind::Other
            })
            .unwrap();

        let product = object_module.finish();

        let bytes = product
            .emit()
            .map_err(|why| {
                log::warn!("{}", why);
                std::io::ErrorKind::Other
            })
            .unwrap();

        let path = {
            let mut file = NamedTempFile::new()?;

            std::io::Write::write_all(&mut file, &bytes).unwrap();

            let path = file.path().clone().to_owned();

            file.persist(&path).unwrap();

            path
        };

        let object_path = &*path.to_string_lossy();

        log::info!("written object file to {:?}", object_path);

        std::process::Command::new(self.cc.clone())
            .args(&[object_path, "-o", self.output.to_str().unwrap(), "-no-pie"])
            .status()
            .map(|_| ())?;

        Ok(path)
    }
}
