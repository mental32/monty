#![allow(warnings)]

use std::{cell::RefCell, collections::HashMap, num::NonZeroUsize, path::Path, rc::Rc};

use crate::prelude::*;
use crate::{
    ast::{
        atom::Atom,
        expr::{Expr, InfixOp},
        primary::Primary,
        stmt::Statement,
    },
    context::ModuleRef,
    func::Function,
    lowering::{Lower, LowerWith},
    scope::LookupTarget,
    typing::{LocalTypeId, TypeMap},
};

use codegen::isa::CallConv;
use cranelift_codegen::{
    self as codegen,
    ir::{types, ExternalName, Signature, StackSlot, StackSlotData, StackSlotKind},
    settings::{self, Configurable},
    verify_function, Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use dashmap::DashMap;

#[derive(Debug)]
pub struct ModuleNames {
    pub namespace: u32,
    pub functions: HashMap<NonZeroUsize, (ExternalName, FuncId)>,
}

pub type CodegenLowerArg<'a, 'b> = CodegenContext<'a, 'b>;

#[derive(Clone)]
pub struct CodegenContext<'a, 'b>
where
    'a: 'b,
{
    pub codegen_backend: &'b CodegenBackend,
    pub builder: Rc<RefCell<FunctionBuilder<'a>>>,
    pub vars: &'a DashMap<NonZeroUsize, StackSlot>,
    pub func: &'b Function,
}

pub struct CodegenBackend {
    // functions: Vec<(FuncId, codegen::ir::Function)>,
    pub(crate) object_module: ObjectModule,
    pub(crate) flags: settings::Flags,
    pub(crate) names: HashMap<ModuleRef, ModuleNames>,
    pub(crate) types: HashMap<LocalTypeId, codegen::ir::Type>,
}

impl CodegenBackend {
    fn produce_external_name(&mut self, fn_name: NonZeroUsize, mref: &ModuleRef) -> ExternalName {
        let n = self.names.len();
        let names = self
            .names
            .entry(mref.clone())
            .or_insert_with(|| ModuleNames {
                namespace: n as u32,
                functions: HashMap::default(),
            });

        let external_name = ExternalName::User {
            namespace: names.namespace,
            index: names.functions.len() as u32,
        };

        external_name
    }

    fn declare_function(
        &mut self,
        func: &Function,
        mref: &ModuleRef,
        linkage: Linkage,
        callcov: codegen::isa::CallConv,
    ) -> Option<(FuncId, codegen::ir::Function)> {
        let fn_name = func.name_as_string()?;

        let mut sig = Signature::new(callcov);

        if func.kind.inner.ret != TypeMap::NONE_TYPE {
            sig.returns
                .push(codegen::ir::AbiParam::new(self.types[&func.kind.inner.ret]));
        }

        for param in func.kind.inner.args.iter() {
            sig.params
                .push(codegen::ir::AbiParam::new(self.types[param]));
        }

        let name = if let Some((name, fid)) = self
            .names
            .get(mref)
            .and_then(|mn| mn.functions.get(&func.def.name().clone().unwrap()))
            .cloned()
        {
            return Some((fid, codegen::ir::Function::with_name_signature(name, sig)));
        } else {
            self.produce_external_name(func.def.name().unwrap(), mref)
        };

        let clfn = codegen::ir::Function::with_name_signature(name.clone(), sig);

        let fid = self
            .object_module
            .declare_function(&fn_name, linkage, &clfn.signature)
            .ok()?;

        self.names
            .get_mut(mref)
            .unwrap()
            .functions
            .insert(func.def.name().unwrap(), (name.clone(), fid));

        Some((fid, clfn))
    }

    #[allow(warnings)]
    fn build_function(&self, fid: FuncId, func: &Function, cl_func: &mut codegen::ir::Function) {
        use cranelift_codegen::ir::InstBuilder;

        log::trace!("codegen::build_function {:?} = {}", fid, func.kind.inner);

        let layout = func.lower_and_then(|_, mut layout| {
            layout.reduce_forwarding_edges();
            layout.fold_linear_block_sequences();
            layout
        });

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(cl_func, &mut builder_ctx);

        let mut vars = dashmap::DashMap::new();

        for var in func.vars.iter() {
            let (var, ty) = (var.key().clone().unwrap(), (var.0));

            let size = func.kind.inner.resolver.type_map.size_of(ty).unwrap();

            let data = StackSlotData::new(StackSlotKind::ExplicitSlot, size as u32);
            let ss = builder.create_stack_slot(data);

            vars.insert(var, ss);
        }

        let mut implicit_return = true;
        let mut it = layout.iter_from(layout.start);

        match it.next() {
            Some(_) => {
                let start = builder.create_block();

                builder.switch_to_block(start);
                builder.append_block_params_for_function_params(start);

                let params: Vec<_> = builder.block_params(start).iter().cloned().collect();

                for ((n, ty), value) in func.vars.iter().map(|r| (r.key().clone(), r.value().0)).zip(params.into_iter()) {
                    let ss = vars.get(&n.unwrap()).unwrap().value().clone();
                    builder.ins().stack_store(value, ss, 0);
                }

            }

            None => unreachable!(),
        }

        let builder = Rc::new(RefCell::new(builder));

        for (bid, block) in it {
            assert!(block.succs.len() <= 1);

            if bid == layout.end {
                break;
            }

            {
                let mut builder = builder.borrow_mut();

                let block = builder.create_block();
                builder.ins().jump(block, &[]);
                builder.switch_to_block(block);
            }

            for node in block.nodes.iter() {
                if let Some(stmt) = crate::isinstance!(node.as_ref(), Statement) {
                    let ret = stmt.lower_with(CodegenContext {
                        codegen_backend: self,
                        builder: Rc::clone(&builder),
                        vars: &vars,
                        func,
                    });

                    if implicit_return && ret.is_some() {
                        implicit_return = ret.unwrap();
                    }

                } else {
                    unreachable!();
                }
            }
        }

        if implicit_return {
            builder.borrow_mut().ins().return_(&[]);
        }
    }

    pub fn declare_functions<'a>(
        &mut self,
        it: impl Iterator<Item = (&'a Function, &'a ModuleRef, Linkage, CallConv)>,
    ) {
        for (func, mref, linkage, callcov) in it {
            self.declare_function(func, mref, linkage, callcov);
        }
    }

    pub fn add_function_to_module(
        &mut self,
        func: &Function,
        mref: &ModuleRef,
        linkage: Linkage,
        callcov: codegen::isa::CallConv,
    ) {
        let (fid, mut cl_func) = self.declare_function(func, mref, linkage, callcov).unwrap();

        self.build_function(fid, func, &mut cl_func);

        verify_function(&cl_func, &self.flags).unwrap();

        let mut ctx = Context::for_function(cl_func);
        let mut ts = codegen::binemit::NullTrapSink {};
        let mut ss = codegen::binemit::NullStackMapSink {};

        self.object_module
            .define_function(fid, &mut ctx, &mut ts, &mut ss)
            .unwrap();
    }

    pub fn new(isa: Option<target_lexicon::Triple>) -> Self {
        let mut flags_builder = settings::builder();

        // allow creating shared libraries
        flags_builder
            .enable("is_pic")
            .expect("is_pic should be a valid option");

        // use debug assertions
        flags_builder
            .enable("enable_verifier")
            .expect("enable_verifier should be a valid option");

        // minimal optimizations
        flags_builder
            .set("opt_level", "speed")
            .expect("opt_level: speed should be a valid option");

        let flags = settings::Flags::new(flags_builder);

        let target_isa = codegen::isa::lookup(isa.unwrap_or_else(target_lexicon::Triple::host))
            .unwrap()
            .finish(settings::Flags::new(settings::builder()));

        let object_builder = ObjectBuilder::new(
            target_isa,
            "<empty>".to_string(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();

        let object_module = ObjectModule::new(object_builder);
        let mut types = HashMap::new();

        {
            types.insert(TypeMap::INTEGER, codegen::ir::types::I64);
            types.insert(TypeMap::BOOL, codegen::ir::types::I64);
        }

        Self {
            object_module,
            types,
            flags,
            names: HashMap::default(),
        }
    }

    pub fn finish<P>(self, output: Option<P>)
    where
        P: AsRef<Path>,
    {
        let product = self.object_module.finish();
        let bytes = product.emit().unwrap();

        let mut file = tempfile::NamedTempFile::new().unwrap();

        std::io::Write::write_all(&mut file, &bytes).unwrap();

        let path = file.path().clone().to_owned();

        file.persist(path.clone()).unwrap();

        let mut cc_args = vec![path.to_str().unwrap()];

        let output: std::path::PathBuf = if let Some(path) = output {
            path.as_ref().to_path_buf()
        } else {
            "a.out".into()
        };

        cc_args.push("-o");
        cc_args.push(&output.to_str().unwrap());

        std::process::Command::new("cc")
            .args(&cc_args)
            .status()
            .unwrap();
    }
}
