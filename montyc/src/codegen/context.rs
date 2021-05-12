#![allow(warnings)]

use std::alloc;
use std::{cell::RefCell, collections::HashMap, num::NonZeroUsize, path::Path, rc::Rc};

use crate::{
    ast::{
        atom::Atom,
        expr::{Expr, InfixOp},
        primary::Primary,
        stmt::Statement,
    },
    ast::{atom::StringRef, retrn::Return},
    codegen::LowerCodegen,
    context::ModuleRef,
    fmt::Formattable,
    func::Function,
    layout::Block,
    lowering::{Lower, LowerWith},
    prelude::*,
    scope::LookupTarget,
    typing::{LocalTypeId, TypeMap},
};

use cranelift_codegen::{
    binemit,
    ir::{self, ExtFuncData, FuncRef, GlobalValue, GlobalValueData},
    ir::{types, ExternalName, Signature, StackSlot, StackSlotData, StackSlotKind},
    isa::{self, CallConv},
    settings::{self, Configurable},
    verify_function, Context,
};

use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use dashmap::DashMap;

use super::{
    storage::Storage,
    structbuf,
    tvalue::{self, TypePair, TypedValue},
};

#[derive(Debug)]
pub struct ModuleNames {
    pub namespace: u32,
    pub functions: HashMap<NonZeroUsize, (ExternalName, FuncId)>,
}

pub type CodegenLowerArg<'long, 'short, 'fx> = (
    CodegenContext<'long, 'short>,
    &'short mut FunctionBuilder<'fx>,
);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Default)]
pub struct AllocId(usize);

#[derive(Debug, Default)]
pub(super) struct Allocator {
    next_free_slot: AllocId,
    inner: Vec<Storage>,
}

impl Allocator {
    pub fn get(&self, alloc_id: AllocId) -> &Storage {
        /// SAFETY: we're the only one that can produce AllocId's
        ///         and you're not allowed to "shrink" the underlying
        ///         backing storage so all IDs are valid. always.
        unsafe {
            self.inner.get_unchecked(alloc_id.0)
        }
    }

    pub fn get_mut(&mut self, alloc_id: AllocId) -> &mut Storage {
        /// SAFETY: we're the only one that can produce AllocId's
        ///         and you're not allowed to "shrink" the underlying
        ///         backing storage so all IDs are valid. always.
        unsafe {
            self.inner.get_unchecked_mut(alloc_id.0)
        }
    }

    pub fn alloc(&mut self, storage: Storage) -> AllocId {
        let id = self.next_free_slot;

        self.next_free_slot = AllocId(id.0 + 1);

        self.inner.push(storage);

        id
    }
}

type RValueAlloc<'a> =
    dyn Fn(CodegenLowerArg<'_, '_, '_>, &mut Storage) -> Option<tvalue::TypedValue> + 'a;

#[derive(Clone)]
pub struct CodegenContext<'a, 'b>
where
    'a: 'b,
{
    pub codegen_backend: &'b CodegenBackend<'a>,
    pub vars: &'b HashMap<NonZeroUsize, AllocId>,
    pub func: &'b Function,
    pub(super) allocator: Rc<RefCell<Allocator>>,
    pub(super) pending_rvalue: Rc<RefCell<Option<(std::alloc::Layout, Box<RValueAlloc<'a>>)>>>,
}

impl<'a, 'b> CodegenContext<'a, 'b>
where
    'a: 'b,
{
    pub fn size_and_layout_of(
        &self,
        TypePair(hl, _): TypePair,
    ) -> Option<(u32, std::alloc::Layout)> {
        self.codegen_backend
            .global_context
            .type_map
            .size_and_layout(hl)
    }

    pub fn alloca_rvalue(
        &self,
        layout: alloc::Layout,
        allocator: impl for<'long, 'short, 'fx> Fn(
                CodegenLowerArg<'long, 'short, 'fx>,
                &mut Storage,
            ) -> Option<tvalue::TypedValue>
            + 'a,
    ) {
        let alloc = Box::new(allocator) as Box<RValueAlloc>;

        if let Some(_) = self.pending_rvalue.borrow_mut().replace((layout, alloc)) {
            panic!("bug! unhandled rvalue allocation was pending but overwritten!");
        }
    }

    pub fn maybe_coerce(
        &self,
        value: TypedValue,
        into: LocalTypeId,
        fx: &mut FunctionBuilder,
    ) -> TypedValue {
        use ir::InstBuilder;

        let TypePair(from, real) = value.kind;

        if from != into {
            let coerce = self
                .codegen_backend
                .global_context
                .type_map
                .coerce(from, into)
                .unwrap();

            let raw = value.clone().into_raw(fx);

            let refined = (coerce)((self.clone(), fx), raw);
            let refined_t = Some(fx.func.dfg.value_type(refined));

            return TypedValue::by_val(refined, TypePair(into, refined_t));
        }

        if let Some(real) = real {
            let refined_t = self.codegen_backend.scalar_type_of(into);

            if real != refined_t {
                // low-level coercion between CL values
                match (real, refined_t) {
                    (ir::types::B8, ir::types::I64) => {
                        let raw = value.clone().into_raw(fx);
                        let refined = fx.ins().bint(ir::types::I64, raw);

                        return TypedValue::by_val(refined, TypePair(into, Some(refined_t)));
                    }

                    (ir::types::B1, ir::types::B8) => {
                        let raw = value.clone().into_raw(fx);
                        let refined = fx.ins().bextend(ir::types::B8, raw);

                        return TypedValue::by_val(refined, TypePair(into, Some(refined_t)));
                    }

                    _ => todo!("{:?} -> {:?}", real, refined_t),
                }
            }
        }

        value
    }

    pub fn with_var_alloc<T>(&self, n: NonZeroUsize, f: impl FnOnce(&Storage) -> T) -> T {
        let alloc_id = self.vars.get(&n).unwrap();
        let alloc = self.allocator.borrow();

        let storage = alloc.get(*alloc_id);

        f(storage)
    }

    pub fn type_of<T>(&self, obj: &Rc<T>) -> Option<LocalTypeId>
    where
        T: AstObject,
    {
        let mref = self.func.scope.module_ref();
        let mref = Some(&mref);

        self.codegen_backend
            .global_context
            .database
            .type_of(&(Rc::clone(obj) as Rc<_>), mref)
    }

    pub fn get_str_data(&self, stref: StringRef, func: &ir::Function) -> Option<GlobalValue> {
        let data_id = self.codegen_backend.strings.get(&stref)?.as_u32();

        let mut it = func
            .global_values
            .iter()
            .filter_map(|(value, data)| match data {
                GlobalValueData::Symbol {
                    name: ExternalName::User { index, .. },
                    ..
                } => Some((value, *index)),
                _ => None,
            });

        it.find_map(|(gv, id)| (id == data_id).then_some(gv))
    }
}

pub struct CodegenBackend<'a> {
    pub(crate) strings: HashMap<StringRef, DataId>,
    pub(crate) names: HashMap<ModuleRef, ModuleNames>,
    pub(crate) types: HashMap<LocalTypeId, ir::Type>,
    pub(crate) external_functions: HashMap<FuncId, Signature>,

    pub(crate) global_context: &'a GlobalContext,
    pub(crate) object_module: RefCell<ObjectModule>,
    pub(crate) flags: settings::Flags,

    pub(crate) pending: Vec<(usize, Linkage, CallConv)>,
}

impl<'global> CodegenBackend<'global> {
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

    pub(super) fn scalar_type_of(&self, type_id: LocalTypeId) -> ir::Type {
        match self.types.get(&type_id) {
            Some(ty) => *ty,
            None => {
                panic!(
                    "Unable to translate type: {}",
                    Formattable {
                        gctx: self.global_context,
                        inner: type_id
                    }
                )
            }
        }
    }

    fn declare_function(
        &mut self,
        func: &Function,
        mref: &ModuleRef,
        linkage: Linkage,
        callcov: CallConv,
    ) -> Option<(FuncId, ir::Function)> {
        let fn_name = func.name_as_string(self.global_context)?;

        let mut sig = Signature::new(callcov);

        if func.kind.inner.ret != TypeMap::NONE_TYPE {
            sig.returns
                .push(ir::AbiParam::new(self.types[&func.kind.inner.ret]));
        }

        for param in func.kind.inner.args.iter() {
            sig.params.push(ir::AbiParam::new(self.types[param]));
        }

        let func_def_name = func.name(self.global_context);

        let name = if let Some((name, fid)) = self
            .names
            .get(mref)
            .and_then(|mn| mn.functions.get(&func_def_name))
            .cloned()
        {
            return Some((fid, ir::Function::with_name_signature(name, sig)));
        } else {
            self.produce_external_name(func_def_name, mref)
        };

        let clfn = ir::Function::with_name_signature(name.clone(), sig);

        let fid = self
            .object_module
            .borrow_mut()
            .declare_function(&fn_name, linkage, &clfn.signature)
            .ok()?;

        self.names
            .get_mut(mref)
            .unwrap()
            .functions
            .insert(func_def_name, (name.clone(), fid));

        Some((fid, clfn))
    }

    #[allow(warnings)]
    fn build_function(&mut self, fid: FuncId, func: &Function, cl_func: &mut ir::Function) {
        use ir::InstBuilder;

        log::trace!(
            "codegen::build_function {:?} = {}",
            fid,
            Formattable {
                gctx: self.global_context,
                inner: &func.kind.inner
            }
        );

        for dref in func.refs.borrow().iter() {
            if let DataRef::StringConstant(st_ref) = dref {
                if !self.strings.contains_key(st_ref) {
                    let data_id = self
                        .object_module
                        .borrow_mut()
                        .declare_data(&format!("str.{}", st_ref.0), Linkage::Export, false, false)
                        .unwrap();

                    {
                        let mut dctx = DataContext::new();
                        let string = st_ref.resolve_as_string(&self.global_context).unwrap();
                        let string = std::ffi::CString::new(string).unwrap();
                        let string = string.into_bytes_with_nul();
                        let string = string.into_boxed_slice();

                        dctx.define(string);

                        self.object_module
                            .borrow_mut()
                            .define_data(data_id, &mut dctx);
                    }

                    self.strings.insert(st_ref.clone(), data_id);

                    self.object_module
                        .borrow_mut()
                        .declare_data_in_func(data_id, cl_func);
                }
            }
        }

        let func_def = func
            .def(self.global_context)
            .unwrap()
            .unspanned()
            .as_function()
            .cloned()
            .unwrap();

        let layout = func_def.lower_and_then(|_, mut layout| {
            // discard all comment nodes
            for block in layout.blocks.values_mut() {
                let _ = block.nodes.retain(|node| {
                    crate::isinstance!(
                        node.as_ref(),
                        Statement,
                        Statement::Expression(Expr::Primary(p)) => !p.inner.is_comment() // retain discards elements when the predicate produces "false"
                    )
                    .unwrap_or(true)
                });
            }

            layout.reduce_forwarding_edges();

            layout.fold_linear_block_sequences(
                |pred: &Block<Rc<dyn AstObject>>, succ: &Block<Rc<dyn AstObject>>| {
                    fn as_ret(object: &Rc<dyn AstObject>) -> Option<()> {
                        crate::isinstance!(object.as_ref(), Return, _ => ()).or(
                            crate::isinstance!(object.as_ref(), Statement, Statement::Ret(r) => ()),
                        )
                    };

                    pred.nodes.iter().find_map(as_ret).is_none()
                        && succ.nodes.iter().find_map(as_ret).is_none()
                },
            );

            layout
        });

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(cl_func, &mut builder_ctx);

        let mut vars = HashMap::new();

        let ctx = CodegenContext {
            codegen_backend: self,
            allocator: Default::default(),
            pending_rvalue: Default::default(),
            vars: &vars,
            func,
        };

        let mut vars = HashMap::new();

        for var in func.vars.iter() {
            let (var, ty) = (var.key().clone(), (var.0));

            let size = crate::typing::SizeOf::size_of(
                self.global_context.type_map.get(ty).unwrap().value(),
                &self.global_context.type_map,
            )
            .unwrap()
            .get();

            let storage = Storage::new_stack_slot(
                (ctx.clone(), &mut builder),
                TypePair(ty, Some(self.scalar_type_of(ty))),
            );

            let alloc_id = ctx.allocator.borrow_mut().alloc(storage);

            if let Some(n) = vars.insert(var, alloc_id) {
                unreachable!("duplicate var ss: {:?}", (var, n))
            }
        }

        let ctx = CodegenContext {
            codegen_backend: self,
            allocator: ctx.allocator,
            pending_rvalue: ctx.pending_rvalue,
            vars: &vars,
            func: ctx.func,
        };

        let mut implicit_return = true;
        let mut it = layout.iter_from(layout.start);

        match it.next() {
            Some(_) => {
                let start = builder.create_block();

                builder.switch_to_block(start);
                builder.append_block_params_for_function_params(start);

                let params: Vec<_> = builder.block_params(start).iter().cloned().collect();

                for ((name, kind), value) in func.args(&self.global_context).zip(params.iter()) {
                    let alloc_id = vars.get(&name).unwrap().clone();

                    let alloc = ctx.allocator.borrow();

                    let storage = alloc.get(alloc_id);

                    storage.write(
                        TypedValue::by_val(*value, TypePair(kind, Some(self.scalar_type_of(kind)))),
                        &mut builder,
                    );
                }
            }

            None => unreachable!(),
        }

        for (bid, block) in it {
            assert!(block.succs.len() <= 1);

            if bid == layout.end {
                break;
            }

            {
                let block = builder.create_block();

                if !builder.is_filled() {
                    builder.ins().jump(block, &[]);
                }

                builder.switch_to_block(block);
            }

            for node in block.nodes.iter() {
                if let Some(stmt) = crate::isinstance!(node.as_ref(), Statement) {
                    let _ = stmt.lower((ctx.clone(), &mut builder));
                } else {
                    unreachable!();
                }
            }
        }

        // if implicit_return {
        //     builder.ins().return_(&[]);
        // }
    }

    pub fn declare_functions<'a>(
        &mut self,
        it: impl Iterator<Item = (usize, &'a Function, ModuleRef, Linkage, CallConv)>,
    ) {
        for (idx, func, mref, linkage, callcov) in it {
            self.pending.push((idx, linkage.clone(), callcov.clone()));
            self.declare_function(func, &mref, linkage, callcov);
        }
    }

    pub fn add_function_to_module(
        &mut self,
        func: &Function,
        mref: &ModuleRef,
        linkage: Linkage,
        callcov: CallConv,
    ) {
        let (fid, mut cl_func) = self.declare_function(func, mref, linkage, callcov).unwrap();

        if func.is_externaly_defined() {
            self.external_functions
                .insert(fid, cl_func.signature.clone());

            return;
        }

        self.build_function(fid, func, &mut cl_func);

        if let Err(e) = verify_function(&cl_func, &self.flags) {
            log::trace!("codegen:add_function_to_module {:?} {:?}", cl_func, e);
        }

        let mut ctx = Context::for_function(cl_func);
        let mut ts = binemit::NullTrapSink {};
        let mut ss = binemit::NullStackMapSink {};

        self.object_module
            .borrow_mut()
            .define_function(fid, &mut ctx, &mut ts, &mut ss)
            .unwrap();
    }

    pub fn new(
        global_context: &'global GlobalContext,
        isa: Option<target_lexicon::Triple>,
    ) -> Self {
        let mut flags_builder = settings::builder();

        // use debug assertions
        flags_builder
            .enable("enable_verifier")
            .expect("enable_verifier should be a valid option");

        // minimal optimizations
        flags_builder
            .set("opt_level", "speed")
            .expect("opt_level: speed should be a valid option");

        let flags = settings::Flags::new(flags_builder);

        let target_isa = isa::lookup(isa.unwrap_or_else(target_lexicon::Triple::host))
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
            types.insert(TypeMap::INTEGER, ir::types::I64);
            types.insert(TypeMap::BOOL, ir::types::B1);
            types.insert(
                global_context.type_map.entry(TypeDescriptor::Generic(
                    crate::typing::Generic::Pointer { inner: TypeMap::U8 },
                )),
                ir::types::I64,
            );

            types.insert(TypeMap::STRING, ir::types::I64);
        }

        Self {
            global_context,
            object_module: RefCell::new(object_module),
            flags,

            pending: vec![],

            types,
            names: HashMap::new(),
            external_functions: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    pub fn finish<P>(mut self, output: P)
    where
        P: AsRef<Path>,
    {
        let pending = self.pending.clone();

        for (idx, linkage, callcov) in pending.iter().cloned() {
            let funcs = self.global_context.functions.borrow();
            let func = funcs.get(idx).unwrap();
            let mref = func.scope.module_ref();

            self.add_function_to_module(func, &mref, linkage, callcov)
        }

        let product = self.object_module.into_inner().finish();
        let bytes = product.emit().unwrap();

        let mut file = tempfile::NamedTempFile::new().unwrap();

        std::io::Write::write_all(&mut file, &bytes).unwrap();

        let path = file.path().clone().to_owned();

        file.persist(path.clone()).unwrap();

        let mut cc_args = vec![path.to_str().unwrap()];

        let output = output.as_ref().to_str().unwrap();

        cc_args.push("-o");
        cc_args.push(&output);

        let status = std::process::Command::new("cc")
            .args(&cc_args)
            .status()
            .unwrap();

        if !status.success() {
            panic!("Failed to compile module.");
        }
    }
}
