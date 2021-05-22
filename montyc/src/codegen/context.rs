use std::{cell::RefCell, collections::HashMap, num::NonZeroUsize, rc::Rc};

use cranelift_codegen::ir::{self, ExternalName, GlobalValue, GlobalValueData};
use cranelift_frontend::FunctionBuilder;

use crate::{
    ast::atom::StringRef,
    codegen::pointer::Pointer,
    fmt::Formattable,
    prelude::{AstObject, Function, Generic, LocalTypeId},
    scope::Scope,
    ssamap::SSAMap,
};

use super::{
    module::CodegenModule,
    storage::Storage,
    tvalue::{self, TypePair, TypedValue},
};

pub type CodegenLowerArg<'long, 'short, 'fx> = (
    CodegenContext<'long, 'short>,
    &'short mut FunctionBuilder<'fx>,
);

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Copy,
    Hash,
    Default,
    derive_more::From,
    derive_more::Into,
)]
pub struct AllocId(usize);

#[repr(transparent)]
pub struct Allocator(SSAMap<AllocId, Rc<Storage>>);

impl Default for Allocator {
    fn default() -> Self {
        Self(SSAMap::new())
    }
}

impl Allocator {
    pub fn get(&self, AllocId(id): AllocId) -> Rc<Storage> {
        let storage = self
            .0
            .get_raw(id)
            .expect("Attempted to access unknown allocation.");

        Rc::clone(storage)
    }

    pub fn alloc(&mut self, storage: Storage) -> AllocId {
        self.0.insert(Rc::new(storage))
    }
}

pub(super) type RValueAlloc<'a> =
    dyn Fn(CodegenLowerArg<'_, '_, '_>, &Storage) -> Option<tvalue::TypedValue> + 'a;

#[derive(Clone)]
pub struct CodegenContext<'global, 'codegen>
where
    'global: 'codegen,
{
    pub codegen_backend: &'codegen CodegenModule<'global>,
    pub vars: &'codegen HashMap<NonZeroUsize, AllocId>,
    pub func: &'codegen Function,
    allocator: Rc<RefCell<Allocator>>,
}

impl<'a, 'b> CodegenContext<'a, 'b>
where
    'a: 'b,
{
    pub fn new(
        module: &'b CodegenModule<'a>,
        vars: &'b HashMap<NonZeroUsize, AllocId>,
        func: &'b Function,
    ) -> Self {
        Self {
            codegen_backend: module,
            vars,
            func,
            allocator: Default::default(),
        }
    }

    pub fn alloc(&self, storage: Storage) -> AllocId {
        self.allocator.borrow_mut().alloc(storage)
    }

    pub fn with_alloc<T>(&self, alloc_id: AllocId, f: impl FnOnce(&Storage) -> T) -> T {
        let s = self.allocator.borrow().get(alloc_id);
        f(&*s)
    }

    pub fn size_and_layout_of(
        &self,
        TypePair(hl, _): TypePair,
    ) -> Option<(u32, std::alloc::Layout)> {
        self.codegen_backend
            .global_context
            .type_map
            .size_and_layout(hl)
    }

    pub fn maybe_coerce(
        &self,
        value: TypedValue,
        into: LocalTypeId,
        fx: &mut FunctionBuilder,
    ) -> TypedValue {
        use ir::InstBuilder;

        let TypePair(from, real) = value.kind;

        let from_ = Formattable {
            inner: from,
            gctx: &self.codegen_backend.global_context,
        };

        let into_ = Formattable {
            inner: into,
            gctx: &self.codegen_backend.global_context,
        };

        log::trace!("codegen:maybe_coerce {} -> {}", from_, into_);

        let tym = &self.codegen_backend.global_context.type_map;

        if from != into {
            let coerce = match self
                .codegen_backend
                .global_context
                .type_map
                .coerce(from, into)
            {
                Some(f) => f,

                None if tym.is_pointer(from) && tym.points_to(from, into) => {
                    let inner = value.deref_into_raw(fx);
                    let ty = fx.func.dfg.value_type(inner);

                    return TypedValue::by_val(inner, TypePair(into, Some(ty)));
                }

                None if tym.is_pointer(from) => {
                    let from = match tym.get_tagged::<Generic>(from).unwrap().unwrap().inner {
                        Generic::Pointer { inner } => inner,
                        _ => unreachable!(),
                    };

                    if tym.is_variant_of_tagged_union(into, from) {
                        let ptr = value.deref_into_raw(fx);
                        let sbuf = Pointer::new(ptr).as_mut_struct((self.clone(), fx), from);

                        let raw = sbuf.read(1, (self.clone(), fx)).unwrap();
                        let ty = fx.func.dfg.value_type(raw);

                        return TypedValue::by_val(raw, TypePair(into, Some(ty)));
                    } else {
                        panic!(
                            "No suitable coercion rule found for {} -> {}",
                            from_, into_
                        )
                    }
                }

                None if tym.is_variant_of_tagged_union(into, from) => {
                    let sbuf = value.as_ptr().as_mut_struct((self.clone(), fx), from);

                    let raw = sbuf.read(1, (self.clone(), fx)).unwrap();
                    let ty = fx.func.dfg.value_type(raw);

                    return TypedValue::by_val(raw, TypePair(into, Some(ty)));
                }

                None => panic!(
                    "No suitable coercion rule found for {} -> {}",
                    from_, into_
                ),
            };

            return (coerce)((self.clone(), fx), value);
        } else if let Some(real) = real {
            let refined_t = self.codegen_backend.scalar_type_of(into);

            if real != refined_t {
                // low-level coercion between CL values
                match (real, refined_t) {
                    (ir::types::B8, ir::types::I64) => {
                        let raw = value.clone().into_raw(fx);
                        let refined = fx.ins().bint(ir::types::I64, raw);

                        return TypedValue::by_val(refined, TypePair(into, Some(refined_t)));
                    }

                    (ir::types::B1, ir::types::I64) => {
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

        f(&*storage)
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

        func.global_values
            .iter()
            .find_map(|(value, data)| match data {
                GlobalValueData::Symbol {
                    name: ExternalName::User { index, .. },
                    ..
                } if *index == data_id => Some(value),
                _ => None,
            })
    }
}
