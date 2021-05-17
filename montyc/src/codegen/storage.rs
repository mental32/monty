use std::{alloc::Layout, cell::RefCell};

use cranelift_codegen::{
    ir::{self, AbiParam, InstBuilder, Signature, StackSlotData, StackSlotKind},
    isa::CallConv,
};
use cranelift_frontend::FunctionBuilder;

use crate::prelude::LocalTypeId;

use super::{context::CodegenLowerArg, pointer::Pointer, tvalue::TypedValue};
use super::{structbuf::StructBuf, tvalue::TypePair};

#[derive(Debug, Clone)]
enum StorageKind {
    Addr(Pointer),
    Boxed(Option<Pointer>),
}

#[derive(Debug)]
pub struct Storage {
    inner: RefCell<StorageKind>,
    kind: TypePair,
}

impl Storage {
    pub fn new_boxed((_, _): CodegenLowerArg<'_, '_, '_>, kind: LocalTypeId) -> Self {
        Self {
            inner: RefCell::new(StorageKind::Boxed(None)),
            kind: TypePair(kind, None),
        }
    }

    pub fn new_stack_slot((ctx, builder): CodegenLowerArg<'_, '_, '_>, kind: TypePair) -> Self {
        let (size, _) = ctx
            .size_and_layout_of(kind.clone())
            .expect("Type must not be zero-sized!");

        // FIXME: Don't force the size to a multiple of 16 bytes once
        //        Cranelift gets a way to specify stack slot alignment.
        let slot_size = (size + 15) / 16 * 16;

        let stack_slot =
            builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size));

        Self {
            inner: RefCell::new(StorageKind::Addr(Pointer::stack_slot(stack_slot))),
            kind,
        }
    }

    pub fn kind(&self) -> TypePair {
        self.kind.clone()
    }

    pub fn write(&self, tvalue: TypedValue, fx: &mut FunctionBuilder) {
        let ptr = if let StorageKind::Boxed(None) = &*self.inner.borrow() {
            todo!()
        } else {
            self.as_ptr()
        };

        let value = tvalue.into_raw(fx);

        ptr.store(value, 0, fx);
    }

    pub fn as_ptr_value(&self) -> TypedValue {
        let ptr = self.as_ptr();

        TypedValue::by_ref(ptr, self.kind.clone())
    }

    pub fn as_ptr(&self) -> Pointer {
        match &*self.inner.borrow() {
            StorageKind::Addr(ptr) => ptr.clone(),
            StorageKind::Boxed(Some(ptr)) => ptr.clone(),
            StorageKind::Boxed(None) => unreachable!(),
        }
    }

    pub fn as_mut_struct(&self, (ctx, fx): CodegenLowerArg<'_, '_, '_>) -> StructBuf {
        let TypePair(type_id, scalar_ty) = self.kind.clone();

        let scalar_ty = if !ctx
            .codegen_backend
            .global_context
            .type_map
            .is_scalar(type_id)
        {
            Some(scalar_ty.unwrap_or(ir::types::I64))
        } else {
            scalar_ty
        };

        let fields: Vec<(Layout, _)> = ctx
            .codegen_backend
            .global_context
            .type_map
            .fields_of(type_id)
            .unwrap()
            .drain(..)
            .map(|(layout, type_id)| (layout, TypePair(type_id, scalar_ty)))
            .collect();

        if let StorageKind::Boxed(None) = self.inner.clone().into_inner() {
            // emit call to malloc and store pointer

            let signature = {
                let mut s = Signature::new(CallConv::SystemV);
                s.params.push(AbiParam::new(ir::types::I64));
                s.returns.push(AbiParam::new(ir::types::I64));
                fx.import_signature(s)
            };

            let libc_malloc = fx.import_function(ir::ExtFuncData {
                name: ir::ExternalName::User {
                    namespace: 0,
                    index: 0,
                },
                signature,
                colocated: false,
            });

            let (l, _) = StructBuf::calculate_layout_and_fields(&*fields);

            use std::convert::TryFrom;

            let size = fx
                .ins()
                .iconst(ir::types::I64, i64::try_from(l.size()).unwrap());

            let inst = fx.ins().call(libc_malloc, &[size]);

            if let [ptr] = fx.func.dfg.inst_results(inst) {
                *self.inner.borrow_mut() = StorageKind::Boxed(Some(Pointer::new(*ptr)));
            } else {
                unreachable!();
            }
        }

        StructBuf::new(self.as_ptr(), fields)
    }
}
