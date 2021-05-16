use cranelift_codegen::ir::{StackSlotData, StackSlotKind};
use cranelift_frontend::FunctionBuilder;

use super::{context::CodegenLowerArg, pointer::Pointer, tvalue::TypedValue};
use super::{structbuf::StructBuf, tvalue::TypePair};

#[derive(Debug)]
enum StorageKind {
    Addr(Pointer),
}

#[derive(Debug)]
pub struct Storage {
    inner: StorageKind,
    kind: TypePair,
}

impl Storage {
    pub fn new_stack_slot((ctx, builder): CodegenLowerArg<'_, '_, '_>, kind: TypePair) -> Self {
        let (size, _) = ctx
            .size_and_layout_of(kind.clone())
            .expect("Type must not be zero-sized!");

        // FIXME: Don't force the size to a multiple of 16 bytes once
        //        Cranelift gets a way to specify stack slot alignment.
        let slot_size = (size + 15) / 16 * 16;

        let stack_slot = builder
            .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size));

        Self {
            inner: StorageKind::Addr(Pointer::stack_slot(stack_slot)),
            kind,
        }
    }

    pub fn write(&self, tvalue: TypedValue, fx: &mut FunctionBuilder) {
        let ptr = self.as_ptr();
        let value = tvalue.into_raw(fx);

        ptr.store(value, 0, fx);
    }

    pub fn as_ptr_value(&self) -> TypedValue {
        let ptr = self.as_ptr();

        TypedValue::by_ref(ptr, self.kind.clone())
    }

    pub fn as_ptr(&self) -> Pointer {
        match &self.inner {
            StorageKind::Addr(ptr) => ptr.clone(),
        }
    }

    pub fn as_mut_struct(&self, (ctx, _): CodegenLowerArg<'_, '_, '_>) -> StructBuf {
        let type_id = self.kind.0;
        let fields = ctx
            .codegen_backend
            .global_context
            .type_map
            .fields_of(type_id)
            .unwrap()
            .drain(..)
            .map(|(layout, type_id)| {
                (
                    layout,
                    TypePair(type_id, Some(ctx.codegen_backend.scalar_type_of(type_id))),
                )
            })
            .collect();

        StructBuf::new(self.as_ptr(), fields)
    }
}
