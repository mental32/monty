use cranelift_codegen::ir::{self, InstBuilder, MemFlags, StackSlot, Value};
use cranelift_frontend::FunctionBuilder;

use crate::prelude::LocalTypeId;

use super::{TypePair, context::CodegenLowerArg, structbuf::StructBuf};

#[derive(Debug, Clone)]
pub struct Pointer {
    pub(super) base: PointerBase,
    offset: i32,
}

impl Pointer {
    pub fn new(addr: Value) -> Self {
        Self {
            base: PointerBase::Address(addr),
            offset: 0,
        }
    }

    pub fn as_mut_struct(&self, (ctx, _): CodegenLowerArg<'_, '_, '_>, type_id: LocalTypeId) -> StructBuf {
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

        StructBuf::new(self.clone(), fields)
    }

    pub fn stack_slot(ss: StackSlot) -> Self {
        Self {
            base: PointerBase::Stack(ss),
            offset: 0,
        }
    }

    pub fn load(&self, ty: ir::Type, offset: i32, fx: &mut FunctionBuilder) -> Value {
        match self.base {
            PointerBase::Address(addr) => fx.ins().load(ty, MemFlags::new(), addr, offset),
            PointerBase::Stack(ss) => fx.ins().stack_load(ty, ss, offset),
        }
    }

    pub fn store(
        &self,
        value: Value,
        offset: i32,
        fx: &mut FunctionBuilder,
    ) -> Pointer {
        let _ = match self.base {
            PointerBase::Address(addr) => fx.ins().store(MemFlags::new(), value, addr, offset),
            PointerBase::Stack(ss) => fx.ins().stack_store(value, ss, offset),
        };

        Self {
            base: self.base,
            offset,
        }
    }

    pub fn offset(&self, offset: i32, _fx: &mut FunctionBuilder) -> Pointer {
        Self {
            base: self.base,
            offset: self.offset + offset,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) enum PointerBase {
    /// Unspecified pointer.
    Address(Value),

    /// Pointer to a stack slot.
    Stack(StackSlot),
}
