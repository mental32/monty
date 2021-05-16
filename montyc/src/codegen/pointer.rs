use cranelift_codegen::ir::{self, InstBuilder, MemFlags, StackSlot, Value};
use cranelift_frontend::FunctionBuilder;

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
