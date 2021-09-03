use super::*;

use cranelift_codegen::ir::{self, InstBuilder, MemFlags, StackSlot, Value};
use cranelift_frontend::FunctionBuilder;

use montyc_core::TypeId;

#[derive(Debug, Clone, Copy)]
pub enum PointerBase {
    Address(Value),
    Stack(StackSlot),
}

#[derive(Debug, Clone, Copy)]
pub struct Pointer {
    pub(crate) base: PointerBase,
    pub(crate) offset: i32,
}

impl Pointer {
    pub fn address(addr: Value) -> Self {
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

    pub fn store(&self, val: Value, offset: i32, fx: &mut FunctionBuilder) -> Pointer {
        let _ = match self.base {
            PointerBase::Address(addr) => fx.ins().store(MemFlags::new(), val, addr, offset),
            PointerBase::Stack(ss) => fx.ins().stack_store(val, ss, offset),
        };

        Self {
            base: self.base,
            offset,
        }
    }

    pub fn offset(&self, offset: i32) -> Pointer {
        Self {
            base: self.base,
            offset: self.offset + offset,
        }
    }
}
