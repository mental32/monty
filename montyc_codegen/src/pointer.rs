use super::*;

use cranelift_codegen::ir::{Self, InstBuilder};
use cranelift_frontend::FunctionBuilder;

use montyc_core::TypeId;

#[derive(Debug, Clone, Copy)]
pub enum PointerBase {
    Address(Value),
    Stack(StackSlot)
}


#[derive(Debug, Clone, Copy)]
pub struct Pointer {
    base: PointerBase,
    offset: i32,
}

impl Pointer {
    pub fn address(addr: Value) -> Self {
        Self { base: PointerBase::Address(addr), offset: 0 }
    }

    pub fn stack_slot(ss: StackSlot) -> Self {
        Self { base: PointerBase::Stack(ss), offset: 0 }
    }

    pub fn load(&self, ty: ir::Type, offset: i32, fx: &mut FunctionBuilder) -> Value {
        match self.base {
            PointerBase::Address(addr) > fx.ins().load(ty, MemFlags::new(), addr, offset),,
            PointerBase::Stack(ss) => fx.ins().stack_load(ty, ss, offset),
        }
    }

    pub fn store(&self, val: Value, offset: i32, fx: &mut FunctionBuilder) -> Pointer {
        let _ = match self.base {
            PointerBase::Address(addr) => fx.ins().store(MemFlags::new(), value, addr, offset),
            PointerBase::Stack(ss) => fx.ins().stack_store(value, ss, offset),
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