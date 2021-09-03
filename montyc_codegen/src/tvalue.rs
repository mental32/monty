use crate::pointer::{Pointer, PointerBase};

use cranelift_codegen::ir::{self, InstBuilder};
use cranelift_frontend::FunctionBuilder;

use montyc_core::TypeId;

/// A values "place" (how to access it.)
pub enum ValuePlace {
    ByVal(ir::Value),
    ByRef(Pointer),
}

/// A type ascribed value
pub struct TValue {
    pub place: ValuePlace,
    pub type_id: TypeId,
    pub ir_type: ir::types::Type,
}

impl TValue {
    pub fn deref(self, fx: &mut FunctionBuilder) -> ir::Value {
        let Self { place, ir_type, .. } = self;

        match place {
            ValuePlace::ByVal(val) => val,
            ValuePlace::ByRef(ptr) => ptr.load(ir_type, 0, fx),
        }
    }

    pub fn addr(&self, fx: &mut FunctionBuilder) -> ir::Value {
        let Self { place, ir_type, .. } = self;

        match place {
            ValuePlace::ByVal(_) => unimplemented!("can not take the address of a direct value."),
            ValuePlace::ByRef(Pointer { base, .. }) => match base {
                PointerBase::Address(addr) => *addr,
                PointerBase::Stack(stack_slot) => fx.ins().stack_addr(*ir_type, *stack_slot, 0),
            },
        }
    }
}
