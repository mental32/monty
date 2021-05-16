use cranelift_codegen::ir::{self, InstBuilder, Value};
use cranelift_frontend::FunctionBuilder;

use crate::typing::LocalTypeId;

use super::pointer::Pointer;

#[derive(Debug, Clone)]
pub struct TypePair(pub(crate) LocalTypeId, pub(crate) Option<ir::types::Type>);

#[derive(Debug, Clone)]
enum InnerValue {
    ByValue(Value),
    ByRef(Pointer),
}

#[derive(Debug, Clone)]
pub struct TypedValue {
    inner: InnerValue,
    pub(super) kind: TypePair,
}

impl TypedValue {
    pub fn by_ref(ptr: Pointer, kind: TypePair) -> Self {
        Self {
            inner: InnerValue::ByRef(ptr),
            kind,
        }
    }

    pub fn by_val(value: Value, kind: TypePair) -> Self {
        Self {
            inner: InnerValue::ByValue(value),
            kind,
        }
    }

    pub fn kind(&self) -> TypePair {
        self.kind.clone()
    }

    pub fn deref_into_raw(self, fx: &mut FunctionBuilder) -> Value {
        match self.inner {
            InnerValue::ByValue(value) => value,
            InnerValue::ByRef(ptr) => ptr.load(self.kind.1.unwrap(), 0, fx),
        }
    }

    pub fn as_ptr(&self) -> Pointer {
        match &self.inner {
            InnerValue::ByValue(_) => unimplemented!(),
            InnerValue::ByRef(ptr) => ptr.clone(),
        }
    }

    pub fn ref_value(&self, fx: &mut FunctionBuilder) -> Value {
        match self.inner {
            InnerValue::ByValue(_) => unreachable!(),
            InnerValue::ByRef(Pointer { base, .. }) => match base {
                    crate::codegen::pointer::PointerBase::Address(addr) => addr,
                    crate::codegen::pointer::PointerBase::Stack(ss) => fx.ins().stack_addr(
                        self.kind
                            .1
                            .expect("TypedValues must have a ir::Type to lower to!"),
                        ss,
                        0,
                    ),
            }
        }
    }

    pub fn into_raw(self, fx: &mut FunctionBuilder) -> Value {
        match self.inner {
            InnerValue::ByValue(v) => v,
            InnerValue::ByRef(_) => self.ref_value(fx),
            // _ => unreachable!(),
        }
    }
}
