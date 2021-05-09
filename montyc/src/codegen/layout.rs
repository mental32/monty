use cranelift_codegen::ir::{StackSlot, Value};

use crate::typing::LocalTypeId;

use super::{TypePair, context::CodegenContext};

#[derive(Debug)]
struct Field {
    size: usize,
    align: usize,
    kind: TypePair,
}

/// An abstraction over a Cranelift `StackSlot` enabling editing like a struct.
#[derive(Debug)]
pub struct StructBuf {
    stack_slot: StackSlot,
    fields: Vec<Field>,
    kind: LocalTypeId,
}

impl StructBuf {
    /// Read the `n`th field of this struct and produce it as a scalar value.
    pub fn read(&self, field: usize, ctx: CodegenContext) -> Option<Value> {
        todo!()
    }

    /// Write `value` to the `n`th field of this struct and, if present, return the previous value.
    pub fn write(&mut self, field: usize, value: Value, ctx: CodegenContext) -> Result<Option<Value>, ()> {
        todo!()
    }
}
