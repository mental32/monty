use std::convert::TryFrom;

use cranelift_codegen::ir::{Value};

use super::{
    context::{CodegenLowerArg},
    pointer::Pointer,
    tvalue::TypePair,
};

#[derive(Debug)]
struct Field {
    offset: i32,
    layout: std::alloc::Layout,
    kind: TypePair,
}

/// An abstraction over a Cranelift `StackSlot` enabling editing like a struct.
#[derive(Debug)]
pub struct StructBuf {
    pointer: Pointer,
    fields: Vec<Field>,
    layout: std::alloc::Layout,
}

impl StructBuf {
    pub fn new(pointer: Pointer, fields: Vec<(std::alloc::Layout, TypePair)>) -> Self {
        let mut struct_fields = vec![];
        let mut layout = std::alloc::Layout::from_size_align(0, 1).unwrap();

        for (field, kind) in fields {
            let (new_layout, offset) = layout.extend(field).unwrap();

            struct_fields.push(Field {
                offset: i32::try_from(offset).unwrap(),
                layout: field,
                kind,
            });

            layout = new_layout;
        }

        let layout = layout.pad_to_align();

        Self {
            pointer,
            fields: struct_fields,
            layout,
        }
    }

    /// The amount of fields
    pub fn field_count(&self) -> usize {
        self.fields.len()
    }

    /// Read the `n`th field of this struct and produce it as a scalar value.
    pub fn read(
        &self,
        field: usize,
        (_, builder): CodegenLowerArg<'_, '_, '_>,
    ) -> Option<Value> {
        let field = self.fields.get(field)?;

        let kind = if let TypePair(_, Some(kind)) = field.kind {
            kind
        } else {
            unreachable!()
        };

        let value = self.pointer.load(kind, field.offset, builder);

        Some(value)
    }

    /// Write `value` to the `n`th field of this struct and return a pointer value to the value.
    pub fn write(
        &self,
        field: usize,
        value: Value,
        (_, builder): CodegenLowerArg<'_, '_, '_>,
    ) -> Option<Pointer> {
        let field = self.fields.get(field)?;

        let ptr = self.pointer.store(value, field.offset, builder);

        Some(ptr)
    }

    pub fn addr(
        &self,
        (_, builder): CodegenLowerArg<'_, '_, '_>,
        field: usize,
    ) -> Option<Pointer> {
        let field = self.fields.get(field)?;

        let ptr = self.pointer.offset(field.offset, builder);

        Some(ptr)
    }
}
