use std::convert::TryFrom;

use cranelift_codegen::ir::{self, StackSlot, Value, stackslot::StackSize};

use crate::typing::LocalTypeId;

use super::{
    context::{CodegenContext, CodegenLowerArg},
    TypePair,
};

#[derive(Debug)]
struct Field {
    offset: u32,
    layout: std::alloc::Layout,
    kind: TypePair,
}

/// An abstraction over a Cranelift `StackSlot` enabling editing like a struct.
#[derive(Debug)]
pub struct StructBuf {
    stack_slot: StackSlot,
    fields: Vec<Field>,
    layout: std::alloc::Layout,
    kind: LocalTypeId,
}

impl StructBuf {
    /// Create a new `StructBuf` of type `kind`, this will create a StackSlot for you.
    pub fn new<'a, 'b, 'c>(
        (ctx, builder): CodegenLowerArg<'a, 'b, 'c>,
        kind: LocalTypeId,
        it: impl Iterator<Item = (std::alloc::Layout, TypePair)>,
    ) -> Self {
        let tydesc = ctx
            .codegen_backend
            .global_context
            .type_map
            .get(kind)
            .unwrap()
            .clone();

        let size = crate::typing::SizeOf::size_of(&tydesc, &ctx.codegen_backend.global_context)
            .expect("type should be sized.")
            .get();

        let stack_slot = builder.create_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            size,
        ));

        let mut fields = vec![];
        let mut layout = std::alloc::Layout::from_size_align(0, 1).unwrap();

        for (field, kind) in it {
            let (new_layout, offset) = layout.extend(field).unwrap();

            fields.push(Field {
                offset: u32::try_from(offset).unwrap(),
                layout: field,
                kind,
            });

            layout = new_layout;
        }

        let layout = layout.pad_to_align();

        Self {
            stack_slot,
            fields,
            layout,
            kind,
        }
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

    /// Read the `n`th field of this struct and produce it as a scalar value.
    pub fn read<'long, 'short, 'fx>(
        &self,
        field: usize,
        (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>,
    ) -> Option<Value> {
        use cranelift_codegen::ir::InstBuilder;

        let field = self.fields.get(field)?;
        let value =
            builder
                .ins()
                .stack_load(field.kind.1, self.stack_slot, field.offset as i32);

        Some(value)
    }

    /// Write `value` to the `n`th field of this struct and, if present, return the previous value.
    pub fn write<'long, 'short, 'fx>(
        &self,
        field: usize,
        value: Value,
        (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>,
    ) -> Option<()> {
        use cranelift_codegen::ir::InstBuilder;

        let field = self.fields.get(field)?;

        builder
            .ins()
            .stack_store(value, self.stack_slot, field.offset as i32);

        Some(())
    }

    pub fn addr<'long, 'short, 'fx>(&self, (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>, field: usize) -> Option<Value> {
        use cranelift_codegen::ir::InstBuilder;

        let offset = self.fields.get(field)?.offset;
        let offset = i32::try_from(offset).ok()?;

        Some(builder.ins().stack_addr(ir::types::I64, self.stack_slot, offset))
    }
}
