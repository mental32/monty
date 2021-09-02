use std::convert::TryFrom;

use cranelift_codegen::ir::{self, InstBuilder};
use cranelift_frontend::FunctionBuilder;

use montyc_core::TypeId;

use crate::{pointer::Pointer, CxArg};

pub(super) struct Field {
    logical_offset: usize,
    physical_offset: i32,
    layout: std::alloc::Layout,
    kind: (TypeId, ir::Type),
}

pub struct StructBuf {
    base_pointer: Pointer,
    pub(super) fields: Vec<Field>,
}

impl StructBuf {
    /// Create a new field-less struct.
    pub fn new(base_pointer: Pointer) -> Self {
        Self {
            base_pointer,
            fields: vec![],
        }
    }

    fn calculate_layout(&self, mut offset_cb: impl FnMut(usize, i32)) -> std::alloc::Layout {
        // SAFETY: The parameters privded to from_size_align satisfy the safety invariants.
        let mut layout = unsafe { std::alloc::Layout::from_size_align_unchecked(0, 1) };

        for (ix, field) in self.fields.iter().enumerate() {
            let (new_layout, offset) = layout.extend(field.layout).expect("arithmetic overflow.");
            let offset = i32::try_from(offset).unwrap();

            offset_cb(ix, offset);

            layout = new_layout;
        }

        layout.pad_to_align()
    }

    pub fn extend<I, T>(&mut self, fields: I)
    where
        I: IntoIterator<Item = T>,
        T: Into<(std::alloc::Layout, TypeId, ir::types::Type)>,
    {
        let new_fields = fields.into_iter().map(|t| {
            let (layout, type_id, ir_type) = t.into();

            Field {
                kind: (type_id, ir_type),
                logical_offset: std::usize::MAX,
                physical_offset: std::i32::MAX,
                layout,
            }
        });

        self.fields.extend(new_fields);

        let mut offsets = Vec::with_capacity(self.fields.len());
        let _ = self.calculate_layout(|ix, phys| {
            offsets.push(phys);
            debug_assert_eq!(ix, offsets.len());
        });

        for (phys, (ix, field)) in offsets.into_iter().zip(self.fields.iter_mut().enumerate()) {
            field.logical_offset = ix;
            field.physical_offset = phys;
        }
    }

    pub fn layout(&self) -> std::alloc::Layout {
        self.calculate_layout(|_, _| ())
    }

    pub fn read_field(&self, field_ix: usize, (ctx, fx): CxArg<'_>) -> Option<ir::Value> {
        let field = self.fields.get(field_ix)?;
        let (_, ir_type) = field.kind;

        let value = self.base_pointer.load(ir_type, field.physical_offset, fx);

        Some(value)
    }

    pub fn write_field(
        &self,
        field_ix: usize,
        value: ir::Value,
        (ctx, fx): CxArg<'_>,
    ) -> Option<Pointer> {
        let field = self.fields.get(field_ix)?;
        let ptr = self.base_pointer.store(value, field.physical_offset, fx);

        Some(ptr)
    }

    pub fn addr_of(&self, field_ix: usize) -> Option<Pointer> {
        let field = self.fields.get(field_ix)?;

        Some(self.base_pointer.offset(field.physical_offset))
    }
}
