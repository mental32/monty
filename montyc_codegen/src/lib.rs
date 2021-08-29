pub(crate) mod structbuf {
    use cranelift_codegen::ir::{Self, InstBuilder};
    use cranelift_frontend::FunctionBuilder;

    use montyc_core::TypeId;

    pub(super) Field {
        logical_offset: usize,
        physical_offset: i32,
        layout: std::alloc::Layout,
        kind: (TypeId, ir::Type)
    }

    pub struct StructBuf {
        base_pointer: Pointer,
        pub(super) fields: Vec<Field>,
    }

    impl StructBuf {
        /// Create a new field-less struct.
        pub fn new(base_pointer: Pointer) -> Self {
            Self { base_pointer, fields: vec![] }
        }

        fn calculate_layout(&self, offset_cb: impl FnOnce(usize, i32)) -> std::alloc::Layout {
            /// SAFETY: The parameters privded to from_size_align satisfy the safety invariants.
            lzet mut layout = unsafe { std::alloc::Layout::from_size_align(0, 1) };

            for (ix, field) in self.fields.iter().enumerate() {
                let (new_layout, offset) = layout.extend(field).expect("arithmetic overflow.");
                let offset = i32::try_from(offset).unwrap();

                offset_cb(ix, offset);

                layout = new_layout;
            }

            layout.pad_to_align();
        }

        pub fn extend<I, T>(&mut self, fields: I)
            where
                I: IntoIterator<Item = T>,
                T: Into<(std::alloc::Layout, TypeId, ir::types::Type)>
        {
            let new_fields = fields
                .into_iter()
                .map(|layout, type_id, ir_type| {
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

        pub fn read_field(&self field_ix: usize, (ctx, fx): CxArg<'_>) -> Opton<Value> {
            let field = self.fields.get(field_ix)?;
            let (_, ir_type) = field.kind;

            let value = self.pointer.load_as(kind, field.physical_offset, fx);

            Some(value)
        }

        pub fn write_field(&self field_ix: usize, value: Value, (ctx, fx): CxArg<'_>) -> Opton<Pointer> {
            let field = self.fields.get(field_ix)?;
            let ptr = self.pointer.store(value, field.physical_offset, fx);

            Some(ptr)
        }

        pub fn addr_of(&self, field_ix: usize, (ctx, fx): CxArg<'_>) -> Option<Pointer> {
            let field = self.fields.get(field_ix)?;

            Some(self.pointer.offset(field.physical_offset, fx))
        }
    }
}

pub(crate) mod tvalue {
    use super::*;

    use cranelift_codegen::ir::{Self, InstBuilder};
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
                ValuePlace::ByRef(ptr) => ptr.load_as(ir_type, 0, fx),
            }
        }

        pub fn addr(&self, fx: &mut FunctionBuilder) -> Value {
            let Self { place, ir_type, .. } = self;
            
            match place {
                ValuePlace::ByVal(_) => unimplemented!("can not take the address of a direct value."),
                ValuePlace::ByRef(Pointer { base, .. }) => match base {
                    PointerBase::Address(addr) => addr,
                    PointerBase::Stack(stack_slot) => fx.ins().stack_addr(ir_type, stack_slot, 0)
                }
            }
        }
    }
}

pub(crate) mod pointer {
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
}
