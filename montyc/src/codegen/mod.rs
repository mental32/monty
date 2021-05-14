#![allow(warnings)]

use crate::typing::LocalTypeId;

use self::context::CodegenLowerArg;

pub mod context;
pub mod lower_ast;

pub mod structbuf {
    use std::convert::TryFrom;

    use cranelift_codegen::ir::{self, stackslot::StackSize, MemFlags, StackSlot, Value};

    use crate::typing::LocalTypeId;

    use super::{
        context::{CodegenContext, CodegenLowerArg},
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
            let mut fields_ = vec![];
            let mut layout = std::alloc::Layout::from_size_align(0, 1).unwrap();

            for (field, kind) in fields {
                let (new_layout, offset) = layout.extend(field).unwrap();

                fields_.push(Field {
                    offset: i32::try_from(offset).unwrap(),
                    layout: field,
                    kind,
                });

                layout = new_layout;
            }

            let layout = layout.pad_to_align();

            Self {
                pointer,
                fields: fields_,
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
            (ctx, builder): CodegenLowerArg<'_, '_, '_>,
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
            (ctx, builder): CodegenLowerArg<'_, '_, '_>,
        ) -> Option<Pointer> {
            let field = self.fields.get(field)?;

            let kind = if let TypePair(_, Some(kind)) = field.kind {
                kind
            } else {
                unreachable!()
            };

            let ptr = self.pointer.store(kind, value, field.offset, builder);

            Some(ptr)
        }

        pub fn addr(
            &self,
            (ctx, builder): CodegenLowerArg<'_, '_, '_>,
            field: usize,
        ) -> Option<Pointer> {
            let field = self.fields.get(field)?;

            let kind = if let TypePair(_, Some(kind)) = field.kind {
                kind
            } else {
                unreachable!()
            };

            let ptr = self.pointer.offset(field.offset, builder);

            Some(ptr)
        }
    }
}

pub trait LowerCodegen {
    fn lower(&self, _: CodegenLowerArg<'_, '_, '_>) -> Option<tvalue::TypedValue>;
}

pub use tvalue::{TypedValue, TypePair};

mod tvalue {
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
                _ => unreachable!(),
            }
        }
    }
}

mod pointer {
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
            ty: ir::Type,
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

        pub fn offset(&self, offset: i32, fx: &mut FunctionBuilder) -> Pointer {
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
}

mod storage {
    use std::convert::TryFrom;

    use cranelift_codegen::ir::{StackSlot, StackSlotData, StackSlotKind};
    use cranelift_frontend::FunctionBuilder;

    use crate::typing::LocalTypeId;

    use super::{context::CodegenLowerArg, pointer::Pointer, tvalue::TypedValue};
    use super::{structbuf::StructBuf, tvalue::TypePair};

    #[derive(Debug)]
    enum StorageKind {
        Addr(Pointer),
    }

    #[derive(Debug)]
    pub struct Storage {
        inner: StorageKind,
        kind: TypePair,
    }

    impl Storage {
        pub fn new_stack_slot((ctx, builder): CodegenLowerArg<'_, '_, '_>, kind: TypePair) -> Self {
            let (size, layout) = ctx
                .size_and_layout_of(kind.clone())
                .expect("Type must not be zero-sized!");

            // FIXME: Don't force the size to a multiple of 16 bytes once
            //        Cranelift gets a way to specify stack slot alignment.
            let slot_size = (size + 15) / 16 * 16;

            let stack_slot = builder
                .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size));

            Self {
                inner: StorageKind::Addr(Pointer::stack_slot(stack_slot)),
                kind,
            }
        }

        pub fn write(&self, tvalue: TypedValue, fx: &mut FunctionBuilder) {
            let ptr = self.as_ptr();

            let ty = tvalue.kind.1.unwrap();

            let value = tvalue.into_raw(fx);

            ptr.store(ty, value, 0, fx);
        }

        pub fn as_ptr_value(&self) -> TypedValue {
            let ptr = self.as_ptr();

            TypedValue::by_ref(ptr, self.kind.clone())
        }

        pub fn as_ptr(&self) -> Pointer {
            match &self.inner {
                StorageKind::Addr(ptr) => ptr.clone(),
            }
        }

        pub fn as_mut_struct(&self, (ctx, builder): CodegenLowerArg<'_, '_, '_>) -> StructBuf {
            let type_id = self.kind.0;
            let fields = ctx
                .codegen_backend
                .global_context
                .type_map
                .fields_of(type_id)
                .unwrap()
                .drain(..)
                .map(|(layout, type_id)| {
                    (
                        layout,
                        TypePair(type_id, Some(ctx.codegen_backend.scalar_type_of(type_id))),
                    )
                })
                .collect();

            StructBuf::new(self.as_ptr(), fields)
        }
    }
}
