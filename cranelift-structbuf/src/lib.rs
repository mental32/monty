// #![deny(missing_docs)]

use std::convert::TryFrom;
use std::{alloc, ops::Range};

use cranelift_codegen::ir::{self, stackslot::StackOffset, InstBuilder, MemFlags, StackSlot};
use cranelift_frontend::FunctionBuilder;

// -- StructField

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructField<Ty> {
    Scalar(ir::Type),
    Complex(StructLayout<Ty>),
}

macro_rules! scalar_fields {
    [ $( ( $i:ident , $p:path ) ),* ] => {
        impl<Ty> $crate::StructField<Ty> {
            $(
                pub fn $i() -> Self {
                    Self::Scalar($p)
                }
            )*
        }
    };
}

scalar_fields![
    (i8, ir::types::I8),
    (i16, ir::types::I16),
    (i32, ir::types::I32),
    (i64, ir::types::I64),
    (i128, ir::types::I128),
    (b1, ir::types::B1),
    (b8, ir::types::B8),
    (b16, ir::types::B16),
    (b32, ir::types::B32),
    (b64, ir::types::B64),
    (f32, ir::types::F32),
    (f64, ir::types::F64)
];

impl<Ty> StructField<Ty> {
    pub fn bytes(&self) -> u32 {
        match self {
            Self::Scalar(s) => s.bytes(),
            Self::Complex(s) => s.fields.iter().map(|f| f.bytes()).sum(),
        }
    }
}

// -- StructLayout

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct StructLayout<Ty = ()> {
    type_tag: Ty,
    fields: Vec<StructField<Ty>>,
}

impl<Ty> StructLayout<Ty>
where
    Ty: Default,
{
    /// Construct a new `StructLayout` with some fields (use `None` if there are no pre-existing fields.)
    ///
    /// ```
    /// use cranelift_structbuf::*;
    ///
    /// let _ = StructLayout::<()>::new(None);
    /// let struky = StructLayout::<()>::new(Some(StructField::i64()));
    /// ```
    ///
    pub fn new<I>(fields: I) -> Self
    where
        I: IntoIterator<Item = StructField<Ty>>,
    {
        Self {
            type_tag: Ty::default(),
            fields: fields.into_iter().map(|t| t.into()).collect(),
        }
    }
}

impl<Ty> StructLayout<Ty> {
    /// Construct a new `StructLayout` with some fields and tagged with a type (use `None` if there are no pre-existing fields.)
    ///
    /// ```
    /// use cranelift_structbuf::*;
    ///
    /// let _ = StructLayout::new_with_type(123, None);
    /// let struky = StructLayout::<()>::new(Some(StructField::i64()));
    /// ```
    ///
    pub fn new_with_type<I>(ty_tag: Ty, fields: I) -> Self
    where
        I: IntoIterator<Item = StructField<Ty>>,
    {
        Self {
            type_tag: ty_tag,
            fields: fields.into_iter().map(|t| t.into()).collect(),
        }
    }

    /// Add a field to this structs layout.
    ///
    /// ```
    /// use cranelift_structbuf::*;
    ///
    /// let index = StructLayout::<()>::new(None).push_field(StructField::b16());
    /// assert_eq!(index, 0);
    /// ```
    ///
    pub fn push_field<T>(&mut self, field: T) -> usize
    where
        T: Into<StructField<Ty>>,
    {
        self.fields.push(field.into());
        self.fields.len().saturating_sub(1)
    }
}

// -- Struct

#[derive(Debug)]
pub struct ValueCell {
    group: usize,

    byte_offset: i32,
    kind: ir::Type,
    value: Option<ir::Value>,
}

impl ValueCell {
    #[inline]
    pub fn byte_offset(&self) -> i32 {
        self.byte_offset
    }

    #[inline]
    pub fn kind(&self) -> ir::Type {
        self.kind
    }

    #[inline]
    pub fn value(&self) -> Option<ir::Value> {
        self.value.clone()
    }
}

#[derive(Debug)]
pub struct Struct<Ty = ()> {
    template: StructLayout<Ty>,
    layout: alloc::Layout,
    slots: Box<[ValueCell]>,
    slot_ranges: Vec<Range<usize>>,
}

impl<Ty> From<&StructLayout<Ty>> for Struct<Ty>
where
    Ty: Clone,
{
    fn from(template: &StructLayout<Ty>) -> Self {
        let (layout, slots, slot_ranges) = alloc_layout_from_template(template);

        Self {
            slot_ranges,
            slots: slots.into_boxed_slice(),
            template: template.clone(),
            layout,
        }
    }
}

impl<Ty> From<StructLayout<Ty>> for Struct<Ty> {
    fn from(template: StructLayout<Ty>) -> Self {
        let (layout, slots, slot_ranges) = alloc_layout_from_template(&template);

        Self {
            slot_ranges,
            slots: slots.into_boxed_slice(),
            template,
            layout,
        }
    }
}

fn alloc_layout_from_template<Ty>(
    template: &StructLayout<Ty>,
) -> (alloc::Layout, Vec<ValueCell>, Vec<Range<usize>>) {
    use std::alloc::Layout;

    let mut layout = unsafe { Layout::from_size_align_unchecked(0, 1) };
    let mut slots = Vec::with_capacity(32);

    fn rec<Ty>(
        this: &StructLayout<Ty>,
        mut layout: Layout,
        slots: &mut Vec<ValueCell>,
        group: usize,
    ) -> (Layout, Vec<Range<usize>>) {
        let mut slot_ranges = Vec::with_capacity(this.fields.len());

        for field in this.fields.iter() {
            let (l, r) = match field {
                StructField::Scalar(kind) => {
                    let (l, o) = Layout::array::<u8>(kind.bytes() as usize)
                        .and_then(|bytes| layout.extend(bytes))
                        .expect("layout error: integer overflow");

                    slots.push(ValueCell {
                        group,
                        kind: *kind,
                        byte_offset: i32::try_from(o).unwrap(),
                        value: None,
                    });

                    (l, vec![slots.len().saturating_sub(1)..slots.len()])
                }

                StructField::Complex(strukt) => rec(strukt, layout, slots, group + 1),
            };

            slot_ranges.push({
                let (head, tail) = r.first().cloned().zip(r.last().cloned()).unwrap();

                head.start..tail.end
            });

            layout = l;
        }

        (layout, slot_ranges)
    }

    let (l, r) = rec(template, layout, &mut slots, 0);

    layout = l;
    layout = layout.pad_to_align();

    (layout, slots, r)
}

impl<Ty> Struct<Ty> {
    /// Get the raw `std::alloc::Layout` of the struct.
    pub fn layout(&self) -> &alloc::Layout {
        &self.layout
    }

    /// Get the type tag for this struct
    pub fn type_tag(&self) -> &Ty {
        &self.template.type_tag
    }

    /// Get the metadata for the `n`th field including its byte offset.
    pub fn field(&self, n: usize) -> Option<(&StructField<Ty>, i32)> {
        let (field, range) = self.template.fields.get(n).zip(self.slot_ranges.get(n))?;
        let cell = self.slots.get(range.start)?;

        Some((field, cell.byte_offset))
    }

    pub fn read<'a>(&'a self, field: usize) -> Option<PendingRead<'a, Ty>> {
        let slot_range = self.slot_ranges.get(field)?.clone();
        let (field, _) = self.field(field)?;

        let read = PendingRead {
            strukt: self,
            field: field,
            slot_range,
        };

        Some(read)
    }

    pub fn write<'a>(&'a mut self, field: usize) -> Option<PendingWrite<'a, Ty>> {
        let slot_range = self.slot_ranges.get(field)?.clone();
        let write = PendingWrite {
            strukt: self,
            slot_range,
        };

        Some(write)
    }
}

#[must_use]
pub enum ReadResult {
    Scalar(ir::Value),
    Complex(ir::Value, i32),
}

impl ReadResult {
    pub fn into_inner(self) -> ir::Value {
        match self {
            Self::Scalar(v) | Self::Complex(v, _) => v,
        }
    }
}

impl From<ReadResult> for ir::Value {
    fn from(rr: ReadResult) -> Self {
        rr.into_inner()
    }
}

#[must_use]
pub struct PendingRead<'a, Ty> {
    strukt: &'a Struct<Ty>,
    field: &'a StructField<Ty>,
    slot_range: Range<usize>,
}

/// let mut layout = StructLayout::new_with_type((), None);
/// layout.push_field(ir::types::I64);
/// let mut strukt = Struct::from(layout);
/// match strukt.read(0).unwrap().stack_slot(ir::types::I64, &mut fx, ss0) {
///     ReadResult::Scalar(val) => val,
///     ReadResult::Complex(base, offset) => todo!()
/// }
impl<'a, Ty> PendingRead<'a, Ty> {
    fn slots(&self) -> &[ValueCell] {
        &self.strukt.slots[self.slot_range.clone()]
    }

    pub fn stack_slot(
        self,
        addr_t: ir::Type,
        fx: &mut FunctionBuilder,
        ss: StackSlot,
    ) -> ReadResult {
        match self.slots() {
            [] => unreachable!(),
            [scalar] => {
                ReadResult::Scalar(fx.ins().stack_load(scalar.kind, ss, scalar.byte_offset))
            }
            [head, _tail @ ..] => {
                ReadResult::Complex(fx.ins().stack_addr(addr_t, ss, head.byte_offset), 0)
            }
        }
    }

    pub fn address_value(
        self,
        fx: &mut FunctionBuilder,
        memflags: MemFlags,
        addr: ir::Value,
    ) -> ReadResult {
        match self.slots() {
            [] => unreachable!(),
            [scalar] => {
                ReadResult::Scalar(
                    fx.ins()
                        .load(scalar.kind, memflags, addr, scalar.byte_offset),
                )
            }

            [head, _tail @ ..] => ReadResult::Complex(addr, head.byte_offset),
        }
    }
}

#[must_use]
pub struct PendingWrite<'a, Ty> {
    strukt: &'a mut Struct<Ty>,
    slot_range: Range<usize>,
}
