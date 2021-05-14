use std::{
    any::TypeId,
    cell::Cell,
    fmt,
    num::{NonZeroU32, NonZeroUsize},
};

use dashmap::DashMap;

use crate::{
    codegen::context::CodegenLowerArg,
    context::{GlobalContext, LocalContext, ModuleRef},
    prelude::SpanRef,
};

pub type NodeId = Option<NonZeroUsize>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, derive_more::From)]
#[repr(transparent)]
pub struct LocalTypeId(usize);

impl LocalTypeId {
    #[inline]
    pub fn as_usize(&self) -> usize {
        self.0
    }

    #[inline]
    pub fn is_builtin(&self) -> bool {
        (0..=255).contains(&self.0)
    }
}

#[derive(Debug)]
pub struct TaggedType<T> {
    pub type_id: LocalTypeId,
    pub inner: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub reciever: Option<LocalTypeId>,
    pub name: SpanRef,
    pub args: Vec<LocalTypeId>,
    pub ret: LocalTypeId,
    pub module_ref: ModuleRef,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Generic {
    Pointer { inner: LocalTypeId },
    Union { inner: Vec<LocalTypeId> },
    Struct { inner: Vec<LocalTypeId> },
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[repr(u8)]
pub enum BuiltinTypeId {
    Invalid = 0,
    Int = 1,
    Float = 2,
    Str = 3,
    Bool = 4,
    None = 5,
    Ellipsis = 6,
    Module = 7,
    Unknown = 8,
    Type = 9,
    Object = 10,

    U8 = 100,
    U16 = 101,
    U32 = 102,
    U64 = 103,

    I8 = 104,
    I16 = 105,
    I32 = 106,
    I64 = 107,

    Never = 255,
}

impl std::fmt::Display for BuiltinTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinTypeId::Invalid => write!(f, "{{error: invalid type}}"),
            BuiltinTypeId::Int => write!(f, "{{integer}}"),
            BuiltinTypeId::Float => write!(f, "{{float}}"),
            BuiltinTypeId::Str => write!(f, "{{str}}"),
            BuiltinTypeId::Bool => write!(f, "{{bool}}"),
            BuiltinTypeId::None => write!(f, "{{None}}"),
            BuiltinTypeId::Ellipsis => write!(f, "... (ellipsis)"),
            BuiltinTypeId::Module => write!(f, "{{module}}"),
            BuiltinTypeId::Unknown => write!(f, "{{unknown}}"),
            BuiltinTypeId::Never => write!(f, "{{never}}"),
            BuiltinTypeId::U8 => write!(f, "{{u8}}"),
            BuiltinTypeId::U16 => write!(f, "{{u16}}"),
            BuiltinTypeId::U32 => write!(f, "{{u32}}"),
            BuiltinTypeId::U64 => write!(f, "{{u64}}"),
            BuiltinTypeId::I8 => write!(f, "{{i8}}"),
            BuiltinTypeId::I16 => write!(f, "{{i16}}"),
            BuiltinTypeId::I32 => write!(f, "{{i32}}"),
            BuiltinTypeId::I64 => write!(f, "{{i64}}"),
            BuiltinTypeId::Type => write!(f, "{{type}}"),
            BuiltinTypeId::Object => write!(f, "{{object}}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub name: SpanRef,
    pub kind: LocalTypeId,
    pub mref: ModuleRef,
}

#[derive(Debug, Clone, PartialEq, derive_more::From)]
pub enum TypeDescriptor {
    Simple(BuiltinTypeId),
    Function(FunctionType),
    Class(ClassType),
    Generic(Generic),
}

impl TypeDescriptor {
    fn inner_type_id(&self) -> TypeId {
        match self {
            Self::Simple(_) => TypeId::of::<BuiltinTypeId>(),
            Self::Function(_) => TypeId::of::<FunctionType>(),
            Self::Class(_) => TypeId::of::<ClassType>(),
            Self::Generic(_) => TypeId::of::<Generic>(),
        }
    }

    fn inner_ptr(&self) -> *const () {
        match self {
            Self::Simple(s) => s as *const _ as *const (),
            Self::Function(f) => f as *const _ as *const (),
            Self::Class(c) => c as *const _ as *const (),
            Self::Generic(g) => g as *const _ as *const (),
        }
    }
}

pub trait TypedObject {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId>;

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()>;
}

pub type CoercionRule = for<'a, 'b, 'c> fn(
    CodegenLowerArg<'a, 'b, 'c>,
    crate::codegen::TypedValue,
) -> crate::codegen::TypedValue;

pub struct TypeMap {
    last_id: Cell<usize>,
    inner: DashMap<LocalTypeId, TypeDescriptor>,
    coercion_rules: DashMap<(LocalTypeId, LocalTypeId), CoercionRule>,
}

impl fmt::Debug for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeMap")
            .field("last_id", &self.last_id.get())
            .field("inner", &self.inner)
            .finish()
    }
}

impl TypeMap {
    pub const INTEGER: LocalTypeId = LocalTypeId(1);
    pub const FLOAT: LocalTypeId = LocalTypeId(2);
    pub const STRING: LocalTypeId = LocalTypeId(3);
    pub const BOOL: LocalTypeId = LocalTypeId(4);
    pub const NONE_TYPE: LocalTypeId = LocalTypeId(5);
    pub const ELLIPSIS: LocalTypeId = LocalTypeId(6);
    pub const MODULE: LocalTypeId = LocalTypeId(7);
    pub const UNKNOWN: LocalTypeId = LocalTypeId(8);
    pub const TYPE: LocalTypeId = LocalTypeId(9);
    pub const OBJECT: LocalTypeId = LocalTypeId(10);
    pub const NEVER: LocalTypeId = LocalTypeId(255);

    pub const U8: LocalTypeId = LocalTypeId(100);
    pub const U16: LocalTypeId = LocalTypeId(101);
    pub const U32: LocalTypeId = LocalTypeId(102);
    pub const U64: LocalTypeId = LocalTypeId(103);
    pub const I8: LocalTypeId = LocalTypeId(104);
    pub const I16: LocalTypeId = LocalTypeId(105);
    pub const I32: LocalTypeId = LocalTypeId(106);
    pub const I64: LocalTypeId = LocalTypeId(107);

    #[inline]
    pub fn correctly_initialized() -> Self {
        let mapping = DashMap::with_capacity(std::mem::variant_count::<BuiltinTypeId>());

        mapping.insert(Self::INTEGER, TypeDescriptor::Simple(BuiltinTypeId::Int));
        mapping.insert(Self::FLOAT, TypeDescriptor::Simple(BuiltinTypeId::Float));
        mapping.insert(Self::STRING, TypeDescriptor::Simple(BuiltinTypeId::Str));
        mapping.insert(Self::BOOL, TypeDescriptor::Simple(BuiltinTypeId::Bool));
        mapping.insert(Self::NONE_TYPE, TypeDescriptor::Simple(BuiltinTypeId::None));
        mapping.insert(
            Self::ELLIPSIS,
            TypeDescriptor::Simple(BuiltinTypeId::Ellipsis),
        );

        mapping.insert(Self::MODULE, TypeDescriptor::Simple(BuiltinTypeId::Module));
        mapping.insert(Self::TYPE, TypeDescriptor::Simple(BuiltinTypeId::Type));
        mapping.insert(Self::OBJECT, TypeDescriptor::Simple(BuiltinTypeId::Object));

        mapping.insert(
            Self::UNKNOWN,
            TypeDescriptor::Simple(BuiltinTypeId::Unknown),
        );

        mapping.insert(Self::NEVER, TypeDescriptor::Simple(BuiltinTypeId::Never));

        mapping.insert(Self::U8, TypeDescriptor::Simple(BuiltinTypeId::U8));
        mapping.insert(Self::U16, TypeDescriptor::Simple(BuiltinTypeId::U16));
        mapping.insert(Self::U32, TypeDescriptor::Simple(BuiltinTypeId::U32));
        mapping.insert(Self::U64, TypeDescriptor::Simple(BuiltinTypeId::U64));
        mapping.insert(Self::I8, TypeDescriptor::Simple(BuiltinTypeId::I8));
        mapping.insert(Self::I16, TypeDescriptor::Simple(BuiltinTypeId::I16));
        mapping.insert(Self::I32, TypeDescriptor::Simple(BuiltinTypeId::I32));
        mapping.insert(Self::I64, TypeDescriptor::Simple(BuiltinTypeId::I64));

        Self {
            inner: mapping,
            last_id: Cell::new(256),
            coercion_rules: DashMap::default(),
        }
    }

    pub fn is_class(&self, type_id: LocalTypeId) -> bool {
        matches!(self.get_tagged::<ClassType>(type_id), Some(Ok(_)))
    }

    fn traverse_fields(
        &self,
        elements: &[LocalTypeId],
    ) -> impl Iterator<Item = (std::alloc::Layout, LocalTypeId)> {
        let mut fields = vec![];

        for kind in elements {
            let tydesc = self.get(*kind).unwrap();

            let tydesc = tydesc.value();

            if let TypeDescriptor::Generic(Generic::Struct { inner }) = tydesc {
                fields.extend(self.traverse_fields(inner.as_slice()));
            } else {
                let size = SizeOf::size_of(tydesc, self).unwrap().get();
                let align = size; // TODO: investigate if alignof(scalar_t) == sizeof(scalar_t)

                let layout =
                    std::alloc::Layout::from_size_align(size as usize, align as usize).unwrap();

                fields.push((layout, *kind))
            }
        }

        fields.into_iter()
    }

    #[inline]
    pub fn fields_of(
        &self,
        type_id: LocalTypeId,
    ) -> Option<Vec<(std::alloc::Layout, LocalTypeId)>> {
        let tydesc = self.get(type_id)?;
        let tydesc = tydesc.value();

        match type_id {
            Self::UNKNOWN => None,

            type_id => {
                if type_id.is_builtin() {
                    let size = SizeOf::size_of(tydesc, self).unwrap().get();
                    let align = size; // TODO: investigate if alignof(scalar_t) == sizeof(scalar_t)

                    let layout =
                        std::alloc::Layout::from_size_align(size as usize, align as usize).unwrap();

                    Some(vec![(layout, type_id)])
                } else {
                    Some(self.traverse_fields(&[type_id]).collect())
                }
            }
        }
    }

    #[inline]
    pub fn size_and_layout(&self, type_id: LocalTypeId) -> Option<(u32, std::alloc::Layout)> {
        let mut layout = std::alloc::Layout::from_size_align(0, 1).unwrap();

        for (field, _) in self.traverse_fields(&[type_id]) {
            let (new_layout, _) = layout.extend(field).unwrap();
            layout = new_layout;
        }

        let layout = layout.pad_to_align();

        Some((layout.size() as u32, layout))
    }

    #[inline]
    pub fn add_coercion_rule(&self, from: LocalTypeId, to: LocalTypeId, rule: CoercionRule) {
        log::trace!("typing:add_coercion_rule from={:?} to={:?}", from, to);

        self.coercion_rules.insert((from, to), rule);
    }

    #[inline]
    pub fn coerce<'a, 'b>(&self, from: LocalTypeId, to: LocalTypeId) -> Option<CoercionRule> {
        if to == Self::OBJECT {
            return Some(|_, value| value);
        }

        self.coercion_rules
            .get(&(from, to))
            .map(|rule| rule.value().clone())
    }

    #[inline]
    pub fn insert<T>(&self, t: T) -> LocalTypeId
    where
        T: Into<TypeDescriptor>,
    {
        let idx = self.last_id.replace(self.last_id.get() + 1);
        let idx = LocalTypeId(idx);

        self.inner.insert(idx, t.into());

        idx
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = (LocalTypeId, TypeDescriptor)> {
        let it: Vec<_> = self.inner.iter().map(|refm| (refm.key().clone(), refm.value().clone())).collect();
        it.into_iter()
    }

    #[inline]
    pub fn unify_call<'a>(
        &self,
        func_t: LocalTypeId,
        callsite: impl Iterator<Item = &'a LocalTypeId>,
    ) -> Result<LocalTypeId, (LocalTypeId, LocalTypeId, usize)> {
        log::trace!("typing:unify_call {:?}", func_t);

        let func = self.get_tagged::<FunctionType>(func_t).unwrap().unwrap();

        for (idx, (actual, expected)) in callsite.cloned().zip(func.inner.args).enumerate() {
            if actual == Self::UNKNOWN || expected == Self::OBJECT {
                continue;
            }

            match self.get_tagged::<ClassType>(actual) {
                Some(Ok(_)) if expected == TypeMap::TYPE => continue,
                Some(_) => (),
                None => unreachable!(),
            }

            if !self.type_eq(actual, expected) {
                return Err((expected, actual, idx));
            }
        }

        Ok(func_t)
    }

    #[inline]
    pub fn unify_func(&self, func_t: LocalTypeId, mold: &FunctionType) -> bool {
        let func = self.get_tagged::<FunctionType>(func_t).unwrap().unwrap();

        if (func.inner.name != mold.name)
            || (func.inner.args.len() != mold.args.len())
            || (mold.ret != Self::UNKNOWN && mold.ret != func.inner.ret)
        {
            return false;
        }

        func.inner
            .args
            .iter()
            .zip(mold.args.iter())
            .all(|(l, r)| (*l == Self::UNKNOWN) || (l == r))
    }

    #[inline]
    pub fn insert_tagged<T>(&self, t: T) -> TaggedType<T>
    where
        T: Into<TypeDescriptor> + Clone,
    {
        let type_id = self.insert(t.clone().into());

        TaggedType { type_id, inner: t }
    }

    #[inline]
    pub fn entry(&self, t: impl Into<TypeDescriptor>) -> LocalTypeId {
        let ty = t.into();

        for types in self.inner.iter() {
            if *types.value() == ty {
                return *types.key();
            }
        }

        self.insert(ty)
    }

    #[inline]
    pub fn type_eq(&self, left: LocalTypeId, right: LocalTypeId) -> bool {
        left == right || self.coercion_rules.contains_key(&(left, right))
    }

    #[inline]
    pub fn get(
        &self,
        type_id: LocalTypeId,
    ) -> Option<dashmap::mapref::one::Ref<'_, LocalTypeId, TypeDescriptor>> {
        self.inner.get(&type_id)
    }

    #[inline]
    pub fn get_tagged<T: 'static + Clone>(
        &self,
        type_id: LocalTypeId,
    ) -> Option<Result<TaggedType<T>, TypeDescriptor>> {
        let descriptor = self.get(type_id)?;

        let ptr = if descriptor.inner_type_id() == TypeId::of::<T>() {
            descriptor.inner_ptr() as *const T
        } else {
            return Some(Err(descriptor.clone()));
        };

        // SAFETY: The TypeId of the inner type of the descriptor
        //         matches that of the type provided so we can ptr
        //         cast the variant payload reference correctly.
        let inner = unsafe { &*ptr }.clone();

        let result = TaggedType { type_id, inner };

        Some(Ok(result))
    }
}

pub(crate) trait SizeOf<O> {
    fn size_of(&self, opts: O) -> Option<NonZeroU32>;
}

impl SizeOf<()> for BuiltinTypeId {
    fn size_of(&self, (): ()) -> Option<NonZeroU32> {
        let n = match self {
            BuiltinTypeId::Float
            | BuiltinTypeId::Ellipsis
            | BuiltinTypeId::Module
            | BuiltinTypeId::Unknown
            | BuiltinTypeId::Type
            | BuiltinTypeId::Never => todo!(),

            BuiltinTypeId::Object => return None,

            BuiltinTypeId::Invalid => unreachable!(),

            BuiltinTypeId::None => 0,

            BuiltinTypeId::U8 | BuiltinTypeId::Bool | BuiltinTypeId::I8 => 1,

            BuiltinTypeId::U16 | BuiltinTypeId::I16 => 2,

            BuiltinTypeId::U32 | BuiltinTypeId::I32 => 4,

            BuiltinTypeId::Str | BuiltinTypeId::Int | BuiltinTypeId::U64 | BuiltinTypeId::I64 => 8,
        };

        NonZeroU32::new(n)
    }
}

impl SizeOf<&TypeMap> for TypeDescriptor {
    fn size_of(&self, opts: &TypeMap) -> Option<NonZeroU32> {
        match self {
            TypeDescriptor::Simple(s) => s.size_of(()),
            TypeDescriptor::Function(_) => None,
            TypeDescriptor::Class(_) => BuiltinTypeId::Int.size_of(()),
            TypeDescriptor::Generic(inner) => match inner {
                Generic::Pointer { inner: _ } => todo!(),
                Generic::Struct { inner } => NonZeroU32::new(
                    inner
                        .iter()
                        .filter_map(|ty| opts.get(*ty)?.value().size_of(opts))
                        .map(|n| n.get())
                        .sum(),
                ),
                Generic::Union { inner } => {
                    let tag_size = BuiltinTypeId::I64.size_of(())?.get();
                    let largest = inner
                        .iter()
                        .filter_map(|ty| opts.get(*ty)?.value().size_of(opts))
                        .map(|n| n.get())
                        .max()?;

                    NonZeroU32::new(tag_size + largest)
                }
            },
        }
    }
}

impl SizeOf<&GlobalContext> for TypeDescriptor {
    fn size_of(&self, opts: &GlobalContext) -> Option<NonZeroU32> {
        SizeOf::size_of(self, &opts.type_map)
    }
}
