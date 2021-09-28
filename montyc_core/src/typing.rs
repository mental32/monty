use std::alloc::Layout;
use std::cell::RefMut;
use std::convert::{TryFrom, TryInto};
use std::iter::FromIterator;
use std::num::NonZeroU64;

use ahash::AHashSet;

use crate::span::SpanRef;
use crate::{MapT, ValueId};

derive_everything! {
    #[repr(transparent)]
    pub struct TypeId(pub NonZeroU64);
}

impl TypeId {
    pub fn is_builtin(&self) -> bool {
        u64::from(self.0) <= 255
    }
}

impl From<TypeId> for usize {
    fn from(tid: TypeId) -> Self {
        tid.0.get() as usize
    }
}

impl From<usize> for TypeId {
    fn from(n: usize) -> Self {
        Self(NonZeroU64::new(n as u64).unwrap())
    }
}

impl Default for TypeId {
    fn default() -> Self {
        match NonZeroU64::new(u64::MAX) {
            Some(n) => Self(n),
            None => unreachable!(),
        }
    }
}

macro_rules! builtins {
    ($(.$tag:literal = $name:ident  { $display:literal, $size:literal },)+) => {
        /// A ZST used to associate builtin constants with.
        pub struct TypingConstants;

        #[allow(missing_docs, non_upper_case_globals, dead_code)]
        impl TypingConstants {
            $(
                pub const $name: self::TypeId = self::TypeId(unsafe { ::std::num::NonZeroU64::new_unchecked($tag) });
            )*
        }

        /// An enum of all builtin types.
        #[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Eq, Ord, Hash)]
        #[repr(u8)]
        pub enum BuiltinType {
            $(
                #[allow(missing_docs)]
                $name = $tag
            ),*
        }

        impl self::BuiltinType {
            /// The size of this type in bytes.
            pub fn size_in_bytes(&self) -> u32 {
                match self {
                    $(
                        Self::$name => $size
                    ),*
                }
            }

            pub fn name(&self) -> &'static str {
                match self {
                    $(
                        Self::$name => $display
                    ),*
                }
            }

            /// Initialize an empty SSAMap with the builtin types.
            pub fn write_ssa(map: &mut crate::utils::SSAMap<self::Type>) {
                assert!(map.is_empty());

                $(
                    map.skip_to_nth(Self::$name as usize).unwrap();

                    let bltn = map.insert::<usize, self::Type>(Type {
                        kind: self::PythonType::Builtin { inner: Self::$name },
                        properties: Default::default(),
                    });

                    assert_eq!(bltn, $tag, "builtin tag mismatch! builtin was specified with the constant {} but the SSAMap allocated it with {}", $tag, bltn);
                )*

                map.skip_to_nth(256).expect("SSAMap<TypeId, Type> has more than 255 entries!");
            }
        }

        impl ::std::fmt::Display for self::BuiltinType {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self {
                    $(
                        Self::$name => write!(f, concat!("{{", $display, "}}")),
                    )*
                }
            }
        }
    };
}

builtins!(
    // Abstract builtin types.
    .1 = Int { "int", 8 },
    .2 = Float { "float", 8 },
    .3 = Str { "str", 8 },
    .4 = Bool { "bool", 8 },
    .5 = None { "None", 8 },
    .6 = Ellipsis { "ellipsis", 0 },
    .7 = Module { "module", 8 },
    .8 = Unknown { "unknown", 0 },
    .9 = Type { "type", 8 },
    .10 = Object { "object", 8 },
    .11 = TSelf { "self", 8 },
    .12 = UntypedFunc { "function", 8 },
    .13 = UntypedTuple { "tuple", 8 },
    .14 = AnyType { "Any", 8 },
    .15 = Bytes { "bytes", 8 },
    // Primitive, lower-level types.
    .100 = U8 { "u8", 1 },
    .101 = U16 { "u16", 2 },
    .102 = U32 { "u32", 4 },
    .103 = U64 { "u64", 8 },
    // signed variants
    .104 = I8 { "i8", 1 },
    .105 = I16 { "i16", 2 },
    .106 = I32 { "i32", 4 },
    .107 = I64 { "i64", 8 },
    // a "never" type.
    .255 = Never { "never", 0 },
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallableKind {
    DefFn,
    Lambda,
}

/// A native Python type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum PythonType {
    /// Special type indicating that a function never returns.
    NoReturn,

    /// Special type indicating an unconstrained type.
    ///
    /// * Every type is compatible with Any.
    /// * Any is compatible with every type.
    ///
    Any,

    /// A tuple type.
    ///
    /// `Tuple[X, Y]` is the type of a tuple of two items the first item of type X and the second type Y.
    Tuple { members: Option<Vec<TypeId>> },

    /// A list type.
    List { inner: TypeId },

    /// Union type; `Union[X, Y]` means either X or Y.
    Union { members: Option<Vec<TypeId>> },

    /// A variable annotated with `Type[C]` may accept values that are classes themselves.
    Type { of: TypeId },

    /// The logical opposite of `Type[c]` used for lazilly resolving types in annotations when checking.
    Instance { of: crate::value::ValueId },

    /// Parameters for generic types as well as for generic function definitions.
    TypeVar { name: SpanRef },

    /// A callable object with a signature of `(params*) -> ret`
    Callable {
        params: Option<Vec<TypeId>>,
        ret: TypeId,
        // kind: CallableKind,
    },

    /// Used for generic signatures, parameterized with `TypeVar`s.
    Generic { args: Option<Vec<TypeId>> },

    /// Any builtin primitive type.
    Builtin { inner: BuiltinType },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyValue {
    Id(ValueId),
    Builtin(TypeId, String),
}

/// Properties are the associated objects on some class or instance aka attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct Property {
    pub type_id: TypeId,
    pub value: PropertyValue,
}

impl Property {
    pub fn new(type_id: TypeId, value: PropertyValue) -> Self {
        Self { type_id, value }
    }
}

/// Represents information about a type.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Type {
    pub kind: PythonType,
    pub properties: MapT<String, Property>,
}

impl From<PythonType> for Type {
    fn from(kind: PythonType) -> Self {
        Self {
            kind,
            properties: Default::default(),
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

pub struct LazyPropertyAccess<'a> {
    type_id: TypeId,
    field: &'a str,
}

impl LazyPropertyAccess<'_> {
    #[inline]
    pub fn get(&self, tcx: &dyn TypingContext) -> Option<Property> {
        let klass_t = tcx.insert(PythonType::Type { of: self.type_id }.into());

        tcx.get_type_repr(klass_t)?
            .properties
            .get(self.field)
            .cloned()
    }
}

/// A `TypeId` with a bound reference to a `TypingContext`.
#[derive(Debug)]
pub struct LocalTypeId {
    /// The `TypeId` of the type.
    pub type_id: TypeId,

    /// The `Type` descriptor.
    pub ty: Type,
}

impl LocalTypeId {
    /// Get the Python type associated with this type id.
    #[inline]
    pub fn as_python_type(&self) -> &PythonType {
        &self.ty.kind
    }

    /// Access an associated type property.
    #[inline]
    pub fn get_property<'a>(&self, name: &'a str) -> Result<Property, LazyPropertyAccess<'a>> {
        self.ty
            .properties
            .get(name)
            .cloned()
            .ok_or_else(|| LazyPropertyAccess {
                type_id: self.type_id,
                field: name,
            })
    }
}

const I64_LAYOUT: Layout = Layout::new::<i64>();

/// Given an iterator of `std::alloc::Layout`s compute a well aligned layout that can hold all of the input layouts.
pub fn calculate_layout(fields: impl Iterator<Item = Layout>) -> (Layout, Vec<i32>) {
    // SAFETY: The parameters privded to from_size_align satisfy the safety invariants.
    let mut layout = unsafe { Layout::from_size_align_unchecked(0, 1) };
    let mut offsets = Vec::with_capacity(16);

    for new_layout in fields {
        let (new_layout, offset) = layout.extend(new_layout).expect("arithmetic overflow.");
        let offset = i32::try_from(offset).unwrap();

        offsets.push(offset);

        layout = new_layout;
    }

    let layout = layout.pad_to_align();

    assert_ne!(layout.size(), 0);

    (layout, offsets)
}

pub enum UnifyFailure {
    NotCallable,
    UnequalArity(usize, usize),
    BadArgumentTypes(Vec<(usize, TypeId, TypeId)>),
}

/// Implemented by providers of typing behaviour.
pub trait TypingContext {
    /// Create and initialize a fresh `TypingContext` with all of the `BuiltinType`s registered.
    fn initialized() -> Self
    where
        Self: Sized;

    /// Cast `&self` to a `&dyn TypingContext`
    fn as_dyn_tcx(&self) -> &dyn TypingContext;

    /// Convert a `TypeId` to a `LocalTypeId<'tcx>` if it belongs in this type context.
    fn localize(&self, type_id: TypeId) -> Option<LocalTypeId>;

    /// Insert a new `Type` and return its assigned `TypeId`, if the type is already present an already existing TypeId may be returned.
    fn insert(&self, ty: Type) -> TypeId;

    /// Insert the following property in this types Type representation.
    fn insert_property(&self, base: TypeId, name: String, property: Property) -> Option<Property>;

    /// Perform a property access on the base type. if it fails then try again on `Type { of: base_t }`
    fn get_property(&self, base_t: TypeId, name: &str) -> Option<Property> {
        let object_type_repr = self.localize(base_t).unwrap();

        match object_type_repr.get_property(name) {
            Ok(p) => Some(p),
            Err(klass) => klass.get(self.as_dyn_tcx()),
        }
    }

    /// Get a reference to the `Type` representation.
    fn get_type_repr(&self, type_id: TypeId) -> Option<Type>;

    /// Get a mutable reference to the `Type` representation.
    fn get_type_repr_mut<'tcx>(&'tcx self, type_id: TypeId) -> Option<RefMut<'tcx, Type>>;

    /// Get the `PythonType` representation of this type.
    fn get_python_type_of(&self, type_id: TypeId) -> Option<PythonType>;

    /// Given a type id, return a human-readable string repr.
    fn display_type(
        &self,
        type_id: TypeId,
        type_id_of_val: &dyn Fn(crate::value::ValueId) -> Option<TypeId>,
    ) -> Option<String>;

    /// Compute the `std::alloc::Layout` for some `TypeId`
    #[inline]
    fn layout_of(&self, type_id: TypeId) -> Layout {
        let ty = self.get_python_type_of(type_id).unwrap();

        match &ty {
            PythonType::NoReturn
            | PythonType::Instance { .. }
            | PythonType::Type { .. }
            | PythonType::TypeVar { .. }
            | PythonType::List { .. }
            | PythonType::Generic { .. } => {
                todo!()
            }

            PythonType::Any => I64_LAYOUT,

            PythonType::Tuple { members } => {
                let members = members.clone().unwrap();
                let members = members
                    .iter()
                    .map(|tid| self.layout_of(*tid))
                    .collect::<Vec<_>>();

                let mut it = members.into_iter();

                let (layout, _) = calculate_layout(&mut it);

                layout
            }

            PythonType::Union { members } => {
                let members = members.clone().unwrap_or_default();
                let member_layouts = members
                    .iter()
                    .map(|tid| self.layout_of(*tid))
                    .collect::<Vec<_>>();

                member_layouts
                    .iter()
                    .max_by(|left, right| left.size().cmp(&right.size()))
                    .unwrap()
                    .clone()
            }

            PythonType::Callable { .. } => I64_LAYOUT,

            PythonType::Builtin { inner } => {
                let size = inner.size_in_bytes();
                assert_ne!(size, 0, "{:?}", inner);

                Layout::array::<u8>(size as usize).unwrap().pad_to_align()
            }
        }
    }

    /// Create a tuple from a vector of `TypeId`s.
    #[inline]
    fn tuple(&self, elements: Vec<TypeId>) -> TypeId {
        let members = if elements.is_empty() {
            None
        } else {
            Some(elements)
        };

        let tuple_type_id = self.insert(
            PythonType::Tuple {
                members: members.clone(),
            }
            .into(),
        );

        let is_empty = members.is_none();
        let members_as_union = match members {
            Some(_) => self.insert(PythonType::Union { members }.into()),
            None => TypingConstants::Never,
        };

        {
            let type_id = self.callable(
                Some(vec![tuple_type_id, TypingConstants::Int]),
                members_as_union,
            );

            let getitem_property = Property::new(
                type_id,
                PropertyValue::Builtin(tuple_type_id, "__getitem__".into()),
            );

            self.insert_property(tuple_type_id, "__getitem__".into(), getitem_property);
        }

        {
            let type_id = self.callable(
                Some(vec![
                    tuple_type_id,
                    TypingConstants::Int,
                    if is_empty {
                        TypingConstants::AnyType
                    } else {
                        members_as_union
                    },
                ]),
                if is_empty {
                    TypingConstants::Never
                } else {
                    members_as_union
                },
            );

            let setitem_property = Property::new(
                type_id,
                PropertyValue::Builtin(tuple_type_id, "__setitem__".into()),
            );

            self.insert_property(tuple_type_id, "__setitem__".into(), setitem_property);
        }

        tuple_type_id
    }

    #[inline]
    fn list(&self, inner: TypeId) -> TypeId {
        self.insert(Type::from(PythonType::List { inner }))
    }

    /// Create a callable type from a signature.
    #[inline]
    fn callable(&self, args: Option<Vec<TypeId>>, ret: TypeId) -> TypeId {
        self.insert(Type {
            kind: PythonType::Callable { params: args, ret },

            properties: Default::default(),
        })
    }

    /// Check if a type is a tuple.
    #[inline]
    fn is_tuple(&self, type_id: TypeId) -> bool {
        type_id == TypingConstants::UntypedTuple || {
            self.get_python_type_of(type_id)
                .map(|ty| matches!(ty, PythonType::Tuple { .. }))
                .unwrap_or(false)
        }
    }

    /// Get the size of a type (in bytes.)
    #[inline]
    fn size_of(&self, type_id: TypeId) -> u32 {
        self.layout_of(type_id).size().try_into().unwrap()
    }

    /// Construct a union type of `left` and `right`
    #[inline]
    fn make_union(&self, left: TypeId, right: TypeId) -> Result<TypeId, TypeId> {
        let lhs = self.get_type_repr(left).map(|t| (left, t)).ok_or(left)?;
        let rhs = self.get_type_repr(right).map(|t| (right, t)).ok_or(right)?;

        let mut members = AHashSet::with_capacity(2);

        for (tid, ty) in [lhs, rhs] {
            if let PythonType::Union { members: rest } = &ty.kind {
                if let Some(rest) = rest {
                    members.extend(rest.iter());
                }
            } else {
                members.insert(tid);
            }
        }

        let members = if members.is_empty() {
            None
        } else {
            Some(Vec::from_iter(members.into_iter()))
        };

        let kind = PythonType::Union { members };

        Ok(self.insert(Type {
            kind,
            properties: Default::default(),
        }))
    }

    /// Try to unify some callable type with the args and ret types.
    #[inline]
    fn unify_func(
        &self,
        func_t: TypeId,
        args: &[TypeId],
        ret: TypeId,
    ) -> Result<PythonType, UnifyFailure> {
        let func_t = self.get_type_repr(func_t).unwrap();

        let (params, f_ret) = match func_t.kind.clone() {
            PythonType::Callable { params: args, ret } => (args.unwrap_or_default(), ret),
            _ => return Err(UnifyFailure::NotCallable),
        };

        if params.len() != args.len() || (ret != TypingConstants::Unknown && f_ret != ret) {
            return Err(UnifyFailure::UnequalArity(params.len(), args.len()));
        }

        let (params, args) = match (params.as_slice(), args) {
            ([TypingConstants::TSelf, params @ ..], [_, args @ ..]) => (params, args),
            (params, args) => (params, args),
        };

        let mut errors = Vec::with_capacity(params.len());

        for (ix, (l, r)) in params.iter().cloned().zip(args.iter().cloned()).enumerate() {
            if (l != TypingConstants::Unknown) && (l != r) {
                errors.push((ix, l, r))
            }
        }

        if errors.is_empty() {
            Ok(func_t.kind.clone())
        } else {
            Err(UnifyFailure::BadArgumentTypes(errors))
        }
    }
}
