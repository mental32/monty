//! Typing-related and management of types logic here.

#![allow(non_upper_case_globals, warnings)]

use std::iter::FromIterator;

use ahash::AHashSet;
use montyc_core::{utils::SSAMap, SpanRef, TypeId};

macro_rules! builtins {
    ($(.$tag:literal = $name:ident  { $display:literal, $size:literal },)+) => {
        impl self::TypingContext {
            $(
                #[allow(missing_docs)]
                pub const $name: ::montyc_core::types::TypeId = ::montyc_core::types::TypeId($tag);
            )*
        }

        /// An enum of all builtin types.
        #[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Eq, Ord)]
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

            pub(in self) fn write_ssa(map: &mut ::montyc_core::utils::SSAMap<::montyc_core::TypeId, self::Type>) {
                $(
                    map.skip_to_nth(Self::$name as usize).unwrap();

                    let bltn = map.insert(Type {
                        kind: self::PythonType::Builtin { inner: Self::$name },
                        layout: self::ObjectLayout::default(),
                    });

                    assert_eq!(bltn, self::TypingContext::$name);
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
    .0 = Invalid { "invalid type", 0 },
    .1 = Int { "int", 8 },
    .2 = Float { "float", 8 },
    .3 = Str { "str", 8 },
    .4 = Bool { "bool", 8 },
    .5 = None { "None", 8 },
    .6 = Ellipsis { "... (ellipsis)", 0 },
    .7 = Module { "module", 8 },
    .8 = Unknown { "unknown", 0 },
    .9 = Type { "type", 8 },
    .10 = Object { "object", 8 },
    .11 = TSelf { "self", 8 },
    .12 = UntypedFunc { "function", 8 },
    .13 = UntypedTuple { "tuple", 8 },
    .14 = AnyType { "Any", 8 },
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

/// A native Python type.
#[derive(Debug, Clone)]
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

    /// Union type; `Union[X, Y]` means either X or Y.
    Union { members: Option<Vec<TypeId>> },

    /// A variable annotated with `Type[C]` may accept values that are classes themselves.
    Type { of: TypeId },

    /// Parameters for generic types as well as for generic function definitions.
    TypeVar { name: SpanRef },

    /// A callable object with a signature of `(args*) -> ret`
    Callable {
        args: Option<Vec<TypeId>>,
        ret: TypeId,
    },

    /// Used for generic signatures, parameterized with `TypeVar`s.
    Generic { args: Option<Vec<TypeId>> },

    /// Any builtin primitive type.
    Builtin { inner: BuiltinType },
}

/// Defines an objects in-memory layout.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct ObjectLayout {
    pub members: Vec<TypeId>,
    pub linkage: Option<cranelift_module::Linkage>,
    pub callcov: Option<cranelift_codegen::isa::CallConv>,
}

/// Represents information about a type.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Type {
    pub kind: PythonType,
    pub layout: ObjectLayout,
}

/// A `TypeId` with a bound reference to a `TypingContext`.
#[derive(Debug)]
pub struct LocalTypeId<'tcx> {
    /// The `TypeId` of the type.
    pub type_id: TypeId,
    context: &'tcx TypingContext,
}

impl<'tcx> LocalTypeId<'tcx> {
    #[inline]
    fn get(&self) -> Option<&Type> {
        // SAFETY: `TypingContext::get` already checks if `self.type_id` is present within the map.
        unsafe { self.context.inner.get_unchecked(self.type_id) }
    }

    /// Check if this type is a union type.
    #[inline]
    pub fn is_union(&self) -> bool {
        self.get()
            .map(|ty| matches!(ty.kind, PythonType::Union { .. }))
            .unwrap_or(false)
    }

    /// Get the Python type associated with this type id.
    #[inline]
    pub fn as_python_type(&self) -> &PythonType {
        let type_desc = self.get().unwrap();
        &type_desc.kind
    }
}

/// A global registar for types.
#[derive(Debug, Default)]
pub struct TypingContext {
    inner: SSAMap<TypeId, Type>,
}

impl TypingContext {
    /// Create and initialize a fresh `TypingContext` with all of the `BuiltinType`s registered.
    #[inline]
    pub fn initialized() -> Self {
        let mut inner = SSAMap::default();

        BuiltinType::write_ssa(&mut inner);

        Self { inner }
    }

    /// Convert a `TypeId` to a `LocalTypeId<'tcx>` if it belongs in this type context.
    #[inline]
    pub fn get<'tcx>(&'tcx self, type_id: TypeId) -> Option<LocalTypeId<'tcx>> {
        let _ = self.inner.get(type_id)?;

        Some(LocalTypeId {
            type_id,
            context: self,
        })
    }

    /// Get a mutable reference to the `Type` representation.
    #[inline]
    pub fn get_type_mut<'tcx>(&'tcx mut self, type_id: TypeId) -> Option<&'tcx mut Type> {
        self.inner.get_mut(type_id)
    }
}

impl TypingContext {
    /// Create a tuple from a vector of `TypeId`s.
    #[inline]
    pub fn tuple(&mut self, elements: Vec<TypeId>) -> TypeId {
        self.inner.insert(Type {
            kind: PythonType::Tuple {
                members: Some(elements),
            },
            layout: ObjectLayout::default(),
        })
    }

    /// Create a callable type from a signature.
    #[inline]
    pub fn callable(&mut self, args: Option<Vec<TypeId>>, ret: TypeId) -> TypeId {
        self.inner.insert(Type {
            kind: PythonType::Callable { args, ret },

            layout: ObjectLayout::default(),
        })
    }

    /// Create a typing.Union from an interator of type ids.
    #[inline]
    pub fn union_type(&mut self, members: &[TypeId]) -> TypeId {
        let members = match members {
            [] => None,
            types => Some(types.to_owned()),
        };

        self.inner.insert(Type {
            kind: PythonType::Union { members },
            layout: ObjectLayout::default(),
        })
    }

    /// Check if a type is a tuple.
    #[inline]
    pub fn is_tuple(&self, type_id: TypeId) -> bool {
        type_id == Self::UntypedTuple || {
            self.inner
                .get(type_id)
                .map(|ty| matches!(ty.kind, PythonType::Tuple { .. }))
                .unwrap_or(false)
        }
    }

    /// Get the size of a type (in bytes.)
    #[inline]
    pub fn size_of(&self, type_id: TypeId) -> u32 {
        let ty = self.inner.get(type_id).unwrap();

        match &ty.kind {
            PythonType::NoReturn => todo!(),
            PythonType::Any => todo!(),
            PythonType::Tuple { members } => todo!(),
            PythonType::Union { members } => todo!(),
            PythonType::Type { of } => todo!(),
            PythonType::TypeVar { name } => todo!(),
            PythonType::Callable { args, ret } => todo!(),
            PythonType::Generic { args } => todo!(),
            PythonType::Builtin { inner } => inner.size_in_bytes(),
        }
    }

    /// Construct a union type of `left` and `right`
    #[inline]
    pub fn make_union(&mut self, left: TypeId, right: TypeId) -> TypeId {
        let lhs = match self.inner.get(left).unwrap().kind.clone() {
            PythonType::Union { members } => Ok(members.unwrap_or_default()),
            _ => Err(left),
        };

        let rhs = match self.inner.get(right).unwrap().kind.clone() {
            PythonType::Union { members } => Ok(members.unwrap_or_default()),
            _ => Err(right),
        };

        let it: Box<dyn Iterator<Item = TypeId>> = match (lhs, rhs) {
            (Ok(a), Ok(b)) => Box::new(a.into_iter().chain(b.into_iter())),

            (Err(b), Ok(a)) | (Ok(a), Err(b)) => Box::new(a.into_iter().chain(Some(b).into_iter())),

            (Err(a), Err(b)) => Box::new(vec![a, b].into_iter()),
        };

        let members: AHashSet<TypeId> = AHashSet::from_iter(it);

        let layout = ObjectLayout::default();
        let kind = PythonType::Union {
            members: if members.is_empty() {
                None
            } else {
                Some(Vec::from_iter(members.into_iter()))
            },
        };

        self.inner.insert(Type { kind, layout })
    }

    /// Try to unify some callable type with the args and ret types.
    #[inline]
    pub fn unify_func(&self, func_t: TypeId, args: &[TypeId], ret: TypeId) -> Option<PythonType> {
        let func_t = self.inner.get(func_t).unwrap();

        let (params, f_ret) = match func_t.kind.clone() {
            PythonType::Callable { args, ret } => (args.unwrap_or_default(), ret),
            _ => return None,
        };

        if params.len() != args.len() || (ret != Self::Unknown && f_ret != ret) {
            return None;
        }

        let (params, args) = match (params.as_slice(), args) {
            ([Self::TSelf, params @ ..], [_, args @ ..]) => (params, args),
            (params, args) => (params, args),
        };

        if params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .all(|(l, r)| (l == Self::Unknown) || (l == r))
        {
            Some(func_t.kind.clone())
        } else {
            None
        }
    }
}
