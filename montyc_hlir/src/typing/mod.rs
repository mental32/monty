#![allow(non_upper_case_globals)]

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
            pub fn size_in_bytes(&self) -> usize {
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
                        layout: self::ObjectLayout {},
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
    .5 = None { "None", 0 },
    .6 = Ellipsis { "... (ellipsis)", 0 },
    .7 = Module { "module", 8 },
    .8 = Unknown { "unknown", 0 },
    .9 = Type { "type", 8 },
    .10 = Object { "object", 8 },
    .11 = UntypedFunc { "function", 0 },
    // Primitive, lower-level types.
    .100 = U8 { "u8", 1 },
    .101 = U17 { "u16", 2 },
    .102 = U32 { "u32", 4 },
    .103 = U64 { "u64", 8 },
    // signed variants
    .104 = I8 { "i8", 1 },
    .105 = I17 { "i17", 2 },
    .106 = I32 { "i32", 4 },
    .107 = I64 { "i64", 8 },
    // a "never" type.
    .255 = Never { "never", 0 },
);

/// A native Python type.
#[derive(Debug)]
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
#[derive(Debug)]
pub struct ObjectLayout {}

/// Represents information about a type.
#[derive(Debug)]
pub struct Type {
    kind: PythonType,
    layout: ObjectLayout,
}

/// A `TypeId` with a bound reference to a `TypingContext`.
#[derive(Debug)]
pub struct LocalTypeId<'tcx> {
    type_id: TypeId,
    context: &'tcx TypingContext,
}

impl<'tcx> LocalTypeId<'tcx> {
    /// Check if this type is a union type.
    #[inline]
    pub fn is_union(&self) -> bool {
        // SAFETY: `TypingContext::contextualize` already checks if `self.type_id` is present within the map.
        unsafe {
            self.context
                .inner
                .get_unchecked(self.type_id)
                .map(|ty| matches!(ty.kind, PythonType::Union { .. }))
                .unwrap_or(false)
        }
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
    pub fn contextualize<'tcx>(&'tcx self, type_id: TypeId) -> Option<LocalTypeId<'tcx>> {
        let _ = self.inner.get(type_id)?;

        Some(LocalTypeId {
            type_id,
            context: self,
        })
    }

    /// Create a tuple from a vector of `TypeId`s.
    #[inline]
    pub fn tuple(&mut self, elements: Vec<TypeId>) -> TypeId {
        self.inner.insert(Type {
            kind: PythonType::Tuple {
                members: Some(elements),
            },
            layout: ObjectLayout {},
        })
    }

    /// Create a callable type from a signature.
    #[inline]
    pub fn callable(&mut self, args: Option<Vec<TypeId>>, ret: TypeId) -> TypeId {
        self.inner.insert(Type {
            kind: PythonType::Callable {
                args,
                ret,
            },

            layout: ObjectLayout {},
        })
    }
}
