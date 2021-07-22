//! Fundamental types that are shared and used all over the place.
//!
//! Most resources typically have at least two type representations:
//!
//! * A "fat", complete, and not-cheap-to-clone representation (this one typically holds all the metadata)
//! * A "cheap", small, and unambiguous reference to the more complete
//!   information that you can throw around copies of everywhere.
//!
//! The naming scheme is typically a regular name for your "fat" representations
//! and for "cheap" representations its the name with a "Ref" suffix i.e. `Module` and `ModuleRef`
//! or `Span` and `SpanRef`
//!

pub mod error;
pub mod span;
pub mod utils;

pub mod module {
    use std::convert::{TryFrom, TryInto};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct ModuleRef(pub u32);

    impl TryFrom<ModuleRef> for usize {
        type Error = std::num::TryFromIntError;

        fn try_from(ModuleRef(n): ModuleRef) -> Result<Self, Self::Error> {
            n.try_into()
        }
    }

    impl TryFrom<usize> for ModuleRef {
        type Error = std::num::TryFromIntError;

        fn try_from(n: usize) -> Result<Self, Self::Error> {
            Ok(Self(n.try_into()?))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct AstNodeId(pub u32);

pub mod types {
    use std::convert::{TryFrom, TryInto};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct TypeId(pub u32);

    impl TypeId {
        pub fn is_builtin(&self) -> bool {
            self.0 <= 255
        }
    }

    impl Default for TypeId {
        fn default() -> Self {
            Self(u32::MAX)
        }
    }

    impl TryFrom<TypeId> for usize {
        type Error = std::num::TryFromIntError;

        fn try_from(TypeId(n): TypeId) -> Result<Self, Self::Error> {
            n.try_into()
        }
    }

    impl TryFrom<usize> for TypeId {
        type Error = std::num::TryFromIntError;

        fn try_from(n: usize) -> Result<Self, Self::Error> {
            Ok(Self(n.try_into()?))
        }
    }
}

pub mod value {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
    pub struct ValueId(usize);

    impl From<ValueId> for usize {
        fn from(ValueId(n): ValueId) -> Self {
            n
        }
    }

    impl From<usize> for ValueId {
        fn from(n: usize) -> Self {
            Self(n)
        }
    }
}

#[macro_export]
macro_rules! patma {
    ($n:expr => $( $pattern:pat )|+ $( if ($guard: expr) )? $(,)? in $e:expr) => {
        match $e {
            $( $pattern )|+ $( if $guard )? => Some($n),
            #[allow(warnings)]
            _ => None,
        }
    };
}

pub use {error::*, module::*, span::*, types::*, value::*};
