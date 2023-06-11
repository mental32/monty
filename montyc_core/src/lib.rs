//! Fundamental types and traits that are used all overthe place.
//!
//! This crate mostly defines `*Ref` ID types for larget data structues
//! that are used and probably defined not in this crate.
//!
#![deny(warnings)]

#[macro_use]
pub(crate) mod macros {
    macro_rules! derive_everything {
        { $t:item } => {
            #[derive(
                Debug,
                PartialEq,
                Eq,
                PartialOrd,
                Ord,
                Hash,
                Clone,
                Copy,
                derive_more::From,
                derive_more::Into,
            )]
            $t
        };
    }

    #[macro_export]
    macro_rules! patma {
        ($n:expr, $( $pattern:pat )|+ $( if ($guard: expr) )? $(,)? in $e:expr) => {
            match $e {
                $( $pattern )|+ $( if $guard )? => Some($n),
                #[allow(warnings)]
                _ => None,
            }
        };
    }
}

pub mod ast;
pub mod codegen;
pub mod dict;
pub mod error;
pub mod func;
pub mod module;
pub mod span;
pub mod typing;
pub mod utils;
pub mod value;

pub type Qualname = Vec<String>; // TODO: turn this into some cheap u64 ID type.

pub type MapT<K, V> = ahash::AHashMap<K, V>;

pub type Rib = ahash::AHashMap<u32, TypeId>;

pub use {error::*, func::*, module::*, span::*, typing::*, value::*};
