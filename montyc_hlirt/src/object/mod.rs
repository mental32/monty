use std::num::NonZeroU64;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, Clone, Copy)]
#[repr(transparent)]
pub struct ObjectId(NonZeroU64);

impl Default for ObjectId {
    fn default() -> Self {
        Self(NonZeroU64::new(u64::MAX).unwrap())
    }
}

impl From<u64> for ObjectId {
    fn from(n: u64) -> Self {
        NonZeroU64::new(n)
            .map(Self)
            .expect("ObjectIds must be non-zero.")
    }
}

impl ObjectId {
    pub fn is_uninit(&self) -> bool {
        *self == Self::default()
    }
}

pub mod builders;
pub mod iter;
pub mod pyobject;
pub mod raw_object;
pub mod shared_object;
pub mod value;
pub mod native_tables {}

pub use self::{builders::*, iter::*, pyobject::*, raw_object::*, shared_object::*, value::*};
