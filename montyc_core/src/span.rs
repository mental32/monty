use std::{hash::Hash, ops::Range};

#[derive(Debug, Clone)]
pub struct SpanData {
    pub range: Range<usize>,
    pub mdoule: crate::module::ModuleRef,
    pub hash: u64,
}

/// `SpanRef`s are lightweight references to `SpanData`s
///
/// They are made up of two `u32`s the first is a group identifier
/// and the second is a distinct identifier.
///
/// Every distinct identifier uniquely refers to a span in a file somewhere,
/// the group identifiers allow for cheap string and identifier equality checks
/// e.g. all variable names `a` will have the same group identifier but different
/// distinct identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpanRef(u32, u32);


impl SpanRef {
    pub fn group(&self) -> u32 {
        self.0
    }

    pub fn distinct(&self) -> u32 {
        self.1
    }
}

impl From<(u32, u32)> for SpanRef {
    fn from((l, r): (u32, u32)) -> Self {
        Self(l, r)
    }
}

impl From<u32> for SpanRef {
    fn from(i: u32) -> Self {
        Self(i, i)
    }
}
