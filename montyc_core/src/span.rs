use std::{hash::Hash, ops::Range};

#[derive(Debug, Clone)]
pub struct SpanData {
    pub range: Range<usize>,
    pub module: crate::module::ModuleRef,
    pub hash: u64,
}

derive_everything! {
    /// `SpanRef`s are lightweight references to `SpanData`s
    ///
    /// They are made up of two `u32`s the first is a group identifier
    /// and the second is a distinct identifier.
    ///
    /// Every distinct identifier uniquely refers to a span in a file somewhere,
    /// the group identifiers allow for cheap string and identifier equality checks
    /// e.g. all variable names `a` will have the same group identifier but different
    /// distinct identifiers.
    ///
    pub struct SpanRef(u32, u32);
}

impl SpanRef {
    /// Get the group identifier of the span ref.
    ///
    /// Multiple spans with the same hash are considered to be in the same group.
    ///
    #[inline]
    pub fn group(&self) -> u32 {
        self.0
    }

    /// Get the distinct identifier of the span ref.
    ///
    /// This is a unique identifier for the span in the file.
    ///
    #[inline]
    pub fn distinct(&self) -> u32 {
        self.1
    }
}
