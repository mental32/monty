use std::ops::Index;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct UnqiueIndex<A, B>((A, B));
