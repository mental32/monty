use crate::SpanRef;

derive_everything! {
    #[derive(Default)]
    pub struct ValueId(pub u32);
}

impl From<petgraph::graph::NodeIndex<u32>> for ValueId {
    fn from(ix: petgraph::graph::NodeIndex<u32>) -> Self {
        Self(ix.index() as u32)
    }
}

impl From<ValueId> for petgraph::graph::NodeIndex<u32> {
    fn from(id: ValueId) -> Self {
        petgraph::graph::NodeIndex::from(id.0)
    }
}

pub const FUNCTION: usize = 0xfacade;
pub const CLASS: usize = 0xc1a55;
pub const MODULE: usize = 0xab0de;
pub const OBJECT: usize = 0xba5ed;

derive_everything! {
    /// Like a `ValueId` but with a const generic tag for added type safety.
    #[repr(transparent)]
    pub struct TaggedValueId<const TAG: usize>(pub ValueId);
}

impl TaggedValueId<{ FUNCTION }> {
    pub fn func(v: ValueId) -> Self {
        Self(v)
    }
}

impl TaggedValueId<{ OBJECT }> {
    pub fn obj(v: ValueId) -> Self {
        Self(v)
    }
}

impl TaggedValueId<{ MODULE }> {
    pub fn module(v: ValueId) -> Self {
        Self(v)
    }
}

impl TaggedValueId<{ CLASS }> {
    pub fn class(v: ValueId) -> Self {
        Self(v)
    }
}

pub type CallableSignature<T> = Option<(Option<SpanRef>, Box<[(SpanRef, Option<T>)]>)>;

#[derive(Debug, Default, Clone)]
pub struct Value {
    pub properties: Vec<()>,
}
