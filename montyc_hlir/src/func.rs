use ahash::AHashSet;

use montyc_core::{ModuleRef, SpanRef, TypeId};

use crate::{flatcode::FlatSeq, value_store::ValueGraphIx};

/// A `hlir::Function` is a `FlatSeq` wrapped with extra metadata like type information.
#[derive(Debug, Clone)]
pub struct Function {
    /// The type signature of the function.
    pub type_id: TypeId,

    /// The flatcode sequence of this function.
    pub code: FlatSeq,

    /// References to global data, i.e. other functions, static/const data, etc...
    pub refs: AHashSet<ValueGraphIx>,

    /// The module the function was defined in.
    pub mref: ModuleRef,

    /// The name of the function.
    pub name: SpanRef,

    /// The index of the function value.
    pub value_ix: ValueGraphIx,
}
