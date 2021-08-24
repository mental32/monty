use montyc_core::TypeId;

use crate::flatcode::FlatSeq;

/// A `hlir::Function` is a `FlatSeq` wrapped with extra metadata like type information.
#[derive(Debug)]
pub struct Function {
    /// The type signature of the function.
    pub type_id: TypeId,

    code: FlatSeq,
}
