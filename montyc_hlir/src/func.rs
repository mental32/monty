use montyc_core::TypeId;

use crate::flatcode::FlatSeq;

/// A `hlir::Function` is a `FlatSeq` wrapped with extra metadata like type information.
#[derive(Debug)]
pub struct Function {
    /// The type signature of the function.
    pub type_id: TypeId,

    /// The flatcode sequence of this function.
    pub code: FlatSeq,

    /// The types for the values in the code.
    pub code_value_types: Vec<TypeId>,
}
