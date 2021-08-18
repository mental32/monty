//! HLIR to represent executable code in a function.
//!

use montyc_core::TypeId;

/// A HLIR function which represents a single Python function lowered to a bytecode-like-form.
#[derive(Debug, Default)]
pub struct Function {
    /// The type signature of the function.
    pub type_id: TypeId,
}
