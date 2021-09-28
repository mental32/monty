use crate::*;

/// A `hlir::Function` is a function object defined in a source file somewhere, tagged and typed.
#[derive(Debug, Clone)]
pub struct Function {
    /// The index of the function value.
    pub value_id: TaggedValueId<FUNCTION>,

    /// The type signature of the function.
    pub type_id: TypeId,

    /// The module the function was defined in.
    pub mref: ModuleRef,

    /// The name of the function.
    pub name: SpanRef,
}
