use std::ops::Range;

use crate::{ModuleRef, SpanRef, TypeId, ValueId};

use thiserror::Error;

pub type MontyResult<T> = Result<T, MontyError>;

pub type Span = Range<usize>;

type LocatedSpan = (ModuleRef, Span);

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Local variable referenced before assignment.")]
    UnboundLocal {
        name: SpanRef,
        used: SpanRef,
        defined: LocatedSpan,
    },

    #[error("Multiple parameters in a function have the same name.")]
    DuplicateParameters,

    #[error("`return` found outside of a function.")]
    ReturnOutsideFunction(LocatedSpan),

    #[error("reassignment with an incompatible type.")]
    IncompatibleReassignment {
        name: SpanRef,
        first_assignment: LocatedSpan,
        bad_reassignment: LocatedSpan,
        expected_type: TypeId,
        actual_type: TypeId,
    },

    #[error("Incompatible return type.")]
    BadReturnType {
        expected: TypeId,
        actual: TypeId,
        ret_node: LocatedSpan,
        def_node: LocatedSpan,
    },

    #[error("Incompatible type used within a conditional.")]
    BadConditionalType { actual: TypeId, span: SpanRef },

    #[error("Incompatible return type due to implicit return.")]
    MissingReturn {
        expected: TypeId,
        def_span: LocatedSpan,
        ret_span: LocatedSpan,
    },

    #[error("Object is not callable.")]
    NotCallable { kind: TypeId, callsite: LocatedSpan },

    #[error("Failed to infer the type of a value.")]
    UnknownType { sref: String },

    #[error("Could not find the definition of this variable.")]
    UndefinedVariable { sref: SpanRef },

    #[error("Incompatible argument type.")]
    BadArgumentType {
        expected: TypeId,
        actual: TypeId,
        arg_node: LocatedSpan,
        def_node: LocatedSpan,
    },

    #[error("Too little or too many arguments.")]
    SusArgumentLength {
        callsite: LocatedSpan,
        expected: usize,
        actual: usize,
    },

    #[error("Generic incompatible type error.")]
    IncompatibleTypes {
        left_span: LocatedSpan,
        left: TypeId,
        right_span: LocatedSpan,
        right: TypeId,
    },

    #[error("Unimplemented binary operator for types.")]
    BadBinaryOp {
        span: LocatedSpan,
        left: TypeId,
        right: TypeId,
        // op: InfixOp,
    },

    #[error("Access on a non-existant or invalid attribute was attempted.")]
    InvalidAttributeAccess { base: TypeId, access: LocatedSpan },

    #[error("The value was expected to be a function but it was not.")]
    NotAFunction,

    #[error("Unsupported feature")]
    Unsupported { span: LocatedSpan, message: String },
}

#[derive(Debug, Error, derive_more::From)]
pub enum MontyError {
    #[error("An interpreter exception occured.")]
    InterpreterException(),

    #[error("Any query error.")]
    QueryError,

    #[error("No value was present.")]
    None,

    #[error("The value specified is not present within the store.")]
    ValueDoesNotExist(ValueId),

    #[error("An IO error.")]
    IO(std::io::Error),

    #[error("An error has occured during typechecking.")]
    TypeError { module: ModuleRef, error: TypeError },
}
