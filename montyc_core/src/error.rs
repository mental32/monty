use std::ops::Range;

use crate::{ModuleRef, SpanRef, TypeId};

use thiserror::Error;

pub type MontyResult<T> = Result<T, MontyError>;

pub type Span = Range<usize>;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Local variable referenced before assignment.")]
    UnboundLocal {
        name: SpanRef,
        used: SpanRef,
        defined: SpanRef,
    },

    #[error("Multiple parameters in a function have the same name.")]
    DuplicateParameters,

    #[error("`return` found outside of a function.")]
    ReturnOutsideFunction(SpanRef),

    #[error("reassignment with an incompatible type.")]
    IncompatibleReassignment {
        name: SpanRef,
        first_assignment: SpanRef,
        bad_reassignment: SpanRef,
        expected_type: TypeId,
        actual_type: TypeId,
    },

    #[error("Incompatible return type.")]
    BadReturnType {
        expected: TypeId,
        actual: TypeId,
        ret_node: Span,
        def_node: Span,
    },

    #[error("Incompatible type used within a conditional.")]
    BadConditionalType { actual: TypeId, span: SpanRef },

    #[error("Incompatible return type due to implicit return.")]
    MissingReturn {
        expected: TypeId,
        def_span: SpanRef,
        ret_span: SpanRef,
    },

    #[error("Object is not callable.")]
    NotCallable { kind: TypeId, callsite: SpanRef },
    //     #[error("Failed to infer the type of a value.")]
    //     UnknownType { node: Rc<dyn AstObject> },

    // #[error("Could not find the definition of this variable.")]
    // UndefinedVariable { node: Rc<dyn AstObject> },
    #[error("Incompatible argument type.")]
    BadArgumentType {
        expected: TypeId,
        actual: TypeId,
        arg_node: SpanRef,
        def_node: SpanRef,
    },

    #[error("Generic incompatible type error.")]
    IncompatibleTypes {
        left_span: SpanRef,
        left: TypeId,
        right_span: SpanRef,
        right: TypeId,
    },

    #[error("Unimplemented binary operator for types.")]
    BadBinaryOp {
        span: SpanRef,
        left: TypeId,
        right: TypeId,
        // op: InfixOp,
    },

    #[error("Unsupported feature")]
    Unsupported { span: SpanRef, message: String },
}

#[derive(Debug, Error, derive_more::From)]
pub enum MontyError {
    #[error("An IO error.")]
    IO(std::io::Error),

    #[error("An error has occured during typechecking.")]
    TypeError { module: ModuleRef, error: TypeError },
}
