use std::rc::Rc;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use thiserror::Error;

use crate::{ast::{expr::{Expr, InfixOp}, funcdef::FunctionDef, retrn::Return}, prelude::*};


#[derive(Debug, Error, Clone)]
pub enum MontyError {
    #[error("Local variable referenced before assignment.")]
    UnboundLocal {
        name: SpanRef,
        assign: Span,
        used: Span,
    },

    #[error("Reassigned a name with an incompatible type.")]
    IncompatibleReassignment {
        name: SpanRef,
        first_assigned: Span,
        incorrectly_reassigned: Span,
        expected: LocalTypeId,
        actual: LocalTypeId,
    },

    #[error("Incompatible return type.")]
    BadReturnType {
        expected: LocalTypeId,
        actual: LocalTypeId,
        ret_node: Rc<Spanned<Return>>,
        def_node: Rc<Spanned<FunctionDef>>,
    },

    #[error("Incompatible type used within a conditional.")]
    BadConditionalType { actual: LocalTypeId, span: Span },

    #[error("Incompatible return type due to implicit return.")]
    MissingReturn {
        expected: LocalTypeId,
        def_span: Span,
        ret_span: Span,
    },

    #[error("Object is not callable.")]
    NotCallable { kind: LocalTypeId, callsite: Span },

    #[error("Failed to infer the type of a value.")]
    UnknownType { node: Rc<dyn AstObject> },

    #[error("Could not find the definition of this variable.")]
    UndefinedVariable { node: Rc<dyn AstObject> },

    #[error("Incompatible argument type.")]
    BadArgumentType {
        expected: LocalTypeId,
        actual: LocalTypeId,
        arg_node: Rc<Spanned<Expr>>,
        def_node: Rc<Spanned<FunctionDef>>,
    },

    #[error("Generic incompatible type error.")]
    IncompatibleTypes {
        left_span: Span,
        left: LocalTypeId,
        right_span: Span,
        right: LocalTypeId,
    },

    #[error("Unimplemented binary operator for types.")]
    BadBinaryOp {
        span: Span,
        left: LocalTypeId,
        right: LocalTypeId,
        op: InfixOp,
    },

    #[error("Unsupported feature")]
    Unsupported { span: Span, message: String },
}

pub trait CompilerError {
    type Success;

    fn unwrap_or_compiler_error<'a>(self, ctx: &LocalContext<'a>) -> Self::Success;
}

impl CompilerError for Option<LocalTypeId> {
    type Success = LocalTypeId;

    fn unwrap_or_compiler_error<'a>(self, ctx: &LocalContext<'a>) -> Self::Success {
        match self {
            Some(t) => t,
            None => {
                ctx.exit_with_error(MontyError::UnknownType {
                    node: ctx.this.as_ref().unwrap().clone(),
                });
            }
        }
    }
}

impl<T> CompilerError for std::result::Result<T, MontyError> {
    type Success = T;

    fn unwrap_or_compiler_error<'b>(self, ctx: &LocalContext<'b>) -> Self::Success {
        match self {
            Ok(t) => t,
            Err(err) => ctx.exit_with_error(err),
        }
    }
}

impl MontyError {
    pub fn into_diagnostic(
        self,
        ctx: &LocalContext<'_>,
    ) -> codespan_reporting::diagnostic::Diagnostic<u64> {
        macro_rules! fmt_type {
            ($t:expr) => {{
                $crate::fmt::Formattable {
                    gctx: ctx.global_context,
                    inner: $t,
                }
            }};
        }

        match self {
            MontyError::BadReturnType {
                expected,
                actual,
                ret_node,
                def_node,
            } => {
                let mut labels = vec![];

                let ret_span = ret_node.span().unwrap();

                let primary = match &ret_node.inner.value {
                    Some(value) => Label::primary(ctx.module_ref.hash(), value.span().unwrap_or(ret_span))
                        .with_message("but value of this type is returned instead."),

                    None => Label::primary(ctx.module_ref.hash(), ret_span).with_message("`None` is returned here."),
                };

                labels.push(primary);

                let def = Label::secondary(
                    ctx.module_ref.hash(),
                    def_node
                        .inner
                        .returns
                        .clone()
                        .and_then(|ret| ret.span())
                        .unwrap_or(def_node.span.clone()),
                )
                .with_message(if def_node.inner.returns.is_some() {
                    format!(
                        "function defined here supposedly returning a value of {}.",
                        fmt_type!(expected)
                    )
                } else {
                    "function defined here is expected to return `None`".to_string()
                });

                labels.push(def);

                Diagnostic::error()
                    .with_message("incomaptible return type for function.")
                    .with_labels(labels)
                    .with_notes(vec![
                        format!("expected: {}", fmt_type!(expected)),
                        format!("actual: {}", fmt_type!(actual)),
                    ])
            }

            MontyError::MissingReturn {
                expected,
                def_span,
                ret_span,
            } => Diagnostic::error()
                .with_message("missing return for annotated function.")
                .with_labels(vec![
                    Label::primary(ctx.module_ref.hash(), def_span)
                        .with_message("But will implicitly return `None` here."),

                    Label::secondary(ctx.module_ref.hash(), ret_span)
                        .with_message(                    format!(
                            "Expected to return a value of {}.",
                            fmt_type!(expected)
                        )),
                ]),

            MontyError::UnknownType { node } => Diagnostic::error()
                .with_message("unable to infer type for value.")
                .with_labels(vec![Label::primary(ctx.module_ref.hash(), node.span().unwrap())
                    .with_message("annotate this name with a type")]),

            MontyError::UndefinedVariable { node } => Diagnostic::error()
                .with_message("use of undefined variable.")
                .with_labels(vec![Label::primary(ctx.module_ref.hash(), node.span().unwrap()).with_message(
                    format!(
                        "\"{}\" is not defined.",
                        ctx.global_context
                            .resolver
                            .sources
                            .get(&ctx.module_ref)
                            .unwrap()
                            .value()
                            .get(node.span().unwrap())
                            .unwrap()
                            .to_string()
                    ),
                )]),

            MontyError::BadArgumentType {
                expected,
                actual,
                arg_node,
                def_node,
            } => Diagnostic::error()
                .with_message("incompatible argument type in function call.")
                .with_labels(vec![
                    Label::primary(ctx.module_ref.hash(), arg_node.span.clone()).with_message(format!(
                        "The argument here evaluates to \"{}\"",
                        fmt_type!(actual)
                    )),
                    Label::secondary(ctx.module_ref.hash(), def_node.inner.name.span.clone()).with_message(format!(
                        "Function defined here expected \"{}\"",
                        fmt_type!(expected),
                    )),
                ]),

            MontyError::BadBinaryOp {
                span,
                left,
                right,
                op,
            } => {
                let ty = &ctx.global_context.type_map;

                let left = ty.get(left).unwrap();
                let right = ty.get(right).unwrap();

                Diagnostic::error()
                    .with_message("Unsupported binary expression.")
                    .with_labels(vec![Label::primary(ctx.module_ref.hash(), span.clone()).with_message(
                        format!(
                            "Operator {:?} is not supported for types \"{}\" and \"{}\"",
                            op.sigil(),
                            ctx.as_formattable(left.value()),
                            ctx.as_formattable(right.value())
                        ),
                    )])
            }

            MontyError::BadConditionalType { actual, span } => Diagnostic::error()
                .with_message("Incompatible type for conditional.")
                .with_labels(vec![Label::primary(ctx.module_ref.hash(), span).with_message(format!(
                    "expected {}, found {}",
                    fmt_type!(TypeMap::BOOL),
                    fmt_type!(actual),
                ))]),

            MontyError::IncompatibleTypes {
                left_span,
                left,
                right_span,
                right,
            } => Diagnostic::error()
                .with_message("Incompatible types")
                .with_labels(vec![
                    Label::primary(ctx.module_ref.hash(), right_span).with_message(format!(
                        "expected {}, found {}",
                        fmt_type!(left),
                        fmt_type!(right)
                    )),
                    Label::secondary(ctx.module_ref.hash(), left_span)
                        .with_message(format!("{}", fmt_type!(left))),
                ]),

            MontyError::UnboundLocal { name, assign, used } => Diagnostic::error()
                .with_message(format!(
                    "Local variable {:?} referenced before assignment",
                    ctx.global_context
                        .resolver
                        .resolve(ctx.module_ref.clone(), name)
                        .unwrap()
                ))
                .with_labels(vec![
                    Label::primary(ctx.module_ref.hash(), used).with_message("name used here."),
                    Label::secondary(ctx.module_ref.hash(), assign).with_message("but initially assigned later here."),
                ]),

            MontyError::IncompatibleReassignment {
                name,
                first_assigned,
                incorrectly_reassigned,
                expected,
                actual,
            } => Diagnostic::error()
                .with_message("Incompatible type for reassignment.")
                .with_labels(vec![
                    Label::primary(ctx.module_ref.hash(), first_assigned).with_message(format!(
                        "\"{}\" was first assigned here with type {}",
                        ctx.global_context
                            .resolver
                            .resolve(ctx.module_ref.clone(), name)
                            .unwrap(),
                        fmt_type!(expected),
                    )),
                    Label::secondary(ctx.module_ref.hash(), incorrectly_reassigned).with_message(format!(
                        "but gets incorrectly reassigned here with type {}",
                        fmt_type!(actual)
                    )),
                ])
                .with_notes(vec![
                    "Note: Names within the same scope can not be assigned with multiple different types...".to_string(),
                    format!("Help: Consider using a union type e.g. Union[{}, {}] instead.",
                        fmt_type!(actual),
                        fmt_type!(expected)
                    )
                ]),

            MontyError::NotCallable { kind, callsite } => Diagnostic::error().with_message("Object is not callable.").with_labels(vec![
                Label::primary(ctx.module_ref.hash(), callsite).with_message(format!("Attempted to call a non-callable object of type: {}", fmt_type!(kind))),
            ]),

            MontyError::Unsupported { span, message } => Diagnostic::error().with_message("Unsupported feature").with_labels(vec![Label::primary(ctx.module_ref.hash(), span).with_message(message)])
        }
    }
}
