#![feature(
    variant_count,
    bool_to_option,
    drain_filter,
    assert_matches,
    get_mut_unchecked
)]
#![deny(warnings)]

use std::rc::Rc;

use ast::{
    expr::{Expr, InfixOp},
    funcdef::FunctionDef,
    retrn::Return,
    AstObject, Spanned,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use context::LocalContext;
use parser::Span;
use prelude::{SpanEntry, TypeMap};
use typing::LocalTypeId;

pub mod ast;
pub mod class;
pub mod context;
pub mod fmt;
pub mod func;
pub mod layout;

pub mod lowering {
    pub trait Lower<Output> {
        fn lower(&self) -> Output;

        fn lower_and_then<F, T>(&self, f: F) -> T
        where
            F: Fn(&Self, Output) -> T,
        {
            f(self, self.lower())
        }
    }

    pub trait LowerWith<Input, Output> {
        fn lower_with(&self, i: Input) -> Output;

        fn lower_with_and_then<F, T>(&self, i: Input, f: F) -> T
        where
            F: Fn(&Self, Output) -> T,
        {
            f(self, self.lower_with(i))
        }
    }
}

pub mod parser;
pub mod scope;
pub mod typing;
pub mod database;
pub mod phantom;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, MontyError>;

#[macro_export]
macro_rules! isinstance {
    ($e:expr, $t:ident) => {{
        if let Some($crate::ast::Spanned { inner, .. }) =
            $crate::ast::_downcast_ref::<$crate::ast::Spanned<$t>>($e)
        {
            Some(inner)
        } else if let Some(inner) = $crate::ast::_downcast_ref::<$crate::scope::Renamed>($e)
            .and_then(|rn| rn.inner.as_ref().downcast_ref::<$t>())
        {
            Some(inner)
        } else {
            $crate::ast::_downcast_ref::<$t>($e)
        }
    }};

    ($e:expr, $t:ident, $p:pat => $a:expr) => {{
        let result = crate::isinstance!($e, $t); // result: Some(ref $t)

        match &result {
            Some($p) => Some($a),
            _ => None,
        }
    }};
}

#[derive(Debug, Error, Clone)]
pub enum MontyError {
    #[error("Local variable referenced before assignment.")]
    UnboundLocal {
        name: SpanEntry,
        assign: Span,
        used: Span,
    },

    #[error("Reassigned a name with an incompatible type.")]
    IncompatibleReassignment {
        name: SpanEntry,
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
        actual: LocalTypeId,
        def_node: Rc<Spanned<FunctionDef>>,
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

impl MontyError {
    pub fn into_diagnostic(
        self,
        ctx: &LocalContext<'_>,
    ) -> codespan_reporting::diagnostic::Diagnostic<()> {
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
                    Some(value) => Label::primary((), value.span().unwrap_or(ret_span))
                        .with_message("but value of this type is returned instead."),

                    None => Label::primary((), ret_span).with_message("`None` is returned here."),
                };

                labels.push(primary);

                let def = Label::secondary(
                    (),
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
                expected: _,
                actual: _,
                def_node,
            } => Diagnostic::error()
                .with_message("missing return for annotated function.")
                .with_labels(vec![
                    Label::primary((), def_node.span().unwrap())
                        .with_message("This function will implicitly return `None`"),
                    Label::secondary((), def_node.inner.returns.clone().unwrap().span().unwrap())
                        .with_message("But it has been annotated to return this instead."),
                ]),

            MontyError::UnknownType { node } => Diagnostic::error()
                .with_message("unable to infer type for value.")
                .with_labels(vec![Label::primary((), node.span().unwrap())
                    .with_message("annotate this name with a type")]),

            MontyError::UndefinedVariable { node } => Diagnostic::error()
                .with_message("use of undefined variable.")
                .with_labels(vec![Label::primary((), node.span().unwrap()).with_message(
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
                    Label::primary((), arg_node.span.clone()).with_message(format!(
                        "The argument here evaluates to \"{}\"",
                        fmt_type!(actual)
                    )),
                    Label::secondary((), def_node.inner.name.span.clone()).with_message(format!(
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
                    .with_labels(vec![Label::primary((), span.clone()).with_message(
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
                .with_labels(vec![Label::primary((), span).with_message(format!(
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
                    Label::primary((), right_span).with_message(format!(
                        "expected {}, found {}",
                        fmt_type!(left),
                        fmt_type!(right)
                    )),
                    Label::secondary((), left_span)
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
                    Label::primary((), used).with_message("name used here."),
                    Label::secondary((), assign).with_message("but initially assigned later here."),
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
                    Label::primary((), first_assigned).with_message(format!(
                        "\"{}\" was first assigned here with type {}",
                        ctx.global_context
                            .resolver
                            .resolve(ctx.module_ref.clone(), name)
                            .unwrap(),
                        fmt_type!(expected),
                    )),
                    Label::secondary((), incorrectly_reassigned).with_message(format!(
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
                Label::primary((), callsite).with_message(format!("Attempted to call a non-callable object of type: {}", fmt_type!(kind))),
            ]),

            MontyError::Unsupported { span, message } => Diagnostic::error().with_message("Unsupported feature").with_labels(vec![Label::primary((), span).with_message(message)])
        }
    }
}

use structopt::*;

#[derive(Debug, StructOpt)]
pub struct CompilerOptions {
    #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
    pub libstd: std::path::PathBuf,

    #[structopt(parse(from_os_str))]
    pub input: Option<std::path::PathBuf>,
}

pub mod prelude {
    pub use crate::{
        ast::{AstObject, Spanned},
        context::{GlobalContext, LocalContext, ModuleContext, ModuleFlags, ModuleRef},
        func::{DataRef, Function},
        layout::{BlockId, Layout},
        lowering::{Lower, LowerWith},
        parser::{token::PyToken, Parseable, ParserT, Span, SpanEntry, SpanRef},
        scope::{LocalScope, LookupTarget, OpaqueScope, Scope, ScopeRoot, WrappedScope},
        typing::{CompilerError, FunctionType, LocalTypeId, TypeMap, TypedObject},
        MontyError,
    };
}
