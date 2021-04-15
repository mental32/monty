#![feature(variant_count, bool_to_option)]
#![allow(warnings)]

use std::rc::Rc;

use ast::{funcdef::FunctionDef, retrn::Return, AstObject, Spanned};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use context::LocalContext;
use typing::LocalTypeId;

pub mod ast;
pub mod class;
pub mod context;
pub mod func;
pub mod parser;
pub mod scope;
pub mod typing;

use thiserror::Error;

pub type Result<'a, T> = std::result::Result<T, MontyError<'a>>;

#[derive(Debug, Error)]
pub enum MontyError<'a> {
    #[error("Incompatible return type.")]
    BadReturnType {
        expected: LocalTypeId,
        actual: LocalTypeId,
        ret_node: Rc<Spanned<Return>>,
        def_node: Rc<Spanned<FunctionDef>>,
        ctx: &'a LocalContext<'a>,
    },

    #[error("Incompatible return type due to implicit return.")]
    MissingReturn {
        expected: LocalTypeId,
        actual: LocalTypeId,
        def_node: Rc<Spanned<FunctionDef>>,
        ctx: &'a LocalContext<'a>,
    },
}

impl<'a> From<MontyError<'a>> for codespan_reporting::diagnostic::Diagnostic<()> {
    fn from(err: MontyError) -> Self {
        match err {
            MontyError::BadReturnType {
                expected,
                actual,
                ret_node,
                def_node,
                ctx,
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
                    def_node.inner.returns.span().unwrap(),
                )
                .with_message(if def_node.inner.returns.is_some() {
                    "function defined here supposedly returning a value of this type."
                } else {
                    "function defined here is expected to return `None`"
                });

                labels.push(def);

                let type_map = ctx.global_context.type_map.borrow();

                Diagnostic::error()
                    .with_message("incomaptible return type for function.")
                    .with_labels(labels)
                    .with_notes(vec![
                        format!("expected: {}", type_map.get(expected).unwrap()),
                        format!("actual: {}", type_map.get(actual).unwrap()),
                    ])
            }

            MontyError::MissingReturn {
                expected,
                actual,
                def_node,
                ctx,
            } => Diagnostic::error()
                .with_message("missing return for annotated function.")
                .with_labels(vec![
                    Label::primary((), def_node.span().unwrap())
                        .with_message("This function will implicitly return `None`"),
                    Label::secondary((), def_node.inner.returns.span().unwrap())
                        .with_message("But it has been annotated to return this instead."),
                ]),
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
