#![feature(variant_count, bool_to_option)]
#![allow(warnings)]

use std::rc::Rc;

use ast::{funcdef::FunctionDef, retrn::Return, AstObject, Spanned};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use typing::LocalTypeId;

pub mod ast;
pub mod class;
pub mod context;
pub mod func;
pub mod parser;
pub mod scope;
pub mod typing;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, MontyError>;

#[derive(Debug, Error)]
pub enum MontyError {
    #[error("Incompatible return type.")]
    BadReturnType {
        expected: LocalTypeId,
        actual: LocalTypeId,
        ret_node: Rc<Spanned<Return>>,
        def_node: Rc<Spanned<FunctionDef>>,
    },
}

impl From<MontyError> for codespan_reporting::diagnostic::Diagnostic<()> {
    fn from(err: MontyError) -> Self {
        match err {
            MontyError::BadReturnType {
                expected,
                actual,
                ret_node,
                def_node,
            } => {
                let mut labels = vec![];

                let ret_span = ret_node.span().unwrap();

                let primary = match &ret_node.inner.value {
                    Some(value) => Label::primary((), value.span().unwrap_or(ret_span)).with_message("{type} is implicitly returned here."),
                    None => Label::primary((), ret_span).with_message("`None` is implicitly returned here.")
                };

                labels.push(primary);

                let def = Label::secondary((), def_node.span().unwrap()).with_message("function defined here with different type.");

                labels.push(def);

                Diagnostic::error()
                    .with_message("incomaptible return type for function.")
                    .with_labels(labels)
            }
        }
    }
}

use structopt::*;

#[derive(Debug, StructOpt)]
pub struct CompilerOptions {
    #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
    libstd: std::path::PathBuf,

    #[structopt(parse(from_os_str))]
    input: Option<std::path::PathBuf>,
}
