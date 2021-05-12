#![deny(warnings)]
#![feature(
    variant_count,
    bool_to_option,
    drain_filter,
    assert_matches,
)]

pub mod ast;
pub mod class;
pub mod codegen;
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

pub mod database;
pub mod parser;
pub mod phantom;
pub mod scope;
pub mod typing;
pub mod error;

pub type Result<T> = std::result::Result<T, error::MontyError>;

#[macro_export]
macro_rules! isinstance {
    ($e:expr, $t:ident) => {{
        if let Some($crate::ast::Spanned { inner, .. }) =
            dbg!($crate::ast::_downcast_ref::<$crate::ast::Spanned<$t>>($e))
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

use structopt::*;

#[derive(Debug, StructOpt, Clone)]
pub struct CompilerOptions {
    /// The path to a monty compatible stdlib.
    #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
    pub libstd: std::path::PathBuf,

    /// The input file to compile.
    #[structopt(parse(from_os_str))]
    pub input: Option<std::path::PathBuf>,

    /// The C compiler to use.
    #[structopt(parse(from_os_str))]
    pub cc: Option<std::path::PathBuf>,

    /// The linker to use.
    #[structopt(parse(from_os_str))]
    pub ld: Option<std::path::PathBuf>,
}

pub mod prelude {
    pub use crate::{
        ast::{AstObject, Spanned},
        context::{GlobalContext, LocalContext, ModuleContext, ModuleFlags, ModuleRef},
        func::{DataRef, Function},
        layout::{BlockId, Layout},
        lowering::{Lower, LowerWith},
        parser::{token::PyToken, Parseable, ParserT, Span, SpanRef},
        scope::{LocalScope, LookupTarget, OpaqueScope, Scope, ScopeRoot, ChainedScope},
        typing::{FunctionType, LocalTypeId, TypeDescriptor, TypeMap, TypedObject},
        error::{CompilerError, MontyError},
    };
}
