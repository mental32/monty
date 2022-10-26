//! Parser implementation for Python 3.8+ syntax
//!

#![forbid(unsafe_code)]
#![warn(warnings)]
#![feature(trace_macros)]

use chumsky::Parser;

pub(crate) mod spanned {
    pub use montyc_ast::spanned::*;
}

pub mod ast {
    pub use montyc_ast::*;
}

pub(crate) mod comb;
pub(crate) mod span_interner;
pub(crate) mod token_stream_iter;

pub mod prelude {
    //! common exports, ast models and such.

    pub use crate::ast::{
        atom::Atom,
        expr::{Expr, InfixOp, UnaryOp},
        primary::Primary,
        AstNode, AstObject,
    };
}

pub(crate) type Token = (montyc_lexer::PyToken, montyc_lexer::Span);

pub type Error = ();
pub type Output<T> = (
    Option<montyc_ast::spanned::Spanned<T>>,
    Vec<chumsky::error::Simple<Token>>,
);

#[inline]
pub fn parse<I>(input: I) -> Result<Output<ast::module::Module>, Error>
where
    I: AsRef<str>,
{
    let input = input.as_ref();
    let lexer = montyc_lexer::lex(input);
    let sr = crate::span_interner::SpanInterner::new();

    let tokens: Vec<Token> = crate::token_stream_iter::TokenStreamIter {
        bound: sr.get(input, ()).unwrap(),
        lexer,
    }
    .map(|res| res.unwrap())
    .collect();

    Ok(comb::module().parse_recovery(tokens))
}
