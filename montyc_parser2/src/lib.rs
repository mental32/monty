//! Parser implementation for Python 3.8+ syntax
//!

#![forbid(unsafe_code)]
#![warn(warnings)]

use chumsky::Parser;

pub(crate) mod spanned {
    pub use montyc_ast::spanned::*;
}

pub mod ast {
    pub use montyc_ast::*;
}

pub mod comb;
pub mod span_interner;
pub mod token_stream_iter;

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

pub type Error = chumsky::error::Simple<Token>;
pub type Output<T> = (Option<montyc_ast::spanned::Spanned<T>>, Vec<Error>);

#[inline]
pub fn parse<I, M>(
    span_interner: &crate::span_interner::SpanInterner<M>,
    input_module_ref: M,
    input: I,
) -> Output<ast::module::Module>
where
    I: AsRef<str>,
    M: Clone,
{
    let input = input.as_ref();
    let lexer = montyc_lexer::lex(input);

    let tokens: Vec<Token> = crate::token_stream_iter::TokenStreamIter {
        bound: span_interner.get(input, input_module_ref).unwrap(),
        lexer,
        previous: None,
    }
    .map(|res| res.unwrap())
    .collect();

    tracing::trace!(n_tokens = ?tokens.len(), "collected lexer tokens");

    let verbose =
        std::env::var("MONTYC_PARSER_VERBOSE").map_or(false, |st| st == "1" || st == "true");

    if verbose {
        comb::module()
            .then_ignore(chumsky::primitive::end())
            .parse_recovery_verbose(tokens)
    } else {
        comb::module().parse_recovery(tokens)
    }
}
