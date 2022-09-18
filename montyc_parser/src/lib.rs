//! Parser implementation for Python 3.8+ syntax
//!

#![forbid(unsafe_code)]
#![warn(warnings)]

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

pub fn parse<I>(input: I) -> Result<ast::module::Module, ()>
where
    I: AsRef<str>,
{
    let input = input.as_ref();
    let tokens: Vec<Token> = montyc_lexer::tokens(input);

    todo!();

    // let (output, errors) = comb::module().parse_recovery(tokens);

    // todo!("{:?}", (output, errors))
}
