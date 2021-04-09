use std::rc::Rc;

use nom::{IResult, sequence::tuple};

use crate::ast::{Spanned, module::{self, Module}, retrn::Return};

use super::{TokenSlice, token::PyToken};

#[cfg(test)]
mod test;

pub mod atom;
pub mod primary;
pub mod assign;
pub mod core;
pub mod expr;
pub mod ifelse;
pub mod block;
pub mod funcdef;
pub mod stmt;

pub use {
    atom::*,
    primary::*,
    assign::*,
    self::core::*,
    expr::*,
    ifelse::*
};

#[inline]
pub fn return_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Return>> {
    let (stream, ret) = expect(stream, PyToken::Return)?;

    let (stream, span_end, value ) = match expression(stream) {
        Ok((stream, value)) => (stream, value.span.end, Some(value)),
        Err(_) => (stream, ret.span.end, None)
    };

    let inner = Return { value };

    let ret = Spanned {
        span: ret.span.start..span_end,
        inner,
    };

    Ok((stream, ret))
}

#[inline]
pub fn module<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Module> {
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, body) = stmt::statement(stream)?;

    let module = Module {
        body: vec![body],
    };

    Ok((stream, module))
}
