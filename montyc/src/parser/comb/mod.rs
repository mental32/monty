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
pub mod class;
pub mod import;

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

fn chomp<'a>(mut stream: TokenSlice<'a>)  -> IResult<TokenSlice<'a>, ()> {
    loop {
        if let Ok((s, _)) = expect(stream, PyToken::Whitespace) {
            stream = s;
            continue;
        } else if let Ok((s, _)) = expect(stream, PyToken::Newline) {
            stream = s;
            continue;
        } else if let Ok((s, _)) = expect_with(stream, |(t, _)| matches!(t, PyToken::SpanRef(_))) {
            stream = s;
            continue;
        } else {
            break
        }
    }

    Ok((stream, ()))
}

#[inline]
pub fn module<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Module> {
    let (stream, ()) = chomp(stream)?;

    let (stream, body) = stmt::statement(stream).unwrap();

    let module = Module {
        body: vec![body],
    };

    let (stream, ()) = chomp(stream)?;

    Ok((stream, module))
}
