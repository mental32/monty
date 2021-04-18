use std::rc::Rc;

use nom::{sequence::tuple, IResult};

use crate::ast::{
    atom::Atom,
    expr::Expr,
    module::{self, Module},
    primary::Primary,
    retrn::Return,
    stmt::Statement,
    Spanned,
};

use super::{token::PyToken, TokenSlice};

#[cfg(test)]
mod test;

pub mod assign;
pub mod atom;
pub mod block;
pub mod class;
pub mod core;
pub mod expr;
pub mod funcdef;
pub mod ifelse;
pub mod import;
pub mod primary;
pub mod stmt;

pub use {self::core::*, assign::*, atom::*, expr::*, ifelse::*, primary::*};


#[inline]
pub fn return_unspanned<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Return> {
    let (stream, Spanned { inner, .. }) = return_stmt(stream)?;

    Ok((stream, inner))
}

#[inline]
pub fn return_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Return>> {
    let (stream, ret) = expect(stream, PyToken::Return)?;

    let (stream, span_end, value) = match expression(stream) {
        Ok((stream, value)) => (stream, value.span.end, Some(value)),
        Err(_) => (stream, ret.span.end, None),
    };

    let inner = Return { value };

    let ret = Spanned {
        span: ret.span.start..span_end,
        inner,
    };

    Ok((stream, ret))
}

fn chomp<'a>(mut stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<Spanned<PyToken>>> {
    let mut refs = vec![];

    loop {
        if let Ok((s, _)) = expect(stream, PyToken::Whitespace) {
            stream = s;
            continue;
        } else if let Ok((s, _)) = expect(stream, PyToken::Newline) {
            stream = s;
            continue;
        } else if let Ok((s, sr)) =
            expect_with(stream, |(t, _)| matches!(t, PyToken::CommentRef(_)))
        {
            refs.push(sr);
            stream = s;
            continue;
        } else {
            break;
        }
    }

    Ok((stream, refs))
}

#[inline]
pub fn module<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Module> {
    let (mut stream, mut head) = chomp(stream)?;

    let mut body: Vec<_> = head
        .drain(..)
        .map(|tok| {
            let a = tok.transparent_with(|_| Atom::from(tok.inner));
            let b = a.transparent_with(|_| Primary::Atomic(a.clone()));
            let c = b.transparent_with(|_| Expr::Primary(b.clone()));
            let d = c.map(|c| Statement::Expression(c));

            Rc::new(d)
        })
        .collect();

    loop {
        if let Ok((s, stmt)) = stmt::statement(stream) {
            body.push(Rc::new(stmt));
            let (s, _) = expect_many_n::<0>(PyToken::Newline)(s)?;
            stream = s;
        } else {
            break;
        }
    }

    let (stream, mut tail) = chomp(stream)?;

    body.extend(tail.drain(..).map(|tok| {
        let a = tok.transparent_with(|_| Atom::from(tok.inner));
        let b = a.transparent_with(|_| Primary::Atomic(a.clone()));
        let c = b.transparent_with(|_| Expr::Primary(b.clone()));
        let d = c.map(|c| Statement::Expression(c));

        Rc::new(d)
    }));

    let module = Module { body };

    Ok((stream, module))
}
