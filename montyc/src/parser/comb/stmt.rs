use nom::{branch::alt, IResult};

use crate::{
    ast::{atom::Atom, expr::Expr, primary::Primary, stmt::Statement, Spanned},
    parser::{token::PyToken, TokenSlice},
};

use super::{
    assignment, expect, expect_many_n, expect_with, expression, funcdef::function_def, return_stmt,
};

#[inline]
fn dyn_assign<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (
        stream,
        Spanned {
            span,
            inner: assign,
        },
    ) = assignment(stream)?;
    let assign = Spanned {
        span,
        inner: Statement::Asn(assign),
    };

    Ok((stream, assign))
}

#[inline]
fn dyn_return<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner: ret }) = return_stmt(stream)?;
    let ret = Spanned {
        span,
        inner: Statement::Ret(ret),
    };

    Ok((stream, ret))
}

#[inline]
fn dyn_funcdef<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = function_def(stream)?;
    let fndef = Spanned {
        span,
        inner: Statement::FnDef(inner),
    };

    Ok((stream, fndef))
}

#[inline]
fn dyn_import<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, import) = super::import::import(stream)?;

    Ok((stream, import.map(Statement::Import)))
}

#[inline]
fn dyn_classdef<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, klass) = super::class::class_def(stream)?;

    Ok((stream, klass.map(Statement::Class)))
}

#[inline]
fn dyn_ifstmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, klass) = super::ifelse::if_stmt(stream)?;

    Ok((stream, klass.map(Statement::If)))
}

#[inline]
fn dyn_while<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, klass) = super::while_::while_stmt(stream)?;

    Ok((stream, klass.map(Statement::While)))
}

#[inline]
fn dyn_span_ref<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, tok) = expect_with(stream, |(t, _)| {
        matches!(
            t,
            PyToken::Ident(_) | PyToken::CommentRef(_) | PyToken::StringRef(_)
        )
    })?;

    let a = tok.map(Atom::from);
    let a = a.transparent_with(Primary::Atomic);
    let a = a.transparent_with(Expr::Primary);
    let a = a.map(Statement::Expression);

    Ok((stream, a))
}

#[inline]
fn dyn_pass<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, span) = expect(stream, PyToken::Pass)?;
    Ok((stream, span.map(|_| Statement::Pass)))
}

#[inline]
fn dyn_expr<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, span) = expression(stream)?;
    Ok((stream, span.map(Statement::Expression)))
}

#[inline]
fn small_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((dyn_assign, dyn_return, dyn_pass, dyn_expr, dyn_span_ref))(stream)
}

#[inline]
fn compound_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((dyn_funcdef, dyn_import, dyn_classdef, dyn_ifstmt, dyn_while))(stream)
}

#[inline]
pub fn statement_unstripped<'a>(
    stream: TokenSlice<'a>,
) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((small_stmt, compound_stmt))(stream)
}

#[inline]
pub fn statement<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let stream = expect_many_n::<0>(PyToken::Whitespace)(stream)
        .map(|(stream, _)| stream)
        .unwrap_or(stream);

    alt((small_stmt, compound_stmt))(stream)
}

#[inline]
pub fn statement_unspanned<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Statement> {
    let (stream, Spanned { inner, .. }) = statement(stream)?;

    Ok((stream, inner))
}
