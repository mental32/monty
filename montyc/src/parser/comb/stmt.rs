use std::rc::Rc;

use nom::{branch::alt, sequence::terminated, IResult};

use crate::{ast::{Spanned, assign::Annotation, atom::Atom, expr::Expr, primary::Primary, stmt::Statement}, parser::{token::PyToken, TokenSlice}};

use super::{
    assignment, expect, expect_, expect_many_n, expect_with, expression, funcdef::function_def,
    name, return_stmt,
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
fn dyn_annotation<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, ident) = terminated(name, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let (stream, _) = terminated(
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    )(stream)?;

    let (stream, kind) = terminated(expression, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let ann = Spanned {
        span: ident.span.start..kind.span.end,
        inner: Statement::Ann(Annotation { name: ident, kind: Rc::new(kind) }),
    };

    Ok((stream, ann))
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
    let a = a.transparent_with(|at| Primary::Atomic(Rc::new(at)));
    let a = a.transparent_with(Expr::Primary);
    let a = a.map(Statement::Expr);

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
    Ok((stream, span.map(Statement::Expr)))
}

#[inline]
fn small_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((
        dyn_assign,
        dyn_annotation,
        dyn_return,
        dyn_pass,
        dyn_expr,
        dyn_span_ref,
    ))(stream)
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
