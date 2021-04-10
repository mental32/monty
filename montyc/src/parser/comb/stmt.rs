use std::rc::Rc;

use nom::{IResult, branch::alt};

use crate::{ast::{AstObject, Spanned, funcdef, stmt::{self, Statement}}, parser::{token::PyToken, TokenSlice}};

use super::{assignment, expect, expect_many_n, expect_with, expression, funcdef::function_def, return_stmt};

#[inline]
fn dyn_assign<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner: assign }) = assignment(stream)?;
    let assign = Spanned { span, inner: Statement::Asn(assign) };

    Ok((stream, assign))
}

#[inline]
fn dyn_return<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner: ret }) = return_stmt(stream)?;
    let ret = Spanned { span, inner: Statement::Ret(ret) };

    Ok((stream, ret))
}

#[inline]
fn dyn_funcdef<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = function_def(stream)?;
    let fndef = Spanned { span, inner: Statement::FnDef(inner) };

    Ok((stream, fndef))
}

#[inline]
fn dyn_import<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = super::import::import(stream)?;
    let imprt = Spanned { span, inner: Statement::Import(inner) };

    Ok((stream, imprt))
}

#[inline]
fn dyn_classdef<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = super::class::class_def(stream)?;
    let klass = Spanned { span, inner: Statement::Class(inner) };

    Ok((stream, klass))
}

#[inline]
fn dyn_span_ref<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = expect_with(stream, |(t, _)| matches!(t, PyToken::SpanRef(_)))?;
    let span = Spanned { span, inner: Statement::SpanRef(inner) };

    Ok((stream, span))
}


#[inline]
fn dyn_pass<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, span) = expect(stream, PyToken::Pass)?;
    Ok((stream, span.map(|_| Statement::Pass)))
}

#[inline]
fn dyn_expr<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, span) = expression(stream)?;
    Ok((stream, span.map(|e| Statement::Expression(e))))
}

#[inline]
fn small_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((dyn_assign, dyn_return, dyn_span_ref, dyn_pass, dyn_expr))(stream)
}

#[inline]
fn compound_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((
        dyn_funcdef,
        dyn_import,
        dyn_classdef,
    ))(stream)
}

#[inline]
pub fn statement<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let stream = expect_many_n::<0>(PyToken::Whitespace)(stream)
        .map(|(stream, _)| stream)
        .unwrap_or(stream);

    alt((small_stmt, compound_stmt))(stream)
}
