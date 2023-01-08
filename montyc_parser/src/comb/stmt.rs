use montyc_ast::ann::Annotation;
use montyc_ast::atom::Atom;
use montyc_ast::expr::Expr;
use montyc_ast::primary::Primary;
use montyc_ast::spanned::Spanned;
use montyc_ast::statement::Statement;
use montyc_ast::Pass;
use montyc_lexer::PyToken;
use nom::{branch::alt, sequence::terminated, IResult};

use crate::TokenStreamRef;

use super::{
    assignment, expect, expect_many_n, expect_with, expression, funcdef::function_def, name,
    return_stmt, whitespace,
};

#[inline]
fn dyn_assign<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, assign) = assignment(stream)?;
    let assign = assign.replace_with(Statement::Asn);
    Ok((stream, assign))
}

#[inline]
fn dyn_annotation<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, ident) = terminated(name, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let (stream, _) = terminated(
        expect(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    )(stream)?;

    let (stream, annotation) =
        terminated(expression, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let ann = Spanned {
        span: ident.span.start..annotation.span.end,
        inner: Annotation {
            name: ident,
            annotation,
        },
    }
    .replace_with(Statement::Ann);

    Ok((stream, ann))
}

#[inline]
fn dyn_return<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, ret) = return_stmt(stream)?;
    let ret = ret.replace_with(Statement::Ret);

    Ok((stream, ret))
}

#[inline]
fn dyn_funcdef<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, fndef) = function_def(stream)?;
    let fndef = fndef.replace_with(Statement::FnDef);

    Ok((stream, fndef))
}

#[inline]
fn dyn_import<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, import) = super::import::import(stream)?;

    Ok((stream, import.replace_with(Statement::Import)))
}

#[inline]
fn dyn_classdef<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, klass) = super::class::class_def(stream)?;

    Ok((stream, klass.replace_with(Statement::Class)))
}

#[inline]
fn dyn_ifstmt<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, klass) = super::ifelse::if_stmt(stream)?;

    Ok((stream, klass.replace_with(Statement::If)))
}

#[inline]
fn dyn_while<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, klass) = super::while_::while_stmt(stream)?;

    Ok((stream, klass.replace_with(Statement::While)))
}

#[inline]
fn dyn_span_ref<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, tok) = expect_with(stream, |(t, _)| {
        matches!(
            t,
            PyToken::IdentRef(_) | PyToken::CommentRef(_) | PyToken::StringRef(_)
        )
    })?;

    let a = tok.map(Atom::from);
    let a = a.replace_with(|at| Primary::Atomic(at));
    let a = a.replace_with(Expr::Primary);
    let a = a.replace_with(Statement::Expr);

    Ok((stream, a))
}

#[inline]
fn dyn_pass<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, span) = expect(PyToken::Pass)(stream)?;
    let span = span.map(|_| Pass);
    Ok((stream, span.replace_with(Statement::Pass)))
}

#[inline]
fn dyn_expr<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, span) = expression(stream)?;
    Ok((stream, span.replace_with(Statement::Expr)))
}

#[inline]
fn small_stmt<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
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
fn compound_stmt<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    alt((dyn_funcdef, dyn_import, dyn_classdef, dyn_ifstmt, dyn_while))(stream)
}

#[inline]
pub fn statement_unstripped<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    alt((small_stmt, compound_stmt))(stream)
}

#[inline]
pub fn statement<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let stream = whitespace(stream)
        .map(|(stream, _)| stream)
        .unwrap_or(stream);

    alt((small_stmt, compound_stmt))(stream)
}

#[inline]
pub fn statement_unspanned<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Statement> {
    let (stream, Spanned { inner, .. }) = statement(stream)?;

    Ok((stream, inner))
}
