use nom::{branch::alt, sequence::terminated, IResult};

use crate::{
    ast::models::{Annotation, Atom, Expr, Primary, Statement},
    spanned::Spanned,
    token::PyToken,
    TokenStreamRef,
};

use super::{
    assignment, expect, expect_, expect_many_n, expect_with, expression, funcdef::function_def,
    name, return_stmt,
};

#[inline]
fn dyn_assign<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
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
fn dyn_annotation<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, ident) = terminated(name, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let (stream, _) = terminated(
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    )(stream)?;

    let (stream, kind) = terminated(expression, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let ann = Spanned {
        span: ident.span.start..kind.span.end,
        inner: Statement::Ann(Annotation {
            name: ident,
            kind: kind,
        }),
    };

    Ok((stream, ann))
}

#[inline]
fn dyn_return<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, Spanned { span, inner: ret }) = return_stmt(stream)?;
    let ret = Spanned {
        span,
        inner: Statement::Ret(ret),
    };

    Ok((stream, ret))
}

#[inline]
fn dyn_funcdef<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = function_def(stream)?;
    let fndef = Spanned {
        span,
        inner: Statement::FnDef(inner),
    };

    Ok((stream, fndef))
}

#[inline]
fn dyn_import<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, import) = super::import::import(stream)?;

    Ok((stream, import.map(Statement::Import)))
}

#[inline]
fn dyn_classdef<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, klass) = super::class::class_def(stream)?;

    Ok((stream, klass.map(Statement::Class)))
}

#[inline]
fn dyn_ifstmt<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, klass) = super::ifelse::if_stmt(stream)?;

    Ok((stream, klass.map(Statement::If)))
}

#[inline]
fn dyn_while<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, klass) = super::while_::while_stmt(stream)?;

    Ok((stream, klass.map(Statement::While)))
}

#[inline]
fn dyn_span_ref<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, tok) = expect_with(stream, |(t, _)| {
        matches!(
            t,
            PyToken::Ident(_) | PyToken::CommentRef(_) | PyToken::StringRef(_)
        )
    })?;

    let a = tok.map(Atom::from);
    let a = a.replace_with(|at| Primary::Atomic(at));
    let a = a.replace_with(Expr::Primary);
    let a = a.map(Statement::Expr);

    Ok((stream, a))
}

#[inline]
fn dyn_pass<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, span) = expect(stream, PyToken::Pass)?;
    Ok((stream, span.map(|_| Statement::Pass)))
}

#[inline]
fn dyn_expr<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Statement>> {
    let (stream, span) = expression(stream)?;
    Ok((stream, span.map(Statement::Expr)))
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
    let stream = expect_many_n::<0>(PyToken::Whitespace)(stream)
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
