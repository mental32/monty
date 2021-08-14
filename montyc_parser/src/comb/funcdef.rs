use nom::{branch::alt, error, multi::many0, sequence::terminated, IResult};

use crate::{
    ast::{Atom, Expr, FunctionDef},
    comb::{expect_many_n_var, whitespace},
    spanned::Spanned,
    token::PyToken,
    TokenStreamRef,
};

use super::{
    class::decorator_list, expect, expect_ident, expect_many_n, expr::expression, stmt::statement,
};

#[inline]
fn argument<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<PyToken>> {
    let (stream, _) = whitespace(stream)?;
    let (stream, name) = expect_ident(stream)?;
    let (stream, _) = whitespace(stream)?;
    Ok((stream, name))
}

#[inline]
fn argument_annotated<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, (Spanned<PyToken>, Option<Spanned<Expr>>)> {
    let (stream, _) = whitespace(stream)?;
    let (stream, name) = expect_ident(stream)?;
    let (stream, _) = whitespace(stream)?;

    let (stream, kind) = match expect(PyToken::Colon)(stream) {
        Ok((stream, _)) => {
            let (stream, _) = whitespace(stream)?;
            let (stream, kind) = expression(stream)?;

            (stream, Some(kind))
        }

        Err(nom::Err::Error(error::Error { input, .. })) if !input.is_eof() => {
            let (stream, _) = whitespace(stream)?;

            (stream, None)
        }

        Err(_) => unimplemented!(),
    };

    Ok((stream, (name, kind)))
}

#[inline]
fn arguments<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    (
        Option<Spanned<PyToken>>,
        Vec<(Spanned<PyToken>, Option<Spanned<Expr>>)>,
    ),
> {
    let (stream, _) = whitespace(stream)?;

    let (stream, recv) = terminated(argument, expect(PyToken::Comma))(stream)
        .map(|(s, r)| (s, Some(r)))
        .unwrap_or((stream, None));

    let (stream, args) = many0(alt((
        terminated(argument_annotated, expect(PyToken::Comma)),
        argument_annotated,
    )))(stream)?;

    Ok((stream, (recv, args)))
}

#[inline]
pub fn function_def<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<FunctionDef>> {
    let (stream, dec) = match decorator_list(stream) {
        Ok((stream, dec)) => (stream, Some(dec)),
        Err(_) => (stream, None),
    };

    let (stream, _def) = expect(PyToken::FnDef)(stream)?;
    let (stream, _) = whitespace(stream)?;
    let (stream, ident) = expect_ident(stream)?;
    let (stream, _) = whitespace(stream)?;
    let (stream, _) = expect(PyToken::LParen)(stream)?;
    let (stream, (reciever, mut arguments)) = arguments(stream)?;
    let (stream, _) = expect(PyToken::RParen)(stream)?;
    let (stream, _) = whitespace(stream)?;

    // return type annotation

    let arrow =
        expect(PyToken::Minus)(stream).and_then(|(stream, _)| expect(PyToken::GreaterThan)(stream));

    let (stream, returns) = if let Ok((stream, _)) = arrow {
        let (stream, _) = whitespace(stream)?;
        let (stream, ret) = expression(stream)?;

        (stream, Some(ret))
    } else {
        (stream, None)
    };

    let (stream, _) = whitespace(stream)?;
    let (stream, _) = expect(PyToken::Colon)(stream)?;
    let (mut stream, _) = whitespace(stream)?;

    // body of the function

    let mut body = vec![];

    if let Ok((s, stmt)) = statement(stream) {
        body.push(stmt);
        stream = s;
    } else {
        let mut indent_level = None;

        loop {
            let (remaining, _) = expect_many_n::<0>(PyToken::Newline)(stream)?;

            let remaining = if indent_level.is_none() {
                let (_, indent) = expect_many_n::<0>(PyToken::Whitespace)(remaining)?;

                indent_level.replace(indent.len());

                remaining
            } else {
                remaining
            };

            if let Ok((remaining, _)) =
                expect_many_n_var(indent_level.unwrap(), PyToken::Whitespace)(remaining)
            {
                let (remaining, part) = statement(remaining)?;

                body.push(part);
                stream = remaining;
            } else {
                break;
            }
        }
    }

    let args: Option<Vec<(_, _)>> = if arguments.is_empty() {
        None
    } else {
        let args = arguments
            .drain(..)
            .map(|(l, r)| match (l.inner, r) {
                (PyToken::Ident(l), r) => (l, r),
                _ => unreachable!(),
            })
            .collect();

        Some(args)
    };

    let span = ident.span;
    let name = match ident.inner {
        PyToken::Ident(n) => Atom::Name(n),
        _ => unreachable!(),
    };

    let name = Spanned { span, inner: name };
    let reciever = reciever.map(|recv| {
        recv.map(|tok| match tok {
            PyToken::Ident(n) => Atom::Name(n),
            _ => unreachable!(),
        })
    });

    let funcdef = FunctionDef {
        reciever,
        name,
        args,
        body,
        returns,
        decorator_list: dec.unwrap_or(vec![]),
    };

    let funcdef = Spanned {
        span: funcdef.name.span.start..funcdef.body.last().unwrap().span.end,
        inner: funcdef,
    };

    Ok((stream, funcdef))
}
