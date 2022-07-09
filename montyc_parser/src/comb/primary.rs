use nom::{
    error::{Error, ErrorKind},
    sequence::terminated,
    IResult,
};

use crate::{
    ast::{models::Primary, Atom, Expr},
    spanned::Spanned,
    token::PyToken,
    TokenStreamRef,
};

use super::{
    atom::atom,
    core::{expect, expect_many_n},
    expression, whitespace,
};

#[inline]
fn primary_subscript<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
    base: &Spanned<Primary>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    let (stream, _) = expect(PyToken::LBracket)(stream)?;
    let (stream, _) = whitespace(stream)?;

    let tuple = |stream| -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
        let (stream, values) = super::atom::tuple_literal_inner(stream)?;

        let first_value = values.get(0).unwrap();
        let last_value = values.last().unwrap();

        let span = first_value.span.start..last_value.span.end;

        let tple = Spanned {
            inner: Atom::Tuple(values),
            span,
        }
        .replace_with(Primary::Atomic)
        .replace_with(Expr::Primary);

        Ok((stream, tple))
    };

    let (stream, index) = tuple(stream).or_else(|_| expression(stream))?;

    let (stream, _) = whitespace(stream)?;
    let (stream, rbracket) = expect(PyToken::RBracket)(stream)?;

    let obj = Spanned {
        span: base.span.start..rbracket.span.end,
        inner: Primary::Subscript {
            value: Box::new(base.clone()),
            index: Box::new(index),
        },
    };

    Ok((stream, obj))
}

#[inline]
fn primary_call<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
    base: &Spanned<Primary>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    let (stream, _) = expect(PyToken::LParen)(stream)?;
    let (mut stream, _) = whitespace(stream)?;

    let mut args = vec![];
    let rparen;

    loop {
        let (s, _) = whitespace(stream)?;

        if let Ok((s, arg)) = super::expr::expression(s) {
            args.push(arg);
            let (s, _) = expect_many_n::<0>(PyToken::Whitespace)(s).unwrap();

            if let Ok((s, _)) = expect(PyToken::Comma)(s) {
                stream = s;
                continue;
            } else if let Ok((s, r)) = expect(PyToken::RParen)(s) {
                rparen = r;
                stream = s;
                break;
            } else {
                unreachable!("{:?}", s);
            }
        } else if let Ok((s, r)) = expect(PyToken::RParen)(s) {
            rparen = r;
            stream = s;
            break;
        } else {
            unreachable!("{:?}", stream);
        }
    }

    let _ = rparen; // TODO: figure out how to span the func name and whole call expr appropriately.

    let obj = Spanned {
        span: base.span.start..base.span.end,
        inner: Primary::Call {
            func: Box::new(base.clone()),
            args: if args.is_empty() { None } else { Some(args) },
        },
    };

    Ok((stream, obj))
}

#[inline]
fn primary_dot_name<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
    base: &Spanned<Primary>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    let (stream, _) = whitespace(stream)?;
    let (stream, _) = expect(PyToken::Dot)(stream)?;
    let (stream, _) = whitespace(stream)?;
    let (stream, ident) = super::atom::name(stream)?;
    let (stream, _) = whitespace(stream)?;

    let obj = Spanned {
        span: base.span.start..ident.span.end,
        inner: Primary::Attribute {
            left: Box::new(base.clone()),
            attr: ident,
        },
    };

    Ok((stream, obj))
}

#[inline]
fn primary_left_recurse<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
    base: &Spanned<Primary>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    // Try each of the subrules until success or bail with a nom error.
    let mut output = primary_dot_name(stream, &base)
        .or_else(|_| primary_call(stream, &base))
        .or_else(|_| primary_subscript(stream, &base))
        .map_err(|_| nom::Err::Error(Error::new(stream, ErrorKind::IsNot)))?;

    // recurse here and parse as much as we can for a primary.
    while let Ok((stream, object)) = primary_left_recurse(output.0, &output.1) {
        output = (stream, object);
    }

    Ok(output)
}

#[inline]
pub fn primary<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    let (stream, atom) = atom(stream)?;
    let base = atom.replace_with(|at| Primary::Atomic(at));

    primary_left_recurse(stream, &base).or(Ok((stream, base)))
}

#[inline]
pub fn await_primary<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    let mut await_ = terminated(
        expect(PyToken::Await),
        expect_many_n::<1>(PyToken::Whitespace),
    );

    if let Ok((stream, await_node)) = await_(stream) {
        let (stream, Spanned { span, inner }) = await_primary(stream)?;

        let obj = Spanned {
            span: await_node.span.start..span.end,
            inner,
        };

        Ok((stream, obj))
    } else {
        primary(stream)
    }
}
