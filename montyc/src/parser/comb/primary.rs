use std::rc::Rc;

use nom::{
    error::{Error, ErrorKind},
    sequence::terminated,
    IResult,
};

use crate::ast::{primary::Primary, Spanned};
use crate::parser::{token::PyToken, TokenSlice};

use super::{
    atom::atom,
    core::{expect, expect_, expect_many_n},
};

#[inline]
fn primary_subscript<'a>(
    stream: TokenSlice<'a>,
    base: &Spanned<Primary>,
) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let (stream, _) = expect(stream, PyToken::LBracket)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, rbracket) = expect(stream, PyToken::RBracket)?;

    let obj = Spanned {
        span: base.span.start..rbracket.span.end,
        inner: Primary::Subscript {
            value: Rc::new(base.clone()),
            index: None,
        },
    };

    Ok((stream, obj))
}

#[inline]
fn primary_call<'a>(
    stream: TokenSlice<'a>,
    base: &Spanned<Primary>,
) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let (stream, _) = expect(stream, PyToken::LParen)?;
    let (mut stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream).unwrap();

    let mut args = vec![];
    let rparen;

    loop {
        let (s, _) = expect_many_n::<0>(PyToken::Whitespace)(stream).unwrap();

        if let Ok((s, arg)) = super::expr::expression(s) {
            args.push(Rc::new(arg));
            let (s, _) = expect_many_n::<0>(PyToken::Whitespace)(s).unwrap();

            if let Ok((s, _)) = expect(s, PyToken::Comma) {
                stream = s;
                continue;

            } else if let Ok((s, r)) = expect(s, PyToken::RParen) {
                rparen = r;
                stream = s;
                break;

            } else {
                unreachable!("{:?}", s);
            }
        } else {
            unreachable!()
        }
    }

    let obj = Spanned {
        span: base.span.start..rparen.span.end,
        inner: Primary::Call {
            func: Rc::new(base.clone()),
            args: if args.is_empty() { None } else { Some(args) },
        },
    };

    Ok((stream, obj))
}

#[inline]
fn primary_dot_name<'a>(
    stream: TokenSlice<'a>,
    base: &Spanned<Primary>,
) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::Dot)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, ident) = super::atom::name(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let obj = Spanned {
        span: base.span.start..ident.span.end,
        inner: Primary::Attribute {
            left: Rc::new(base.clone()),
            attr: ident,
        },
    };

    Ok((stream, obj))
}

#[inline]
fn primary_left_recurse<'a>(
    stream: TokenSlice<'a>,
    base: &Spanned<Primary>,
) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
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
pub fn primary<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let (stream, atom) = atom(stream)?;
    let base = atom.map(Primary::Atomic);

    primary_left_recurse(stream, &base).or(Ok((stream, base)))
}

#[inline]
pub fn await_primary<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let mut await_ = terminated(
        expect_(PyToken::Await),
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
