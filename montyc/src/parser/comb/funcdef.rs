use std::rc::Rc;

use nom::{branch::alt, error, multi::many0, sequence::terminated, IResult};

use crate::{ast::{atom::Atom, funcdef::FunctionDef, primary::Primary, Spanned}, parser::{token::PyToken, TokenSlice}, prelude::SpanRef};

use super::{
    class::decorator_list, expect, expect_, expect_ident, expect_many_n, primary, stmt::statement,
};

#[inline]
fn argument<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<PyToken>> {
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, name) = expect_ident(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    Ok((stream, name))
}

#[inline]
fn argument_annotated<'a>(
    stream: TokenSlice<'a>,
) -> IResult<TokenSlice<'a>, (Spanned<PyToken>, Spanned<Primary>)> {
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, name) = expect_ident(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, kind) = match expect(stream, PyToken::Colon) {
        Ok((stream, _)) => {
            let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
            let (stream, kind) = primary(stream)?;

            match &kind.inner {
                Primary::Atomic(_) => {}
                Primary::Subscript { value: _, index: _ } => {}
                Primary::Call { func: _, args: _ } => {}
                Primary::Attribute { left: _, attr: _ } => {}
                Primary::Await(_) => unreachable!(),
            }

            (stream, kind)
        }

        Err(nom::Err::Error(error::Error {
            input: [(_, first), ..],
            ..
        })) => panic!("Missing type annotation @ {:?}", name.span.start..first.end),

        Err(_) => unimplemented!(),
    };

    Ok((stream, (name, kind)))
}

#[inline]
fn arguments<'a>(
    stream: TokenSlice<'a>,
) -> IResult<
    TokenSlice<'a>,
    (
        Option<Spanned<PyToken>>,
        Vec<(Spanned<PyToken>, Spanned<Primary>)>,
    ),
> {
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, recv) = terminated(argument, expect_(PyToken::Comma))(stream)
        .map(|(s, r)| (s, Some(r)))
        .unwrap_or((stream, None));

    let (stream, args) = many0(alt((
        terminated(argument_annotated, expect_(PyToken::Comma)),
        argument_annotated,
    )))(stream)?;

    Ok((stream, (recv, args)))
}

#[inline]
pub fn function_def<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<FunctionDef>> {
    let (stream, dec) = match decorator_list(stream) {
        Ok((stream, dec)) => (stream, Some(dec)),
        Err(_) => (stream, None),
    };

    let (stream, _def) = expect(stream, PyToken::FnDef)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, ident) = expect_ident(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::LParen)?;
    let (stream, (reciever, arguments)) = arguments(stream)?;
    let (stream, _) = expect(stream, PyToken::RParen)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    // return type annotation

    let arrow =
        expect(stream, PyToken::Minus).and_then(|(stream, _)| expect(stream, PyToken::GreaterThan));

    let (stream, returns) = if let Ok((stream, _)) = arrow {
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, ret) = expect_ident(stream)?;

        let ret = ret
            .map(|t| match t {
                PyToken::Ident(n) => Atom::Name(n),
                _ => unreachable!(),
            })
            .transparent_with(|atom| Primary::Atomic(Rc::new(atom)));

        (stream, Some(ret))
    } else {
        (stream, None)
    };

    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::Colon)?;
    let (mut stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    // body of the function

    let mut body = vec![];

    if let Ok((s, stmt)) = statement(stream) {
        body.push(Rc::new(stmt) as Rc<_>);
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

            if let Ok((remaining, _)) = expect_many_n::<4>(PyToken::Whitespace)(remaining) {
                let (remaining, part) = match statement(remaining) {
                    Ok(i) => i,
                    Err(err) => {
                        if let nom::Err::Error(err) = &err {
                            if let Some((top, _)) = err.input.get(0) {
                                if matches!(top, PyToken::Elif | PyToken::Else) {
                                    break;
                                }
                            }
                        }

                        return Err(err);
                    }
                };

                body.push(Rc::new(part) as Rc<_>);
                stream = remaining;
            } else {
                break;
            }
        }
    }

    let args: Option<Vec<(SpanRef, _)>> = if arguments.is_empty() {
        None
    } else {
        Some(
            arguments
                .iter()
                .map(|(l, r)| match (l.inner, r) {
                    (PyToken::Ident(l), r) => (l, Rc::new(r.clone())),
                    _ => unreachable!(),
                })
                .collect(),
        )
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
        span: funcdef.name.span.start..funcdef.body.last().unwrap().span().unwrap().end,
        inner: funcdef,
    };

    Ok((stream, funcdef))
}
