use std::rc::Rc;

use nom::{branch::alt, error, multi::many0, sequence::terminated, IResult};

use crate::{
    ast::{atom::Atom, funcdef::FunctionDef, primary::Primary, Spanned},
    parser::{token::PyToken, SpanEntry, TokenSlice},
};

use super::{expect, expect_, expect_ident, expect_many_n, primary, stmt::statement};

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
                Primary::Subscript { value, index } => {}
                Primary::Call { func, args } => {}
                Primary::Attribute { left, attr } => {}
                Primary::Await(_) => unreachable!()
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
    let (stream, def) = expect(stream, PyToken::FnDef)?;
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

        let inner = Primary::Atomic(Atom::Name(match ret.inner {
            PyToken::Ident(n) => n,
            _ => unreachable!(),
        }));

        let ret = Spanned {
            span: ret.span,
            inner,
        };

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
        loop {
            if let Ok((remaining, _)) = terminated(
                expect_(PyToken::Newline),
                expect_many_n::<4>(PyToken::Whitespace),
            )(stream)
            {
                // panic!("{:?}", remaining);
                let (remaining, part) = statement(remaining).unwrap();

                body.push(Rc::new(part) as Rc<_>);
                stream = remaining;
            } else {
                break;
            }
        }
    }

    let args: Option<Vec<(SpanEntry, _)>> = if arguments.is_empty() {
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

    let funcdef = FunctionDef {
        name,
        args,
        body,
        returns,
    };

    let funcdef = Spanned {
        span: 0..0,
        inner: funcdef,
    };

    Ok((stream, funcdef))
}
