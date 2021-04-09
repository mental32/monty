use std::rc::Rc;

use nom::{branch::alt, error, multi::many0, sequence::terminated, IResult};

use crate::{
    ast::{atom::Atom, funcdef::FunctionDef, primary::Primary, Spanned},
    parser::{token::PyToken, SpanEntry, TokenSlice},
};

use super::{expect, expect_, expect_ident, expect_many_n, stmt::statement};

// #[inline]
// fn argument<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, (SpanEntry, SpanEntry)> {
//     let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

//     let (stream, name_obj) = expect_ident(stream)?;

//     let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

//     let (stream, kind) = match expect(stream, PyToken::Colon) {
//         Ok((stream, _)) => {
//             let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
//             let (stream, kind) = expect_ident(stream)?;
//             (stream, kind)
//         }

//         Err(nom::Err::Error(error::Error {
//             input: [(_, first), ..],
//             ..
//         })) => panic!(
//             "Missing type annotation @ {:?}",
//             name_obj.span.start..first.end
//         ),

//         Err(_) => unimplemented!(),
//     };

//     // match (name, kind) {
//     //     (PyToken::Ident(name), PyToken::Ident(kind)) => Ok((stream, (name, kind))),
//     //     _ => unreachable!(),
//     // }
// }

// #[inline]
// fn arguments<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<(SpanEntry, SpanEntry)>> {
//     let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
//     many0(alt((
//         terminated(argument, expect_(PyToken::Comma)),
//         argument,
//     )))(stream)
// }

#[inline]
pub fn function_def<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<FunctionDef>> {
    let (stream, def) = expect(stream, PyToken::FnDef)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, ident) = expect_ident(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::LParen)?;
    // let (stream, arguments) = arguments(stream)?;
    // let arguments = vec![];
    let (stream, _) = expect(stream, PyToken::RParen)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

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
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, body) = statement(stream)?;

    // let args = if arguments.is_empty() {
    //     None
    // } else {
    //     Some(arguments)
    // };

    let span = ident.span;
    let name = match ident.inner {
        PyToken::Ident(n) => Atom::Name(n),
        _ => unreachable!(),
    };

    let name = Spanned { span, inner: name };

    let funcdef = FunctionDef {
        name,
        body: vec![Rc::new(body) as Rc<_>],
        returns,
    };

    let funcdef = Spanned {
        span: 0..0,
        inner: funcdef,
    };

    Ok((stream, funcdef))
}
