use nom::IResult;

use crate::{
    ast::models::{Atom, ClassDef, Primary},
    spanned::Spanned,
    token::PyToken,
    TokenSlice,
};

use super::{atom, chomp, expect, expect_many_n, stmt::statement};

#[inline]
pub fn class_def<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<ClassDef>> {
    // decorators on the def "@foo\n@bar"

    let (stream, decorators) = decorator_list(stream)?;

    // head of ClassDef "class <name>:"

    let (stream, tok) = expect(stream, PyToken::ClassDef)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, name) = atom(stream)?;

    let name = name;

    let start = decorators
        .get(0)
        .map(|t| t.span.start.clone())
        .unwrap_or(tok.span.start);

    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::Colon)?;
    let (mut stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    // body of ClassDef

    let mut body = vec![];

    if let Ok((s, _)) = expect(stream, PyToken::Newline) {
        stream = s;
        loop {
            if let Ok((s, _)) = expect_many_n::<4>(PyToken::Whitespace)(stream) {
                let (s, stmt) = statement(s)?;
                body.push(stmt);
                let (s, _) = expect_many_n::<0>(PyToken::Newline)(s)?;
                stream = s;
            } else {
                break;
            }
        }
    } else {
        let (_s, stmt) = statement(stream)?;
        body.push(stmt);
    }

    let end = body
        .last()
        .map(|s| s.span.end)
        .unwrap_or(name.span.end.clone());

    let def = Spanned {
        inner: ClassDef {
            name,
            decorator_list: decorators,
            body,
        },
        span: start..end,
    };

    Ok((stream, def))
}

pub(super) fn decorator<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let (stream, _at) = expect(stream, PyToken::At)?;
    let (stream, dec) = super::primary(stream)?;

    match &dec.inner {
        Primary::Atomic(atom)
            if matches!(
                atom,
                Spanned {
                    inner: Atom::Name(_),
                    ..
                }
            ) => {}
        Primary::Attribute { .. } => {}
        Primary::Call { .. } => {}
        _ => unreachable!(),
    }

    Ok((stream, dec))
}

pub(super) fn decorator_list<'a>(
    stream: TokenSlice<'a>,
) -> IResult<TokenSlice<'a>, Vec<Spanned<Primary>>> {
    let mut list = vec![];
    let mut stream = stream;

    loop {
        if let Ok((tail, dec)) = decorator(stream) {
            list.push(dec);

            let (tail, _refs) = chomp(tail).unwrap_or((tail, vec![]));

            stream = tail;
        } else {
            break;
        }
    }

    Ok((stream, list))
}
