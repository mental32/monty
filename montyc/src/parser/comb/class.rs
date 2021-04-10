use std::rc::Rc;

use nom::IResult;

use crate::{
    ast::{atom::Atom, class::ClassDef, primary::Primary, Spanned},
    parser::{token::PyToken, TokenSlice},
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

    let name = Rc::new(name);

    let start = decorators
        .get(0)
        .map(|t| t.span.start.clone())
        .unwrap_or(tok.span.start);

    let end = name.span.end.clone();

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
                body.push(Rc::new(stmt));
                let (s, _) = expect_many_n::<0>(PyToken::Newline)(s)?;
                stream = s;
            } else {
                break;
            }
        }
    } else {
        let (s, stmt) = statement(stream)?;
        body.push(Rc::new(stmt));
    }

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

fn decorator<'a>(mut stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Primary>> {
    let (stream, at) = expect(stream, PyToken::At)?;
    let (stream, dec) = super::primary(stream)?;

    match &dec.inner {
        crate::ast::primary::Primary::Atomic(Atom::Name(_))
        | crate::ast::primary::Primary::Attribute { .. } => {}
        _ => unreachable!(),
    }

    Ok((
        stream,
        Spanned {
            span: at.span.start..dec.span.end,
            inner: dec.inner,
        },
    ))
}

fn decorator_list<'a>(
    stream: TokenSlice<'a>,
) -> IResult<TokenSlice<'a>, Vec<Rc<Spanned<Primary>>>> {
    let mut list = vec![];
    let mut stream = stream;

    loop {
        if let Ok((tail, dec)) = decorator(stream) {
            list.push(Rc::new(dec));

            let (tail, refs) = chomp(tail).unwrap_or((tail, vec![]));

            stream = tail;
        } else {
            break;
        }
    }

    Ok((stream, list))
}
