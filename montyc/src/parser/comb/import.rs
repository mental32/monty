use std::rc::Rc;

use nom::{branch::alt, IResult};

use crate::{
    ast::{atom::Atom, import::Import, Spanned},
    parser::{token::PyToken, TokenSlice},
};

use super::{expect, expect_many_n, primary};

#[inline]
fn import_from<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Import>> {
    let (stream, tok) = expect(stream, PyToken::Import)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let mut names = vec![];

    let mut remaining = stream;

    let stream = loop {
        let (r, _) = expect_many_n::<0>(PyToken::Whitespace)(remaining)?;
        let (r, thing) = primary(r)?;
        remaining = r;

        match &thing.inner {
            crate::ast::primary::Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {}
            crate::ast::primary::Primary::Attribute { .. } => {}
            _ => unreachable!(),
        }

        names.push(Rc::new(thing));

        remaining = match expect(remaining, PyToken::Comma) {
            Ok((remaining, _)) => remaining,
            Err(_) => break remaining,
        };
    };

    let imprt = Spanned {
        span: tok.span.start..names.last().unwrap().span.end,
        inner: Import::Names(names),
    };

    Ok((stream, imprt))
}

#[inline]
fn from_import<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Import>> {
    let (stream, tok) = expect(stream, PyToken::From)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, dots) = expect_many_n::<0>(PyToken::Dot)(stream)?;
    let (stream, module) = primary(stream)?;

    let level = dots.len();

    match &module.inner {
        crate::ast::primary::Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {},
        crate::ast::primary::Primary::Attribute { .. } => {}
        _ => unreachable!(),
    }

    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::Import)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let mut names = vec![];

    let mut remaining = stream;

    let stream = loop {
        let (r, _) = expect_many_n::<0>(PyToken::Whitespace)(remaining)?;
        let (r, thing) = primary(r)?;
        remaining = r;

        match &thing.inner {
            crate::ast::primary::Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {},
            crate::ast::primary::Primary::Attribute { .. } => {}
            _ => unreachable!(),
        }

        names.push(Rc::new(thing));

        remaining = match expect(remaining, PyToken::Comma) {
            Ok((remaining, _)) => remaining,
            Err(_) => break remaining,
        };
    };

    let imprt = Spanned {
        span: tok.span.start..names.last().unwrap().span.end,
        inner: Import::From {
            module: Rc::new(module),
            names,
            level,
        },
    };

    Ok((stream, imprt))
}

#[inline]
pub fn import<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Import>> {
    alt((import_from, from_import))(stream)
}
