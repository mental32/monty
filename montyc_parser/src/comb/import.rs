use nom::{branch::alt, IResult};

use crate::{
    ast::{Atom, Import, Primary},
    spanned::Spanned,
    token::PyToken,
    TokenStreamRef,
};

use super::{expect, expect_many_n, primary};

#[inline]
fn import_from<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Import>> {
    let (stream, tok) = expect(stream, PyToken::Import)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let mut names = vec![];

    let mut remaining = stream;

    let stream = loop {
        let (r, _) = expect_many_n::<0>(PyToken::Whitespace)(remaining)?;
        let (r, thing) = primary(r)?;
        remaining = r;

        match &thing.inner {
            Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {}
            Primary::Attribute { .. } => {}
            _ => unreachable!(),
        }

        names.push(thing);

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
fn from_import<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Import>> {
    let (stream, tok) = expect(stream, PyToken::From)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, dots) = expect_many_n::<0>(PyToken::Dot)(stream)?;
    let (stream, module) = primary(stream)?;

    let level = dots.len();

    match &module.inner {
        Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {}
        Primary::Attribute { .. } => {}
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
            Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {}
            Primary::Attribute { .. } => {}
            _ => unreachable!(),
        }

        names.push(thing);

        remaining = match expect(remaining, PyToken::Comma) {
            Ok((remaining, _)) => remaining,
            Err(_) => break remaining,
        };
    };

    let imprt = Spanned {
        span: tok.span.start..names.last().unwrap().span.end,
        inner: Import::From {
            module: (module),
            names,
            level,
        },
    };

    Ok((stream, imprt))
}

#[inline]
pub fn import<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Import>> {
    alt((import_from, from_import))(stream)
}
