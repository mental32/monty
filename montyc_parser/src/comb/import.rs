use montyc_ast::atom::Atom;
use montyc_ast::import::Import;
use montyc_ast::primary::Primary;
use montyc_ast::spanned::Spanned;
use montyc_lexer::PyToken;
use nom::{branch::alt, IResult};

use crate::comb::whitespace;
use crate::TokenStreamRef;

use super::{expect, expect_many_n, primary};

#[inline]
fn import_from<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Import>> {
    let (stream, tok) = expect(PyToken::Import)(stream)?;
    let (stream, _) = whitespace(stream)?;

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

        remaining = match expect(PyToken::Comma)(remaining) {
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
    let (stream, tok) = expect(PyToken::From)(stream)?;
    let (stream, _) = whitespace(stream)?;

    let (stream, dots) = expect_many_n::<0>(PyToken::Dot)(stream)?;
    let (stream, module) = primary(stream)?;

    let level = dots.len();

    match &module.inner {
        Primary::Atomic(atom) if matches!(atom.inner, Atom::Name(_)) => {}
        Primary::Attribute { .. } => {}
        _ => unreachable!(),
    }

    let (stream, _) = whitespace(stream)?;
    let (stream, _) = expect(PyToken::Import)(stream)?;
    let (stream, _) = whitespace(stream)?;

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

        remaining = match expect(PyToken::Comma)(remaining) {
            Ok((remaining, _)) => remaining,
            Err(_) => break remaining,
        };
    };

    let imprt = Spanned {
        span: tok.span.start..names.last().unwrap().span.end,
        inner: Import::From {
            module,
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
