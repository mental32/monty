use nom::sequence::tuple;
use nom::{branch::alt, IResult};

use crate::ast::models::{Atom, Expr};
use crate::comb::{expect, expect_any_of, expect_many_n, expect_with, whitespace};
use crate::spanned::Spanned;
use crate::token::PyToken;
use crate::TokenStreamRef;

#[inline]
fn expect_digits<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<PyToken>> {
    expect_with(stream, |(tok, _)| matches!(tok, PyToken::Digits(_)))
}

#[inline]
pub fn expect_ident<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<PyToken>> {
    expect_with(stream, |(tok, _)| matches!(tok, PyToken::Ident(_)))
}

#[inline]
pub(crate) fn name<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Atom>> {
    let (stream, ident) = expect_ident(stream)?;

    let name = if let PyToken::Ident(name) = ident.inner {
        Spanned {
            span: ident.span,
            inner: Atom::Name(name),
        }
    } else {
        unreachable!();
    };

    Ok((stream, name))
}

#[inline]
fn integer<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Atom>> {
    let (stream, digits) = expect_digits(stream)?;

    let int = if let PyToken::Digits(inner) = digits.inner {
        Spanned {
            span: digits.span,
            inner: Atom::Int(inner),
        }
    } else {
        unreachable!();
    };

    Ok((stream, int))
}

#[inline]
fn float<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Atom>> {
    let (stream, (left, _, right)) =
        tuple((expect_digits, expect(PyToken::Dot), expect_digits))(stream)?;

    if let (PyToken::Digits(int), PyToken::Digits(real)) = (left.inner, right.inner) {
        // TODO(mental): figure out a way to parse floats more sensibly.
        let value = format!("{}.{}", int, real)
            .parse()
            .expect("Unable to parse float literal.");

        let float = Atom::Float(value);
        let span = left.span.start..right.span.end;

        Ok((stream, Spanned { span, inner: float }))
    } else {
        unreachable!()
    }
}

#[inline]
fn string_ref<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Atom>> {
    let (stream, ident) = expect_with(stream, |(t, _)| matches!(t, PyToken::StringRef(_)))?;

    let name = if let PyToken::StringRef(name) = ident.inner {
        Spanned {
            span: ident.span,
            inner: Atom::Str(name),
        }
    } else {
        unreachable!();
    };

    Ok((stream, name))
}

#[inline]
pub fn tuple_literal_inner<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Vec<Spanned<Expr>>> {
    let (stream, first) = super::expr::expression(stream)?;
    let (stream, _) = whitespace(stream).unwrap_or((stream, ()));
    let (stream, _) = match expect(PyToken::Comma)(stream) {
        Ok(i) => i,
        Err(e) => return Err(e),
    };

    let mut values = vec![first];

    let (mut stream, _) = whitespace(stream).unwrap_or((stream, ()));

    while let Ok((s, expr)) = super::expr::expression(stream) {
        values.push(expr);

        let (s, _) = expect_many_n::<0>(PyToken::Whitespace)(s).unwrap_or((s, vec![]));

        let (s, _) = match expect(PyToken::Comma)(s) {
            Ok(i) => i,
            Err(_) => {
                stream = s;
                break;
            }
        };

        let (s, _) = expect_many_n::<0>(PyToken::Whitespace)(s).unwrap_or((s, vec![]));

        stream = s;
    }

    let (stream, _) = whitespace(stream).unwrap_or((stream, ()));

    Ok((stream, values))
}

#[inline]
fn tuple_literal<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Atom>> {
    let (stream, lparen) = expect(PyToken::LParen)(stream)?;
    let (stream, _) = whitespace(stream).unwrap_or((stream, ()));
    let (stream, values) = tuple_literal_inner(stream)?;
    let (stream, rparen) = expect(PyToken::RParen)(stream)?;

    let tple = Spanned {
        inner: Atom::Tuple(values),
        span: lparen.span.start..rparen.span.end,
    };

    Ok((stream, tple))
}

#[inline]
pub fn atom_unspanned<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Atom> {
    let (stream, Spanned { inner, .. }) = atom(stream)?;
    Ok((stream, inner))
}

#[inline]
pub fn atom<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Atom>> {
    use PyToken::{Ellipsis, False, None, True};

    let fallback = |stream| {
        let (stream, Spanned { span, inner }) =
            expect_any_of([Ellipsis, True, False, None])(stream)?;

        let atom = Spanned {
            span,
            inner: Atom::from(inner),
        };

        Ok((stream, atom))
    };

    let (stream, atom) = alt((name, float, string_ref, integer, tuple_literal, fallback))(stream)?;

    Ok((stream, atom))
}
