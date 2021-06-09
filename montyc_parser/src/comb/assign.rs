use nom::{sequence::terminated, IResult};

use crate::ast::models::Assign;
use crate::spanned::Spanned;
use crate::{token::PyToken, TokenSlice};

use super::primary;
use super::{
    core::{expect_, expect_many_n},
    expression,
};

#[inline]
pub fn assignment_unspanned<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Assign> {
    let (stream, Spanned { inner, .. }) = assignment(stream)?;
    Ok((stream, inner))
}

#[inline]
pub fn assignment<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Assign>> {
    let (stream, ident) = terminated(primary, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let mut parsed = terminated(
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    );

    let (stream, kind) = if let Ok((stream, _)) = parsed(stream) {
        let (stream, kind) =
            terminated(expression, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

        (stream, Some(kind))
    } else {
        (stream, None)
    };

    let (stream, _) = terminated(
        expect_(PyToken::Equal),
        expect_many_n::<0>(PyToken::Whitespace),
    )(stream)?;

    let (stream, value) = expression(stream)?;

    let span = ident.span.start..value.span.end;

    let obj = Spanned {
        span,
        inner: Assign {
            name: ident,
            value,
            kind,
        },
    };

    Ok((stream, obj))
}
