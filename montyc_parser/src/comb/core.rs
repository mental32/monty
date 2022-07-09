use nom::{
    error::{Error, ErrorKind},
    multi::{many0, many1, many_m_n},
    Err, IResult,
};

use crate::{spanned::Spanned, token::PyToken, TokenStreamRef};

macro_rules! nom_err {
    ($input:expr, $code:expr) => {
        Err(Err::Error(Error::new($input, $code)))
    };
}

#[inline]
pub fn expect_many_n<const N: usize>(
    value: PyToken,
) -> impl for<'this, 'source, 'data> Fn(
    TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    Vec<Spanned<PyToken>>,
> {
    move |stream| match N {
        0 => many0(expect(value))(stream),
        1 => many1(expect(value))(stream),
        m => many_m_n(m, m.saturating_add(1), expect(value))(stream),
    }
}

#[inline]
pub fn expect_many_n_var(
    n: usize,
    value: PyToken,
) -> impl for<'this, 'source, 'data> Fn(
    TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    Vec<Spanned<PyToken>>,
> {
    move |stream| match n {
        0 => many0(expect(value))(stream),
        1 => many1(expect(value))(stream),
        m => many_m_n(m, m.saturating_add(1), expect(value))(stream),
    }
}

#[inline]
pub fn expect(
    value: PyToken,
) -> impl for<'this, 'source, 'data> Fn(
    TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    Spanned<PyToken>,
> {
    move |stream: TokenStreamRef<'_, '_, '_>| expect_with(stream, |(t, _)| *t == value)
}

#[inline]
pub fn expect_any_of<const N: usize>(
    values: [PyToken; N],
) -> impl for<'this, 'source, 'data> Fn(
    TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    Spanned<PyToken>,
> {
    move |stream: TokenStreamRef<'_, '_, '_>| {
        for token in values.iter() {
            match expect(*token)(stream) {
                Ok(r) => return Ok(r),
                Err(_) => continue,
            }
        }

        Err(Err::Error(Error::new(stream, ErrorKind::Alt)))
    }
}

#[inline]
pub fn expect_any_token<const N: usize>(
    values: [PyToken; N],
) -> impl for<'this, 'source, 'data> Fn(
    TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    Spanned<PyToken>,
> {
    move |stream: TokenStreamRef<'_, '_, '_>| {
        for token in values.iter() {
            match expect(*token)(stream) {
                Ok(r) => return Ok(r),
                Err(_) => continue,
            }
        }

        Err(Err::Error(Error::new(stream, ErrorKind::Alt)))
    }
}

#[inline]
pub fn expect_with<'this, 'source, 'data, F>(
    tokens: TokenStreamRef<'this, 'source, 'data>,
    predicate: F,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<PyToken>>
where
    F: Fn((&PyToken, &logos::Span)) -> bool,
{
    let TokenStreamRef {
        stream,
        tokens_slice_start,
    } = tokens.clone();

    let token_slice_is_empty = stream.borrow().tokens[tokens_slice_start..].is_empty();

    if token_slice_is_empty && tokens.grow().is_none() {
        return nom_err!(tokens, ErrorKind::Eof);
    }

    let token_slice = &stream.borrow().tokens[tokens_slice_start..];

    assert!(!token_slice.is_empty());

    match token_slice {
        [(token, span), ..] => {
            if predicate((token, span)) {
                let (token, span) = (token.clone(), span.clone());
                let out = Spanned { span, inner: token };
                let inp = TokenStreamRef {
                    stream,
                    tokens_slice_start: tokens_slice_start + 1,
                };

                Ok((inp, out))
            } else {
                nom_err!(tokens, ErrorKind::IsNot)
            }
        }

        [] => unreachable!(),
    }
}

pub fn expect_wrapped_values<const N: usize>(
    values: [PyToken; N],
    wrapper: PyToken,
) -> impl for<'this, 'source, 'data> Fn(
    TokenStreamRef<'this, 'source, 'data>,
) -> IResult<
    TokenStreamRef<'this, 'source, 'data>,
    Vec<Spanned<PyToken>>,
> {
    if N == 0 {
        panic!("Must proved a value array that contains at least one token, not zero.")
    }

    move |stream| {
        let (mut stream, _) = expect_many_n::<0>(wrapper)(stream)?;

        let mut results = Vec::with_capacity(N);

        for token in values.iter() {
            let (s, obj) = expect(*token)(stream)?;
            stream = s;
            results.push(obj)
        }

        let (stream, _) = expect_many_n::<0>(wrapper)(stream)?;

        Ok((stream, results))
    }
}
