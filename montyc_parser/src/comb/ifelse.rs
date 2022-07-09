use nom::{
    sequence::{terminated, tuple},
    IResult,
};

use crate::{
    ast::{If, IfChain},
    comb::{expect_many_n_var, whitespace},
    spanned::Spanned,
    token::PyToken,
    TokenStreamRef,
};

use super::{expect, expect_many_n, expression, stmt::statement};

#[inline]
pub fn if_stmt<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<IfChain>> {
    let (stream, token) = match expect(PyToken::If)(stream) {
        Ok((stream, tok)) => {
            log::trace!("parser:if_stmt parsing If");
            (stream, tok)
        }

        Err(err) => return Err(err),
    };

    let (mut stream, (_, test, _, _, _)) = tuple((
        whitespace,
        expression,
        whitespace,
        expect(PyToken::Colon),
        whitespace,
    ))(stream)?;

    // body of the function

    let mut body = vec![];

    let (stream, if_obj) = if let Ok((s, stmt)) = statement(stream) {
        body.push(stmt);

        let if_obj = token.map(|_| If { test, body });

        (
            s,
            IfChain {
                branches: vec![if_obj],
                orelse: None,
            },
        )
    } else {
        let mut inner_indent_level = None;

        loop {
            let (remaining, _) = expect_many_n::<0>(PyToken::Newline)(stream)?;

            let remaining = if inner_indent_level.is_none() {
                let (_, indent) = expect_many_n::<0>(PyToken::Whitespace)(remaining)?;

                inner_indent_level.replace(indent.len());

                remaining
            } else {
                remaining
            };

            if let Ok((remaining, _)) =
                expect_many_n_var(inner_indent_level.unwrap(), PyToken::Whitespace)(remaining)
            {
                let (remaining, part) = match statement(remaining) {
                    Ok(i) => i,
                    Err(_) => break,
                };

                body.push(part);
                stream = remaining;
            } else {
                break;
            }
        }

        let mut if_chain = IfChain {
            branches: vec![token.map(|_| If { test: test, body })],
            orelse: None,
        };

        let mut stream = stream;

        let mut outer_indent_level = None;

        'elif: loop {
            let (s, _) = expect_many_n::<0>(PyToken::Newline)(stream)?;

            let s = if outer_indent_level.is_none() {
                let (_, indent) = expect_many_n::<0>(PyToken::Whitespace)(s)?;

                outer_indent_level.replace(indent.len());

                s
            } else {
                s
            };

            if let Ok((s, _)) =
                expect_many_n_var(outer_indent_level.unwrap(), PyToken::Whitespace)(s)
            {
                let elif = match expect(PyToken::Elif)(s) {
                    Ok(inner) => inner,
                    Err(_) => break 'elif,
                };

                let (s, ref elif_) = elif;
                let (mut elif_stream, (_, test, _, _, _)) = tuple((
                    whitespace,
                    expression,
                    whitespace,
                    expect(PyToken::Colon),
                    whitespace,
                ))(s)?;

                let mut elif_body = vec![];

                loop {
                    if let Ok((remaining, _)) = terminated(
                        expect(PyToken::Newline),
                        expect_many_n::<4>(PyToken::Whitespace),
                    )(elif_stream)
                    {
                        let (remaining, part) = match statement(remaining) {
                            Ok(i) => i,
                            Err(_) => break,
                        };

                        elif_body.push(part);
                        elif_stream = remaining;
                    } else {
                        break;
                    }
                }

                let elif = Spanned {
                    span: elif_.span.clone(),
                    inner: If {
                        test,
                        body: elif_body,
                    },
                };

                if_chain.branches.push(elif);

                stream = elif_stream;
            } else {
                break;
            }
        }

        let (else_stream, _) =
            expect_many_n::<0>(PyToken::Newline)(stream).unwrap_or((stream, vec![]));

        let else_stream = if outer_indent_level.is_none() {
            let (_, indent) = expect_many_n::<0>(PyToken::Whitespace)(else_stream)?;

            outer_indent_level.replace(indent.len());

            else_stream
        } else {
            else_stream
        };

        let (else_stream, _) =
            expect_many_n_var(outer_indent_level.unwrap(), PyToken::Whitespace)(else_stream)
                .unwrap_or((else_stream, vec![]));

        if let Ok((stream, _)) = expect(PyToken::Else)(else_stream) {
            let (mut stream, (_, _, _)) =
                tuple((whitespace, expect(PyToken::Colon), whitespace))(stream)?;

            let mut else_body = vec![];

            let mut inner_indent_level = None;

            loop {
                let (inner, _) = expect_many_n::<0>(PyToken::Newline)(stream).unwrap();

                let inner = if inner_indent_level.is_none() {
                    let (_, indent) = expect_many_n::<0>(PyToken::Whitespace)(inner).unwrap();

                    inner_indent_level.replace(indent.len());

                    inner
                } else {
                    inner
                };

                if let Ok((remaining, _)) =
                    expect_many_n_var(inner_indent_level.unwrap(), PyToken::Whitespace)(inner)
                {
                    let (inner, part) = match statement(remaining) {
                        Ok(i) => i,
                        Err(_) => break,
                    };

                    else_body.push(part);
                    stream = inner;
                } else {
                    break;
                }
            }

            // todo!("{:#?}", stream);

            if_chain.orelse = Some(else_body);

            (stream, if_chain)
        } else {
            (stream, if_chain)
        }
    };

    return Ok((
        stream,
        Spanned {
            span: if_obj.branches[0].span.start..if_obj.branches.last().unwrap().span.end,
            inner: if_obj,
        },
    ));
}
