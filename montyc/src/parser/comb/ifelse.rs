#![allow(warnings)]

use std::rc::Rc;

use nom::{
    sequence::{terminated, tuple},
    IResult,
};

use crate::{
    ast::{
        atom::Atom,
        ifelif::{BranchTail, If, IfChain},
        Spanned,
    },
    parser::{token::PyToken, TokenSlice},
};

use super::{atom, chomp, expect, expect_, expect_many_n, expression, stmt::statement};

#[inline]
pub fn if_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<IfChain>> {
    let (stream, token) = match expect(stream, PyToken::If) {
        Ok((stream, tok)) => {
            log::trace!("parser:if_stmt parsing If");
            (stream, tok)
        }

        Err(err) => return Err(err),
    };

    let (mut stream, (_, test, _, _, _)) = tuple((
        expect_many_n::<0>(PyToken::Whitespace),
        expression,
        expect_many_n::<0>(PyToken::Whitespace),
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    ))(stream)?;

    // body of the function

    let mut body = vec![];

    let (stream, if_obj) = if let Ok((s, stmt)) = statement(stream) {
        body.push(Rc::new(stmt) as Rc<_>);

        let if_obj = token.map(|_| If {
            test: Rc::new(test),
            body,
        });

        (s, IfChain {
            branches: vec![Rc::new(if_obj)],
            orelse: None,
        })
    } else {
        loop {
            if let Ok((remaining, _)) = terminated(
                expect_(PyToken::Newline),
                expect_many_n::<4>(PyToken::Whitespace),
            )(stream)
            {
                // panic!("{:?}", remaining);
                let (remaining, part) = match statement(remaining) {
                    Ok(i) => i,
                    Err(e) => break,
                };

                body.push(Rc::new(part) as Rc<_>);
                stream = remaining;
            } else {
                break;
            }
        }

        let mut if_chain = IfChain {
            branches: vec![Rc::new(token.map(|_| If {
                test: Rc::new(test),
                body,
            }))],
            orelse: None,
        };

        let mut stream = stream;

        'elif: loop {
            let (s, nl) = expect_many_n::<0>(PyToken::Newline)(stream).unwrap_or((stream, vec![]));
            let (s, ws) = expect_many_n::<0>(PyToken::Whitespace)(s).unwrap_or((s, vec![]));

            let elif = match (expect(s, PyToken::Elif)) {
                Ok(inner) => inner,
                Err(_) if nl.is_empty() && ws.is_empty() => break 'elif,
                Err(e) => break,
            };

            let (s, ref elif_) = elif;
            let (mut elif_stream, (_, test, _, _, _)) = tuple((
                expect_many_n::<0>(PyToken::Whitespace),
                expression,
                expect_many_n::<0>(PyToken::Whitespace),
                expect_(PyToken::Colon),
                expect_many_n::<0>(PyToken::Whitespace),
            ))(s)?;

            let test = Rc::new(test);

            let mut elif_body = vec![];

            loop {
                if let Ok((remaining, _)) = terminated(
                    expect_(PyToken::Newline),
                    expect_many_n::<4>(PyToken::Whitespace),
                )(elif_stream)
                {
                    let (remaining, part) = match statement(remaining) {
                        Ok(i) => i,
                        Err(e) => break,
                    };

                    elif_body.push(Rc::new(part) as Rc<_>);
                    elif_stream = remaining;
                } else {
                    break;
                }
            }

            let elif = Rc::new(Spanned {
                span: elif_.span.clone(),
                inner: If {
                    test,
                    body: elif_body,
                },
            });

            if_chain.branches.push(elif);

            stream = elif_stream;
        }

        let (stream, nl) = expect_many_n::<0>(PyToken::Newline)(stream).unwrap_or((stream, vec![]));
        let (mut stream, ws) = expect_many_n::<0>(PyToken::Whitespace)(stream).unwrap_or((stream, vec![]));

        if let Ok((else_stream, else_)) = (expect(stream, PyToken::Else)) {
            let (mut else_stream, (_, _, _)) = tuple((
                expect_many_n::<0>(PyToken::Whitespace),
                expect_(PyToken::Colon),
                expect_many_n::<0>(PyToken::Whitespace),
            ))(else_stream)?;

            let mut else_body = vec![];

            loop {
                if let Ok((remaining, _)) = terminated(
                    expect_(PyToken::Newline),
                    expect_many_n::<4>(PyToken::Whitespace),
                )(else_stream)
                {
                    let (remaining, part) = match statement(remaining) {
                        Ok(i) => i,
                        Err(e) => break,
                    };

                    else_body.push(Rc::new(part) as Rc<_>);
                    else_stream = remaining;
                } else {
                    break;
                }
            }

            if_chain.orelse = Some(else_body);

            stream = else_stream;
        }

        (stream, if_chain)
    };

    return Ok((
        stream,
        Spanned {
            span: if_obj.branches[0].span.start..if_obj.branches.last().unwrap().span.end,
            inner: if_obj,
        },
    ));
}
