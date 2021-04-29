use std::rc::Rc;

use nom::{IResult, sequence::tuple};

use crate::{ast::while_::While, parser::TokenSlice, prelude::*};

use super::{expect, expect_, expect_many_n, expression, stmt::statement};

#[inline]
pub fn while_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<While>> {
    let (stream, tok) = expect(stream, PyToken::While)?;

    let (mut stream, (_, test, _, _, _)) = tuple((
        expect_many_n::<0>(PyToken::Whitespace),
        expression,
        expect_many_n::<0>(PyToken::Whitespace),
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    ))(stream)?;

    let test = Rc::new(test);

    let mut body = vec![];

    if let Ok((s, stmt)) = statement(stream) {
        body.push(Rc::new(stmt) as Rc<_>);
        stream = s;
    } else {
        loop {
            let (remainaing, _) = expect_many_n::<0>(PyToken::Newline)(stream)?;

            if let Ok((remaining, _)) = expect_many_n::<4>(PyToken::Whitespace)(remainaing) {
                let (remaining, part) = match statement(remaining) {
                    Ok(i) => i,
                    Err(err) => {
                        if let nom::Err::Error(err) = &err {
                            if let Some((top, _)) = err.input.get(0) {
                                if matches!(top, PyToken::Elif | PyToken::Else) {
                                    break
                                }
                            }
                        }

                        return Err(err);
                    }
                };

                body.push(Rc::new(part) as Rc<_>);
                stream = remaining;
            } else {
                break;
            }
        }
    }
    let while_ = While {
        test,
        body,
    };

    let while_ = Spanned {
        inner: while_,
        span: tok.span,
    };

    Ok((stream, while_))
}