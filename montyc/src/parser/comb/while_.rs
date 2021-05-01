#![allow(warnings)]

use std::rc::Rc;

use nom::{sequence::tuple, IResult};

use crate::{ast::while_::While, parser::TokenSlice, prelude::*};

use super::{
    expect, expect_, expect_many_n, expect_many_n_var, expression, stmt::statement_unstripped,
};

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

    if let Ok((s, stmt)) = statement_unstripped(stream) {
        body.push(Rc::new(stmt) as Rc<_>);
        stream = s;
    } else {
        let mut indent_level = None;

        loop {
            let (remaining, _) = expect_many_n::<0>(PyToken::Newline)(stream)?;

            let remaining = if indent_level.is_none() {
                let (_, indent) = expect_many_n::<0>(PyToken::Whitespace)(remaining)?;

                indent_level.replace(indent.len());

                remaining
            } else {
                remaining
            };

            if let Ok((remaining, _)) =
                dbg!(expect_many_n_var(indent_level.unwrap(), PyToken::Whitespace)(remaining))
            {
                let (remaining, part) = statement_unstripped(remaining)?;

                log::trace!("parse:while body ++ {:?}", part);

                body.push(Rc::new(part) as Rc<_>);
                stream = remaining;
            } else {
                break;
            }
        }
    }

    let while_ = While { test, body };

    let while_ = Spanned {
        inner: while_,
        span: tok.span,
    };

    Ok((stream, while_))
}
