use montyc_ast::atom::Atom;
use montyc_ast::classdef::ClassDef;
use montyc_ast::expr::Expr;
use montyc_ast::primary::Primary;
use montyc_ast::spanned::Spanned;
use nom::IResult;

use crate::comb::whitespace;
use crate::TokenStreamRef;

use montyc_lexer::PyToken;

use super::{atom, chomp, expect, expect_many_n, stmt::statement};

#[inline]
pub fn class_def<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<ClassDef>> {
    // decorators on the def "@foo\n@bar"

    let (stream, decorators) = decorator_list(stream)?;

    // head of ClassDef "class <name>:"

    let (stream, tok) = expect(PyToken::ClassDef)(stream)?;
    let (stream, _) = whitespace(stream)?;
    let (stream, name) = atom(stream)?;

    let name = name;

    let start = decorators
        .get(0)
        .map(|t| t.span.start.clone())
        .unwrap_or(tok.span.start);

    let (stream, _) = whitespace(stream)?;
    let (stream, _) = expect(PyToken::Colon)(stream)?;
    let (mut stream, _) = whitespace(stream)?;

    // body of ClassDef

    let mut body = vec![];

    if let Ok((s, _)) = expect(PyToken::Newline)(stream) {
        stream = s;
        loop {
            if let Ok((s, _)) = expect_many_n::<4>(PyToken::Whitespace)(stream) {
                let (s, stmt) = statement(s)?;
                body.push(stmt);
                let (s, _) = expect_many_n::<0>(PyToken::Newline)(s)?;
                stream = s;
            } else {
                break;
            }
        }
    } else {
        let (_s, stmt) = statement(stream)?;
        body.push(stmt);
    }

    let end = body
        .last()
        .map(|s| s.span.end)
        .unwrap_or(name.span.end.clone());

    let def = Spanned {
        inner: ClassDef {
            name,
            // decorator_list: decorators,
            body,
        },
        span: start..end,
    };

    Ok((stream, def))
}

pub(super) fn decorator<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Primary>> {
    let (stream, _at) = expect(PyToken::At)(stream)?;
    let (stream, dec) = super::primary(stream)?;

    match &dec.inner {
        Primary::Atomic(atom)
            if matches!(
                atom,
                Spanned {
                    inner: Atom::Name(_),
                    ..
                }
            ) => {}
        Primary::Attribute { .. } => {}
        Primary::Call { .. } => {}
        _ => unreachable!(),
    }

    Ok((stream, dec))
}

pub(super) fn decorator_list<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Vec<Spanned<Expr>>> {
    let mut list = vec![];
    let mut stream = stream;

    loop {
        if let Ok((tail, dec)) = decorator(stream) {
            list.push(dec.replace_with(Expr::Primary));

            let (tail, _refs) = chomp(tail).unwrap_or((tail, vec![]));

            stream = tail;
        } else {
            break;
        }
    }

    Ok((stream, list))
}
