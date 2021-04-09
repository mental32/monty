use std::rc::Rc;

use nom::{IResult, branch::alt};

use crate::{ast::{AstObject, Spanned, funcdef, stmt::{self, Statement}}, parser::{token::PyToken, TokenSlice}};

use super::{assignment, expect_many_n, funcdef::function_def, return_stmt};

#[inline]
fn dyn_assign<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner: assign }) = assignment(stream)?;
    let assign = Spanned { span, inner: Statement::Asn(assign) };

    Ok((stream, assign))
}

#[inline]
fn dyn_return<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner: ret }) = return_stmt(stream)?;
    let ret = Spanned { span, inner: Statement::Ret(ret) };

    Ok((stream, ret))
}

#[inline]
fn dyn_funcdef<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let (stream, Spanned { span, inner }) = function_def(stream)?;
    let fndef = Spanned { span, inner: Statement::FnDef(inner) };

    Ok((stream, fndef))
}

// #[inline]
// fn dyn_pass<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Rc<dyn AstObject>> {
//     let (stream, _) = expect(stream, PyToken::Pass)?;
//     let assign = Rc::new(assign) as Rc<_>;

//     Ok((stream, assign))
// }


#[inline]
fn small_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    alt((dyn_assign, dyn_return))(stream)
}

#[inline]
fn compound_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    dyn_funcdef(stream)
    // alt((
    //     dyn_funcdef,
    //     // if_stmt,
    //     // class_def,
    //     // with_stmt,
    //     // for_stmt,
    //     // try_stmt,
    //     // while_stmt,
    // ))(stream)
}

// #[inline]
// fn simple_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
//     let (stream, block) = alt((
//         many1(terminated(small_stmt, expect_(PyToken::Newline))),
//         |stream| small_stmt(stream).map(|(stream, r)| (stream, vec![r])),
//     ))(stream)?;

//     let obj = AstObject {
//         span: block.iter().next().map(|o| o.span.start).unwrap_or(0)
//             ..block.iter().last().map(|o| o.span.end).unwrap_or(0),
//         inner: AstNode::Block(block),
//     };

//     Ok((stream, obj))
// }

// #[inline]
// pub(crate) fn statement<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
//     let stream = expect_many_n::<0>(PyToken::Whitespace)(stream)
//         .map(|(stream, _)| stream)
//         .unwrap_or(stream);

//     alt((simple_stmt, compound_stmt))(stream)
// }

#[inline]
pub fn statement<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<Statement>> {
    let stream = expect_many_n::<0>(PyToken::Whitespace)(stream)
        .map(|(stream, _)| stream)
        .unwrap_or(stream);

    alt((small_stmt, compound_stmt))(stream)
}
