// use nom::{IResult, sequence::tuple};

// use crate::ast::{Spanned, ifelif::{BranchTail, If}};
// use crate::parser::{TokenSlice, token::PyToken};
// use super::{block::block, expect_, expect_many_n, expression};


// #[inline]
// fn else_block<'a>(_stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<BranchTail>> {
//     todo!();

//     // use PyToken::{Whitespace, Newline};

//     // let (stream, _) = expect_many_n::<0>(Newline)(stream)?;
//     // let (stream, _) = expect_many_n::<0>(Whitespace)(stream)?;

//     // preceded(
//     //     terminated(expect_(PyToken::Else), expect_(PyToken::Colon)),
//     //     block,
//     // )(stream)
// }

// #[inline]
// fn elif_stmt<'a>(_stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<BranchTail>> {
//     todo!();

//     // let (mut stream, (_, tok, _, test, _, _, body, _)) = tuple((
//     //     expect_many_n::<0>(PyToken::Newline),
//     //     // expect_many_n::<0>(PyToken::Whitespace),
//     //     expect_(PyToken::Elif),
//     //     expect_many_n::<1>(PyToken::Whitespace),
//     //     expression,
//     //     expect_many_n::<0>(PyToken::Whitespace),
//     //     expect_(PyToken::Colon),
//     //     expect_many_n::<0>(PyToken::Whitespace),
//     //     block,
//     // ))(stream)?;

//     // let mut if_node = ast::IfStmt {
//     //     test: Box::new(test),
//     //     body,
//     //     orelse: None,
//     // };

//     // if let Ok((s, stmt)) = elif_stmt(stream) {
//     //     stream = s;
//     //     if_node.orelse = Some(vec![stmt]);
//     // } else if let Ok((s, stmt)) = else_block(stream) {
//     //     stream = s;
//     //     if_node.orelse = Some(stmt);
//     // }

//     // let if_obj = Spanned {
//     //     span: tok.span,
//     //     inner: BranchTail::If(Box::new(if_node)),
//     // };

//     // return Ok((stream, if_obj));
// }

// #[inline]
// pub fn if_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Spanned<If>> {
//     let (stream, (token, _, test, _, _, _, body)) = tuple((
//         expect_(PyToken::If),
//         expect_many_n::<0>(PyToken::Whitespace),
//         expression,
//         expect_many_n::<0>(PyToken::Whitespace),
//         expect_(PyToken::Colon),
//         expect_many_n::<0>(PyToken::Whitespace),
//         block,
//     ))(stream)?;

//     let if_obj = token.map(|_| If {
//         test: Box::new(test),
//         body,
//         orelse: None,
//     });

//     return Ok((stream, if_obj));
// }
