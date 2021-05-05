use nom::IResult;

use crate::ast::{expr::Expr, Spanned};
use crate::parser::TokenSlice;

#[inline]
pub fn block<'a>(_stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<Spanned<Expr>>> {
    todo!()

    // #[inline]
    // fn invalid_block<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    //     terminated(expect_(PyToken::Newline), not(expect_(PyToken::Whitespace)))(stream)
    // }

    // let mut indented_block = preceded(
    //     expect_(PyToken::Newline),
    //     preceded(many0(expect_(PyToken::Whitespace)), statement),
    // );

    // if let Ok((stream, stmt)) = indented_block(stream) {
    //     return Ok((stream, vec![stmt]));
    // } else if let Ok((stream, stmt)) =
    //     preceded(expect_many_n::<0>(PyToken::Whitespace), simple_stmt)(stream)
    // {
    //     return Ok((stream, vec![stmt]));
    // } else if let Ok((_, _)) = invalid_block(stream) {
    //     todo!("raise an indentation error here.");
    // } else {
    //     unimplemented!("{:?}", stream)
    // }
}
