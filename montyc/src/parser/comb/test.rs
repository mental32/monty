use nom::IResult;

use crate::parser::{Parser, TokenSlice, token::PyToken};

use super::*;
// use super::ast::AstObject;

#[inline]
fn assert_parses_exactly<'a, S, F, R>(st: S, f: F) -> Vec<R>
where
    S: IntoIterator<Item = &'a str>,
    F: for<'b> Fn(TokenSlice<'b>) -> IResult<TokenSlice<'b>, R>,
    R: std::fmt::Debug
{
    let mut gathered = vec![];

    for string in st.into_iter() {
        let parser = Parser::new(string);
        let stream = parser.token_stream().map(Result::unwrap).collect::<Vec<_>>().into_boxed_slice();

        match f(&stream) {
            Ok(([], obj)) => gathered.push(obj),
            why => panic!("Failed to parse... {:?} ({:#?})", string, why),
        }
    }

    gathered
}

// #[test]
// fn test_parse_statements() {
//     assert_parses_exactly(vec![
//         "if False: pass\nelif True: pass\nelse: return None",
//         "f = 0",
//         "pass",
//         "return object() ** await x + 5",
//         "if True: pass",
//         "def _asFSDF120983_740823570wRCSCE           (                   ):return None +-None",
//     ], statements);
// }

// #[test]
// fn test_parse_statement() {
//     assert_parses_exactly(vec![
//         "f = 0",
//         "pass",
//         "return object() ** await x + 5",
//         "if True: pass",
//         "def _asFSDF120983_740823570wRCSCE           (                   ):return None +-None",
//     ], statement);
// }

#[test]
fn test_parse_spanrefs() {
    assert_parses_exactly(vec![r"# Hello, There!", r#""Hello, There!""#], |stream| {
        let (stream, o) = expect_with(stream, |(tok, _)| matches!(tok, PyToken::SpanRef(_)))?;
        Ok((stream, o))
    });
}


#[test]
fn test_parse_true() {
    match assert_parses_exactly(Some("True"), expect_(PyToken::True)).as_slice() {
        [top, ..] => assert!(matches!(top.inner, PyToken::True)),
        [] => unreachable!(),
    }
}

#[test]
fn test_parse_false() {
    match assert_parses_exactly(Some("False"), expect_(PyToken::False)).as_slice() {
        [top, ..] => assert!(matches!(top.inner, PyToken::False)),
        [] => unreachable!(),
    }
}

#[test]
fn test_parse_assign() {
    assert_parses_exactly(
        vec![
            "_lkALSFDw555ca1=False",
            "Adadsgsawedcasdbvdrsfetgsa:int=7000",
            "pi    :float = 3.14",
            "vibe:              checked=True",
        ],
        assignment,
    );
}

// #[test]
// fn test_parse_funcdef() {
//     assert_parses_exactly(vec!["def foo(): pass"], function_def);
// }

#[test]
fn test_parse_expression() {
    assert_parses_exactly(
        vec![
            // term
            "+await a.b % c()",
            "a   //await b.c()",
            "await False@True[].b.c()",
            // await powers
            // await powers
            "foooooooooo. bar   () ** await 3",
            "await 1 ** 2",
            "await   a               .            b.c() ** d .e .f(  ) . g . h ()",
            "1 ** 1",
            "a ** b",
            "a ** b.c()",
            // primary
            "a.b",
            "f.b.c",
            "f()",
            "a.b()",
            "a.b.c()",
            "a().b[]()",
            "list[]",
            // atoms
            "True",
            "False",
            "None",
            "...",
            "1738",
            "__nom_parser__",
        ],
        expression,
    );
}

#[test]
fn test_parse_primary() {
    assert_parses_exactly(
        vec![
            // await powers
            "foooooooooo. bar   ()",
            "await   1",
            "await   a               .            b.c()  .d .e .f(  ) . g . h ()",
            // primary
            "a.b",
            "f.b.c",
            "f()",
            "a.b()",
            "a.b.c()",
            "a().b[]()",
            // atom
            "list[]",
            "True",
            "False",
            "None",
            "...",
            "1738",
            "__nom_parser__",
        ],
        await_primary,
    );
}

#[test]
fn test_parse_atom() {
    assert_parses_exactly(
        vec![
            "True",
            "False",
            "None",
            "...",
            "1738",
            "__nom_parser__",
        ],
        atom,
    );
}
