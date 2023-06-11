use chumsky::Parser;

fn lex(input: &str) -> Vec<(montyc_lexer::PyToken, montyc_lexer::Span)> {
    let lexer = montyc_lexer::lex(input);
    let sr = montyc_parser::span_interner::SpanInterner::new();

    montyc_parser::token_stream_iter::TokenStreamIter::new(sr.get(input, ()).unwrap(), lexer)
        .map(|res| res.unwrap())
        .collect()
}

fn parse<F, T>(expected: &str, f: F)
where
    F: FnOnce() -> T,
    T: serde::Serialize,
{
    let expected: serde_json::Value =
        serde_json::from_str(expected).expect("expected json must always be valid");

    let actual =
        serde_json::to_value(f()).expect("failed to serialise parser output to json Value");

    if expected != actual {
        eprintln!(
            "{pretty}",
            pretty = serde_json::to_string_pretty(&actual).unwrap()
        );
    }

    assert_eq!(expected, actual);
}

#[test]
fn test_parse_expr_integer() {
    parse(include_str!("expr_integer/expected.json"), || {
        montyc_parser::comb::expr()
            .parse(lex(include_str!("expr_integer/input.py")))
            .expect("failed to parse expr")
    })
}

#[test]
fn test_parse_expr_await() {
    parse(include_str!("expr_await/expected.json"), || {
        montyc_parser::comb::expr()
            .parse(lex(include_str!("expr_await/input.py")))
            .expect("failed to parse expr")
    })
}

#[test]
fn test_parse_statement_funcdef() {
    parse(include_str!("statement_funcdef/expected.json"), || {
        montyc_parser::comb::statement(0)
            .parse(lex(include_str!("statement_funcdef/input.py")))
            .expect("failed to parse statement")
    })
}

#[test]
fn test_parse_statement_classdef() {
    parse(include_str!("statement_classdef/expected.json"), || {
        montyc_parser::comb::statement(0)
            .parse_recovery_verbose(lex(include_str!("statement_classdef/input.py")))
            .0
            .expect("failed to parse statement")
    })
}
