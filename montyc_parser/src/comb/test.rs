use super::*;

use chumsky::Parser;
use montyc_ast::{AstNode, AstObject};
use montyc_lexer::Span;

fn lex(input: &str) -> Vec<(PyToken, Span)> {
    let lexer = montyc_lexer::lex(input);
    let sr = crate::span_interner::SpanInterner::new();

    crate::token_stream_iter::TokenStreamIter {
        bound: sr.get(input, ()).unwrap(),
        lexer,
        previous: None,
    }
    .map(|res| res.unwrap())
    .collect()
}

#[track_caller]
fn expect_int<A>(a: A)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Int(_) => (),
        n => panic!("{n:#?}"),
    }
}

#[track_caller]
fn expect_tuple<A>(a: A)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Tuple(Atom::Tuple(_)) => (),
        n => panic!("{n:#?}"),
    }
}

#[track_caller]
fn expect_bool<A>(a: A)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Bool(_) => (),
        n => panic!("{n:#?}"),
    }
}

#[track_caller]
fn expect_name<A>(a: A)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Name(_) => (),
        n => panic!("{n:#?}"),
    }
}

#[track_caller]
fn expect_attr<A>(a: A) -> (Box<Spanned<Primary>>, Box<Spanned<Primary>>)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Attr(Primary::Attribute { left, attr }) => (left, attr),
        n => panic!("{n:#?}"),
    }
}

#[track_caller]
fn expect_call<A>(a: A) -> (Box<Spanned<Primary>>, Option<Vec<Spanned<Expr>>>)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Call(Primary::Call { func, args }) => (func, args),
        n => panic!("{n:#?}"),
    }
}

#[track_caller]
fn expect_subscript<A>(a: A) -> (Box<Spanned<Primary>>, Box<Spanned<Expr>>)
where
    A: AstObject,
{
    let node = a.into_ast_node();

    match node {
        AstNode::Subscript(Primary::Subscript { value, index }) => (value, index),
        n => panic!("{n:#?}"),
    }
}

#[test]
pub fn atom_parser_works() {
    let stream = lex("1");
    let out = (super::atom()).parse(stream).unwrap().inner;
    expect_int(out);

    let stream = lex("(1,2,3)");
    let out = (super::atom())
        .parse_recovery_verbose(stream)
        .0
        .unwrap()
        .inner;
    expect_tuple(out);
}

#[test]
pub fn primary_parsing_dot_access() {
    let stream = lex("a.b");
    let out = super::expr().parse(stream).unwrap();

    let (left, right) = expect_attr(out);
    expect_name(*left);
    expect_name(*right);

    let stream = lex("a.b.c");
    let out = super::expr().parse(stream).unwrap();

    let (left, right) = expect_attr(out);
    expect_name(*left);
    let (left, right) = expect_attr(*right);
    expect_name(*left);
    expect_name(*right);
}

#[test]
pub fn primary_parsing_call_expr() {
    let stream = lex("a()");
    let out = super::expr().parse(stream).unwrap();

    let (func, args) = Some(out).map(expect_call).unwrap();
    expect_name(*func);
    assert!(args.is_none(), "{args:?}");

    let stream = lex("a.b()");
    let out = super::expr().parse(stream).unwrap();

    let (left, right) = Some(out).map(expect_attr).unwrap();
    expect_name(*left);

    let (func, args) = expect_call(*right);
    expect_name(*func);
    assert!(args.is_none(), "{args:?}");
}

#[test]
pub fn primary_parsing_subscript() {
    let stream = lex("a[0]");
    let out = super::expr().parse(stream).unwrap();

    let (obj, index) = expect_subscript(out);
    expect_name(*obj);
    expect_int(*index);
}

#[test]
pub fn import_parsing() {
    let stream = lex("import x");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Import(imp) => match imp.inner {
            montyc_ast::import::Import::Names(names) => assert_eq!(names.len(), 1),
            montyc_ast::import::Import::From { .. } => panic!(),
        },
        _ => panic!(),
    }

    let stream = lex("import x.y");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Import(imp) => match imp.inner {
            montyc_ast::import::Import::Names(names) => assert_eq!(names.len(), 2),
            montyc_ast::import::Import::From { .. } => panic!(),
        },
        _ => panic!(),
    }
}

#[test]
pub fn while_parsing() {
    let stream = lex("while True:\n    1");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::While(whl) => {
            let whl = whl.inner;
            expect_int(whl.body[0].clone());
        }
        _ => panic!(),
    }
}

#[test]
pub fn ifch_parsing() {
    let stream = lex("if x:\n    1");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::If(ifch) => {
            let ifch = ifch.inner;
            assert_eq!(ifch.branches.len(), 1);
            assert!(ifch.orelse.is_none());
        }
        st => panic!("{st:#?}"),
    }

    let stream = lex("if x:\n    1\nelse:\n    2");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::If(ifch) => {
            let ifch = ifch.inner;
            assert_eq!(ifch.branches.len(), 1);
            assert!(ifch.orelse.is_some());
        }
        _ => panic!(),
    }

    let stream = lex("if x:\n    1\nelif True:\n    2");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::If(ifch) => {
            let ifch = ifch.inner;
            assert_eq!(ifch.branches.len(), 2);
            assert!(ifch.orelse.is_none());
        }
        _ => panic!(),
    }

    let stream = lex("if x:\n    1\nelif True:\n    4\nelse:\n    2");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::If(ifch) => {
            let ifch = ifch.inner;
            assert_eq!(ifch.branches.len(), 2);
            assert!(ifch.orelse.is_some());
        }
        _ => panic!(),
    }
}

#[test]
pub fn return_parsing() {
    let stream = lex("return True");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Ret(r) => {
            expect_bool(r.inner.value.unwrap().clone());
        }
        _ => panic!(),
    }
}

#[test]
pub fn annotation_parsing() {
    let stream = lex("a: b");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Ann(asn) => {
            let asn = asn.inner;
            expect_name(asn.name);
            expect_name(asn.annotation);
        }
        _ => panic!(),
    }
}

#[test]
pub fn assign_parsing() {
    let stream = lex("a = b");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Asn(asn) => {
            let asn = asn.inner;
            expect_name(asn.name);
            expect_name(asn.value);
            assert!(asn.annotation.is_none())
        }
        _ => panic!(),
    }

    let stream = lex("a: c = b");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Asn(asn) => {
            let asn = asn.inner;
            expect_name(asn.name);
            expect_name(asn.value);
            assert!(asn.annotation.is_some())
        }
        _ => panic!(),
    }
}

#[test]
pub fn classdef_parsing() {
    let stream = lex("class Foo:\n    pass");
    let out = super::statement(0).parse(stream).unwrap().inner;

    match out {
        Statement::Class(klass) => {
            let klass = klass.inner;
            expect_name(klass.name);
        }
        _ => panic!(),
    }
}

#[test]
pub fn funcdef_parsing() {
    let stream = lex("def foo(bar):\n    (420)\n    def baz():\n        69");
    let (out, err) = super::statement(0).parse_recovery_verbose(stream);

    assert!(err.is_empty(), "{err:#?}");
    let out = out.unwrap().inner;

    match out {
        Statement::FnDef(fndef) => {
            let fndef = fndef.inner;

            assert!(fndef.decorator_list.is_empty());
            assert!(!fndef.args.is_empty());
            expect_name(fndef.name);
            expect_int(fndef.body[0].clone());

            match fndef.body[1].clone().inner {
                Statement::FnDef(fndef) => {
                    let fndef = fndef.inner;

                    expect_name(fndef.name);
                    expect_int(fndef.body[0].clone());
                }
                _ => panic!(),
            }
        }

        _ => panic!(),
    }
}

#[test]
pub fn module_parsing() {
    let stream = lex("a\nb\nc\nd[e]");
    let out = super::module();
    let out = out.parse(stream).unwrap().inner;

    assert_eq!(dbg!(&out.body).len(), 4);

    expect_name(out.body.get(0).cloned().unwrap());
    expect_name(out.body.get(1).cloned().unwrap());
    expect_name(out.body.get(2).cloned().unwrap());

    let (obj, index) = expect_subscript(out.body.get(3).cloned().unwrap());
    expect_name(*obj);
    expect_name(*index);
}
