use chumsky::prelude::*;

use montyc_ast::expr::Expr;
use montyc_ast::funcdef::{FunctionDef, FunctionDefParam};
use montyc_ast::spanned::Spanned;
use montyc_ast::statement::Statement;
use montyc_lexer::PyToken;

use crate::ast::atom::Atom;
use crate::ast::primary::Primary;
use crate::Token;

macro_rules! p {
    ($out:ty; $bound:ident) => {
        impl Parser<Token, $out, Error = Simple<Token>> + $bound + 'static
    };

    ($out:ty) => {
        p!(Token, $out)
    };

    ($i:ty, $out:ty) => {
        impl Parser<$i, $out, Error = Simple<Token>> + 'static
    };
}

mod tokens {
    use super::*;

    macro_rules! token_parser {
        ($i:ident, $debug:literal, $p:pat) => {
            #[track_caller]
            pub fn $i() -> impl Parser<super::Token, Spanned<PyToken>, Error = Simple<Token>> + Clone {
                select! {
                    (e @ $p, span) => Spanned::new(e, span),
                }
                .debug($debug)
            }
        };

        [$( ($i2:ident, $debug2:literal, $p2:pat) );* ] => {
            $(
                token_parser!( $i2, $debug2, $p2 );
            )*
        };
    }

    token_parser![
        (true_,       "tokens::true_",      PyToken::True);
        (false_,      "tokens::false_",     PyToken::False);
        (none,        "tokens::none",       PyToken::None);
        (is,          "tokens::is",         PyToken::Is);
        (or,          "tokens::or",         PyToken::Or);
        (not,         "tokens::not",        PyToken::Not);
        (await_,      "tokens::await",      PyToken::Await);
        (async_,      "tokens::async",      PyToken::Async);
        (if_,         "tokens::if",         PyToken::If);
        (elif,        "tokens::elif",       PyToken::Elif);
        (else_,       "tokens::else",       PyToken::Else);
        (classdef,    "tokens::classdef",   PyToken::ClassDef);
        (fndef,       "tokens::fndef",      PyToken::FnDef);
        (return_,     "tokens::return",     PyToken::Return);
        (while_,      "tokens::while",      PyToken::While);
        (pass,        "tokens::pass",       PyToken::Pass);
        (continue_,   "tokens::continue",   PyToken::Continue);
        (in_,         "tokens::in",         PyToken::In);
        (break_,      "tokens::break",      PyToken::Break);
        (from,        "tokens::from",       PyToken::From);
        (import,      "tokens::import",     PyToken::Import);
        (raise,       "tokens::raise",      PyToken::Raise);
        (assert,      "tokens::assert",     PyToken::Assert);
        (del,         "tokens::del",        PyToken::Del);
        (global,      "tokens::global",     PyToken::Global);
        (yield_,      "tokens::yield",      PyToken::Yield);
        (nonlocal,    "tokens::nonlocal",   PyToken::Nonlocal);
        (ellipsis,    "tokens::ellipsis",   PyToken::Ellipsis);
        (lparen,      "tokens::lparen",     PyToken::LParen);
        (rparen,      "tokens::rparen",     PyToken::RParen);
        (lbracket,    "tokens::lbracket",   PyToken::LBracket);
        (rbracket,    "tokens::rbracket",   PyToken::RBracket);
        (lbrace,      "tokens::lbrace",     PyToken::LBrace);
        (rbrace,      "tokens::rbrace",     PyToken::RBrace);
        (at,          "tokens::at",         PyToken::At);
        (escape,      "tokens::escape",     PyToken::Escape);
        (div,         "tokens::div",        PyToken::Div);
        (caret,       "tokens::caret",      PyToken::Caret);
        (comma,       "tokens::comma",      PyToken::Comma);
        (equal,       "tokens::equal",      PyToken::Equal);
        (dot,         "tokens::dot",        PyToken::Dot);
        (and,         "tokens::and",        PyToken::And);
        (colon,       "tokens::colon",      PyToken::Colon);
        (plus,        "tokens::plus",       PyToken::Plus);
        (minus,       "tokens::minus",      PyToken::Minus);
        (star,        "tokens::star",       PyToken::Star);
        (tilde,       "tokens::tilde",      PyToken::Tilde);
        (modulo,      "tokens::modulo",     PyToken::Modulo);
        (lessthan,    "tokens::lessthan",   PyToken::LessThan);
        (pipe,        "tokens::pipe",       PyToken::Pipe);
        (greaterthan, "tokens::greaterthan",PyToken::GreaterThan);
        (bang,        "tokens::bang",       PyToken::Bang);
        (newline,     "tokens::newline",    PyToken::Newline);
        (formfeed,    "tokens::formfeed",   PyToken::FormFeed);
        (whitespace,  "tokens::whitespace", PyToken::Whitespace)
    ];
}

#[track_caller]
pub fn ident() -> p!(Spanned<Atom>; Clone) {
    select! {
        (PyToken::IdentRef(sr), span) => Spanned::new(Atom::Name(sr), span)
    }
    .debug("ident")
}

#[track_caller]
pub fn true_() -> p!(Spanned<Atom>; Clone) {
    select! {
        (PyToken::True, span) => Spanned::new(Atom::Bool(true), span)
    }
    .debug("True")
}

#[track_caller]
pub fn false_() -> p!(Spanned<Atom>; Clone) {
    select! {
        (PyToken::False, span) => Spanned::new(Atom::Bool(false), span)
    }
    .debug("False")
}

#[track_caller]
pub fn none() -> p!(Spanned<Atom>; Clone) {
    select! {
        (PyToken::None, span) => Spanned::new(Atom::None, span)
    }
    .debug("None")
}

#[track_caller]
pub fn int() -> p!(Spanned<Atom>; Clone) {
    select! {
        (PyToken::Digits(n), span) => Spanned::new(Atom::Int(n), span)
    }
    .debug("int")
}

#[track_caller]
pub fn ellipsis() -> p!(Spanned<Atom>; Clone) {
    select! {
        (PyToken::Ellipsis, span) => Spanned::new(Atom::Ellipsis, span),
    }
    .debug("Ellipsis")
}

#[track_caller]
pub fn atom() -> p!(Spanned<Atom>; Clone) {
    let val = ident()
        .or(true_())
        .or(false_())
        .or(none())
        .or(int())
        .or(ellipsis())
        .debug("atom.value");

    recursive(|atom| {
        let items = atom
            .padded_by(tokens::whitespace().repeated())
            .separated_by(tokens::comma())
            .allow_trailing()
            .at_least(1)
            .debug("atom.items");

        let tuple = items
            .delimited_by(tokens::lparen(), tokens::rparen())
            .map(|i: Vec<Spanned<Atom>>| {
                let tuple = Atom::Tuple(
                    i.into_iter()
                        .map(|i| i.replace_with(Primary::Atomic).replace_with(Expr::Primary))
                        .collect::<Vec<Spanned<Expr>>>(),
                );
                Spanned::new(tuple, Default::default())
            })
            .debug("atom.tuple");

        val.clone()
            .or(val.delimited_by(tokens::lparen(), tokens::rparen()))
            .or(tuple)
    })
    .debug("atom()")
}

#[track_caller]
pub fn primary() -> impl Parser<Token, Spanned<Primary>, Error = Simple<Token>> {
    recursive(|pr| {
        let wrapped_atom = atom()
            .map(|at| at.replace_with(Primary::Atomic))
            .debug("primary.wrapped_atom");

        fn dotted<P, Q>(
            recurse: P,
            wrapped_atom: Q,
        ) -> impl Parser<Token, Spanned<Primary>, Error = Simple<Token>>
        where
            P: Parser<Token, Spanned<Primary>, Error = Simple<Token>>,
            Q: Parser<Token, Spanned<Primary>, Error = Simple<Token>> + Clone,
        {
            let dot = select! {
                (PyToken::Dot, _) => PyToken::Dot,
            };

            // <dotted> := <wAtom> DOT <primary>+
            // <primary> := <dotted> | <wAtom>
            wrapped_atom
                .clone()
                .then(
                    dot.ignored()
                        .then(recurse.or(wrapped_atom))
                        .map(|((), r)| r)
                        .repeated()
                        .at_least(1),
                )
                .foldl(|base, attr| {
                    let left = Box::new(base);
                    let attr = Box::new(attr);

                    let span = left.span_to(&attr);
                    let t = Primary::Attribute { left, attr };

                    Spanned::new(t, span)
                })
        }

        fn call_expr<P>(
            wrapped_atom: P,
        ) -> impl Parser<Token, Spanned<Primary>, Error = Simple<Token>>
        where
            P: Parser<Token, Spanned<Primary>, Error = Simple<Token>>,
        {
            let whitespace = tokens::whitespace().repeated();

            let items = atom()
                .map(|i| i.replace_with(Primary::Atomic).replace_with(Expr::Primary))
                .or(expr())
                .padded_by(whitespace)
                .separated_by(tokens::comma())
                .at_least(1);

            wrapped_atom
                .then_ignore(tokens::lparen())
                .then(items.or_not())
                .then_ignore(tokens::rparen())
                .map(|(base, args)| {
                    let func = Box::new(base);

                    let span = func.span.clone();
                    let t = Primary::Call { func, args };

                    Spanned::new(t, span)
                })
        }

        fn subscript<P>(
            wrapped_atom: P,
        ) -> impl Parser<Token, Spanned<Primary>, Error = Simple<Token>>
        where
            P: Parser<Token, Spanned<Primary>, Error = Simple<Token>>,
        {
            let lbracket = select! {
                (PyToken::LBracket, _) => PyToken::LBracket,
            };

            let rbracket = select! {
                (PyToken::RBracket, _) => PyToken::RBracket,
            };

            wrapped_atom
                .then(expr().delimited_by(lbracket, rbracket))
                .map(|(value, index)| {
                    let value = Box::new(value);
                    let index = Box::new(index);

                    let span = value.span_to(&index);
                    let t = Primary::Subscript { value, index };

                    Spanned::new(t, span)
                })
        }

        let whitespace = select! {
            (PyToken::Whitespace, _) => PyToken::Whitespace,
        }
        .repeated();

        let await_ = select! {
            (PyToken::Await, _) => PyToken::Await,
        };

        let awaited = await_
            .then_ignore(whitespace)
            .repeated()
            .at_least(1)
            .then(pr.clone())
            .map(|(mut waits, primary): (Vec<PyToken>, Spanned<Primary>)| {
                let span = primary.span.clone();
                let mut acc = if let Some(_) = waits.pop() {
                    Primary::Await(Box::new(primary))
                } else {
                    unreachable!()
                };

                while let Some(_) = waits.pop() {
                    acc = Primary::Await(Box::new(Spanned::new(acc, span.clone())))
                }

                Spanned::new(acc, span.clone())
            });

        awaited
            .or(dotted(pr.clone(), wrapped_atom.clone()))
            .or(call_expr(wrapped_atom.clone()))
            .or(subscript(wrapped_atom.clone()))
    })
    .debug("primary()")
}

pub fn expr() -> impl Parser<Token, Spanned<crate::ast::expr::Expr>, Error = Simple<Token>> {
    let wrapped_atom = atom().map(|at| at.replace_with(Primary::Atomic));

    wrapped_atom.map(|p| p.replace_with(Expr::Primary))
}

/// annotated_identifier := <ident> [":" <expr>]
/// annotated_identifier_list = (<annotated_identifier> ",")*
pub fn annotated_identifier_list(
) -> impl Parser<Token, Vec<Spanned<FunctionDefParam>>, Error = Simple<Token>> {
    ident()
        .then(tokens::colon().ignore_then(expr()).or_not())
        .map(|(arg, ann)| {
            let span = arg.span.start..ann.as_ref().map(|ann| ann.span.end).unwrap_or(arg.span.end);
            let param = FunctionDefParam {
                named: arg.inner.as_name().unwrap(),
                annotation: ann,
            };

            Spanned::new(param, span)
        })
        .separated_by(tokens::comma())
}

#[track_caller]
pub fn indented_block<F, P, T>(prefix: usize, f: F) -> p!(Vec<T>)
where
    F: Fn() -> P + 'static,
    P: Parser<Token, T, Error = Simple<Token>> + 'static,
    T: 'static,
{
    tokens::whitespace()
        .repeated()
        .exactly(prefix)
        .then_with(move |_| f())
        .separated_by(tokens::newline())
        .at_least(1)
        .debug("indented_block")
}

pub fn funcdef(indent: usize) -> impl Parser<Token, Spanned<FunctionDef>, Error = Simple<Token>> {
    let parameters = annotated_identifier_list().delimited_by(tokens::lparen(), tokens::rparen());

    let return_type_annotation = tokens::minus()
        .ignore_then(tokens::greaterthan())
        .ignore_then(tokens::whitespace().repeated().or_not())
        .ignore_then(expr());

    let decorator = tokens::at()
        .ignore_then(primary())
        .debug("funcdef.decorator");

    let decorator_list = decorator
        .separated_by(tokens::newline())
        .debug("funcdef.decorator_list");

    let body =
        tokens::newline().ignore_then(indented_block(indent + 4, move || statement(indent + 4)));

    decorator_list
        .then(tokens::fndef())
        .then_ignore(tokens::whitespace().repeated().or_not())
        .then(ident())
        .then_ignore(tokens::whitespace().repeated().or_not())
        .then(parameters)
        .then_ignore(tokens::whitespace().repeated().or_not())
        .then(return_type_annotation.or_not())
        .then_ignore(tokens::whitespace().repeated().or_not())
        .then_ignore(tokens::colon())
        .then_ignore(tokens::whitespace().repeated().or_not())
        .then(body)
        .map(
            |(((((decorator_list, fndef), name), args), returns), body)| {
                (decorator_list, fndef, name, args, returns, body)
            },
        )
        .map(
            |(decorator_list, fndef, name, args, returns, body): (
                Vec<Spanned<Primary>>,          // ["@" <primary>]*
                Spanned<PyToken>,               //"def"
                Spanned<Atom>,                  // <ident>
                Vec<Spanned<FunctionDefParam>>, // "(" (<ident> (":" <expr>)?),* ")"
                Option<Spanned<Expr>>,          // "->" <expr>
                Vec<Spanned<Statement>>,        // <body>
            )| {
                let span = fndef.span.clone();
                let fndef = FunctionDef {
                    name,
                    args,
                    body,
                    decorator_list,
                    returns,
                };

                Spanned::new(fndef, span)
            },
        )
        .debug("funcdef")
}

pub fn statement(indent: usize) -> impl Parser<Token, Spanned<Statement>, Error = Simple<Token>> {
    let comment = select! {
        (PyToken::Comment, _) => PyToken::Comment,
    };

    let wrapped_primary = primary()
        .map(|p| p.replace_with(Expr::Primary))
        .or(expr())
        .map(|expr| expr.replace_with(Statement::Expr))
        .then_ignore(comment.or_not())
        .boxed()
        .debug("wrapped_primary");

    let fndef = funcdef(indent)
        .map(|fndef| fndef.replace_with(Statement::FnDef))
        .boxed();

    fndef.or(wrapped_primary)
}

pub fn module() -> impl Parser<Token, Spanned<crate::ast::module::Module>, Error = Simple<Token>> {
    statement(0)
        .separated_by(tokens::newline().repeated())
        .map(|stmts| {
            let span = match stmts.as_slice() {
                [] => 0..0,
                [one] => one.span.clone(),
                [first, .., last] => first.span_to(last),
                _ => unreachable!(),
            };

            let t = montyc_ast::module::Module { body: stmts };

            Spanned::new(t, span)
        })
}

#[cfg(test)]
mod test {
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
        let out = (super::primary()).parse(stream).unwrap();

        let (left, right) = expect_attr(out);
        expect_name(*left);
        expect_name(*right);

        let stream = lex("a.b.c");
        let out = (super::primary()).parse(stream).unwrap();

        let (left, right) = expect_attr(out);
        expect_name(*left);
        let (left, right) = expect_attr(*right);
        expect_name(*left);
        expect_name(*right);
    }

    #[test]
    pub fn primary_parsing_call_expr() {
        let stream = lex("a()");
        let out = (super::primary()).parse(stream).unwrap();

        let (func, args) = Some(out).map(expect_call).unwrap();
        expect_name(*func);
        assert!(args.is_none(), "{args:?}");

        let stream = lex("a.b()");
        let out = (super::primary()).parse(stream).unwrap();

        let (left, right) = Some(out).map(expect_attr).unwrap();
        expect_name(*left);

        let (func, args) = expect_call(*right);
        expect_name(*func);
        assert!(args.is_none(), "{args:?}");
    }

    #[test]
    pub fn primary_parsing_subscript() {
        let stream = lex("a[0]");
        let out = (super::primary()).parse(stream).unwrap();

        let (obj, index) = expect_subscript(out);
        expect_name(*obj);
        expect_int(*index);
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
}
