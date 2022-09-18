use chumsky::prelude::*;

use montyc_ast::statement::Statement;
use montyc_lexer::PyToken;

use crate::ast::atom::Atom;
use crate::ast::expr::Expr;
use crate::ast::primary::Primary;
use crate::spanned::Spanned;
use crate::Token;

pub fn primary() -> impl Parser<Token, Spanned<crate::ast::primary::Primary>, Error = Simple<Token>>
{
    recursive(|pr| {
        let wrapped_atom = atom().map(|at| at.replace_with(Primary::Atomic));

        fn dotted(
            recurse: impl Parser<Token, Spanned<crate::ast::primary::Primary>, Error = Simple<Token>>,
            wrapped_atom: impl Parser<Token, Spanned<crate::ast::primary::Primary>, Error = Simple<Token>>
                + Clone,
        ) -> impl Parser<Token, Spanned<crate::ast::primary::Primary>, Error = Simple<Token>>
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

                    let span = 0..0;
                    let t = Primary::Attribute { left, attr };

                    Spanned::new(t, span)
                })
        }

        fn call_expr(
            wrapped_atom: impl Parser<
                Token,
                Spanned<crate::ast::primary::Primary>,
                Error = Simple<Token>,
            >,
        ) -> impl Parser<Token, Spanned<crate::ast::primary::Primary>, Error = Simple<Token>>
        {
            let lparen = select! {
                (PyToken::LParen, _) => PyToken::LParen,
            };

            let rparen = select! {
                (PyToken::RParen, _) => PyToken::RParen,
            };

            let comma = select! {
                (PyToken::Comma, _) => PyToken::Comma,
            };

            let whitespace = select! {
                (PyToken::Whitespace, _) => PyToken::Whitespace,
            }
            .repeated();

            let items = atom()
                .map(|i| i.replace_with(Primary::Atomic).replace_with(Expr::Primary))
                .or(expr())
                .padded_by(whitespace)
                .separated_by(comma)
                .at_least(1);

            wrapped_atom
                .then_ignore(lparen)
                .then(items.or_not())
                .then_ignore(rparen)
                .map(|(base, args)| {
                    let func = Box::new(base);

                    let span = 0..0;
                    let t = Primary::Call { func, args };

                    Spanned::new(t, span)
                })
        }

        fn subscript(
            wrapped_atom: impl Parser<
                Token,
                Spanned<crate::ast::primary::Primary>,
                Error = Simple<Token>,
            >,
        ) -> impl Parser<Token, Spanned<crate::ast::primary::Primary>, Error = Simple<Token>>
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

                    let span = 0..0;
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
}

pub fn expr() -> impl Parser<Token, Spanned<crate::ast::expr::Expr>, Error = Simple<Token>> {
    let wrapped_atom = atom().map(|at| at.replace_with(Primary::Atomic));

    wrapped_atom.map(|p| p.replace_with(Expr::Primary))
}

pub fn module() -> impl Parser<Token, Spanned<crate::ast::module::Module>, Error = Simple<Token>> {
    let newline = select! {
        (PyToken::Newline, _) => PyToken::Newline,
    };

    statement().separated_by(newline.repeated()).map(|stmts| {
        let span = 0..0;
        let t = montyc_ast::module::Module { body: stmts };

        Spanned::new(t, span)
    })
}

pub fn statement(
) -> impl Parser<Token, Spanned<crate::ast::statement::Statement>, Error = Simple<Token>> {
    let comment = select! {
        (PyToken::Comment, _) => PyToken::Comment,
    };

    primary()
        .map(|p| p.replace_with(Expr::Primary))
        .or(expr())
        .map(|expr| expr.replace_with(Statement::Expr))
        .then_ignore(comment.or_not())
}

pub fn atom() -> impl Parser<Token, Spanned<crate::ast::atom::Atom>, Error = Simple<Token>> + Clone
{
    recursive(|atom| {
        let val = select! {
            (PyToken::IdentRef(sr), span) => Spanned::new(Atom::Name(sr), span),
            (PyToken::True, span) => Spanned::new(Atom::Bool(true), span),
            (PyToken::False, span) => Spanned::new(Atom::Bool(false), span),
            (PyToken::None, span) => Spanned::new(Atom::None, span),
            (PyToken::Digits(n), span) => Spanned::new(Atom::Int(n), span),
            (PyToken::Ellipsis, span) => Spanned::new(Atom::Ellipsis, span),
        };

        let lparen = select! {
            (PyToken::LParen, _) => PyToken::LParen,
        };

        let rparen = select! {
            (PyToken::RParen, _) => PyToken::RParen,
        };

        let comma = select! {
            (PyToken::Comma, _) => PyToken::Comma,
        };

        let items = atom.separated_by(comma).allow_trailing();
        let tuple = items
            .delimited_by(lparen, rparen)
            .map(|i: Vec<Spanned<Atom>>| {
                let tuple = Atom::Tuple(
                    i.into_iter()
                        .map(|i| i.replace_with(Primary::Atomic).replace_with(Expr::Primary))
                        .collect::<Vec<Spanned<Expr>>>(),
                );
                Spanned::new(tuple, Default::default())
            });

        val.or(tuple)
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
        let (out, errors) = (super::atom()).parse_recovery(stream);
        assert!(errors.is_empty());
        assert!(out.is_some());

        expect_int(out.unwrap());

        let stream = lex("(1,2,3)");
        let (out, errors) = (super::atom()).parse_recovery(stream);
        assert!(errors.is_empty(), "{errors:#?}");
        assert!(out.is_some(), "{out:#?}");

        expect_tuple(out.unwrap());
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
    pub fn module_parsing() {
        let stream = lex("a\nb\nc\nd[e]");
        let out = (super::module()).parse(stream).unwrap().inner;

        assert_eq!(dbg!(&out.body).len(), 4);

        expect_name(out.body.get(0).cloned().unwrap());
        expect_name(out.body.get(1).cloned().unwrap());
        expect_name(out.body.get(2).cloned().unwrap());

        let (obj, index) = expect_subscript(out.body.get(3).cloned().unwrap());
        expect_name(*obj);
        expect_name(*index);
    }
}
