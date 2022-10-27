use chumsky::prelude::*;

use montyc_ast::expr::Expr;
use montyc_ast::funcdef::{FunctionDef, FunctionDefParam};
use montyc_ast::ifstmt::{If, IfChain};
use montyc_ast::spanned::Spanned;
use montyc_ast::statement::Statement;
use montyc_ast::{ann, assign, classdef, return_, while_};
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
        (true_,       "<true_>",      PyToken::True);
        (false_,      "<false_>",     PyToken::False);
        (none,        "<none>",       PyToken::None);
        (is,          "<is>",         PyToken::Is);
        (or,          "<or>",         PyToken::Or);
        (not,         "<not>",        PyToken::Not);
        (await_,      "<await>",      PyToken::Await);
        (async_,      "<async>",      PyToken::Async);
        (if_,         "<if>",         PyToken::If);
        (elif,        "<elif>",       PyToken::Elif);
        (else_,       "<else>",       PyToken::Else);
        (classdef,    "<classdef>",   PyToken::ClassDef);
        (fndef,       "<fndef>",      PyToken::FnDef);
        (return_,     "<return>",     PyToken::Return);
        (while_,      "<while>",      PyToken::While);
        (pass,        "<pass>",       PyToken::Pass);
        (continue_,   "<continue>",   PyToken::Continue);
        (in_,         "<in>",         PyToken::In);
        (break_,      "<break>",      PyToken::Break);
        (from,        "<from>",       PyToken::From);
        (import,      "<import>",     PyToken::Import);
        (raise,       "<raise>",      PyToken::Raise);
        (assert,      "<assert>",     PyToken::Assert);
        (del,         "<del>",        PyToken::Del);
        (global,      "<global>",     PyToken::Global);
        (yield_,      "<yield>",      PyToken::Yield);
        (nonlocal,    "<nonlocal>",   PyToken::Nonlocal);
        (ellipsis,    "<ellipsis>",   PyToken::Ellipsis);
        (lparen,      "<lparen>",     PyToken::LParen);
        (rparen,      "<rparen>",     PyToken::RParen);
        (lbracket,    "<lbracket>",   PyToken::LBracket);
        (rbracket,    "<rbracket>",   PyToken::RBracket);
        (lbrace,      "<lbrace>",     PyToken::LBrace);
        (rbrace,      "<rbrace>",     PyToken::RBrace);
        (at,          "<at>",         PyToken::At);
        (escape,      "<escape>",     PyToken::Escape);
        (div,         "<div>",        PyToken::Div);
        (caret,       "<caret>",      PyToken::Caret);
        (comma,       "<comma>",      PyToken::Comma);
        (equal,       "<equal>",      PyToken::Equal);
        (dot,         "<dot>",        PyToken::Dot);
        (and,         "<and>",        PyToken::And);
        (colon,       "<colon>",      PyToken::Colon);
        (plus,        "<plus>",       PyToken::Plus);
        (minus,       "<minus>",      PyToken::Minus);
        (star,        "<star>",       PyToken::Star);
        (tilde,       "<tilde>",      PyToken::Tilde);
        (modulo,      "<modulo>",     PyToken::Modulo);
        (lessthan,    "<lessthan>",   PyToken::LessThan);
        (pipe,        "<pipe>",       PyToken::Pipe);
        (greaterthan, "<greaterthan>",PyToken::GreaterThan);
        (bang,        "<bang>",       PyToken::Bang);
        (newline,     "<newline>",    PyToken::Newline);
        (formfeed,    "<formfeed>",   PyToken::FormFeed);
        (whitespace,  "<whitespace>", PyToken::Whitespace)
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
pub fn primary(
    expr: impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone + 'static,
) -> p!(Spanned<Primary>; Clone) {
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
            // <dotted> := <wAtom> DOT <primary>+
            // <primary> := <dotted> | <wAtom>
            wrapped_atom
                .clone()
                .then(
                    tokens::dot()
                        .ignore_then(recurse.or(wrapped_atom))
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
            expr: impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
            wrapped_atom: P,
        ) -> impl Parser<Token, Spanned<Primary>, Error = Simple<Token>>
        where
            P: Parser<Token, Spanned<Primary>, Error = Simple<Token>>,
        {
            let whitespace = tokens::whitespace().repeated();

            let items = atom()
                .map(|i| i.replace_with(Primary::Atomic).replace_with(Expr::Primary))
                .or(expr)
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
            expr: impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
            wrapped_atom: P,
        ) -> impl Parser<Token, Spanned<Primary>, Error = Simple<Token>>
        where
            P: Parser<Token, Spanned<Primary>, Error = Simple<Token>>,
        {
            wrapped_atom
                .then(expr.delimited_by(tokens::lbracket(), tokens::rbracket()))
                .map(|(value, index)| {
                    let value = Box::new(value);
                    let index = Box::new(index);

                    let span = value.span_to(&index);
                    let t = Primary::Subscript { value, index };

                    Spanned::new(t, span)
                })
        }

        let awaited = tokens::await_()
            .then_ignore(tokens::whitespace().repeated())
            .repeated()
            .at_least(1)
            .then(pr.clone())
            .map(|(mut waits, primary): (Vec<_>, Spanned<Primary>)| {
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
            .or(call_expr(expr.clone(), wrapped_atom.clone()))
            .or(subscript(expr.clone(), wrapped_atom.clone()))
    })
    .debug("primary()")
}

pub fn expr() -> p!(Spanned<crate::ast::expr::Expr>; Clone) {
    recursive(|ex| {
        primary(ex)
            .map(|p| p.replace_with(Expr::Primary))
            .or(atom().map(|a| a.replace_with(Primary::Atomic).replace_with(Expr::Primary)))
    })
}

/// annotated_identifier := <ident> [":" <expr>]
/// annotated_identifier_list = (<annotated_identifier> ",")*
pub fn annotated_identifier_list() -> p!(Vec<Spanned<FunctionDefParam>>) {
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
pub fn indent(prefix: usize) -> p!(()) {
    tokens::whitespace().repeated().exactly(prefix).map(|_| ())
}

#[track_caller]
pub fn indented_block<F, P, T>(prefix: usize, f: F) -> p!(Vec<T>)
where
    F: Fn() -> P + Clone + 'static,
    P: Parser<Token, T, Error = Simple<Token>> + Clone + 'static,
    T: 'static,
{
    indent(prefix)
        .then_with(move |()| f())
        .separated_by(tokens::newline())
        .at_least(1)
        .debug("indented_block")
}

pub fn funcdef(indent: usize) -> p!(Spanned<FunctionDef>) {
    let parameters = annotated_identifier_list().delimited_by(tokens::lparen(), tokens::rparen());

    let return_type_annotation = tokens::minus()
        .ignore_then(tokens::greaterthan())
        .ignore_then(tokens::whitespace().repeated().or_not())
        .ignore_then(expr());

    let decorator = tokens::at().ignore_then(expr()).debug("funcdef.decorator");

    let decorator_list = decorator
        .separated_by(tokens::newline())
        .debug("funcdef.decorator_list");

    let prefix = indent + 4;
    let body = tokens::newline()
        .ignore_then(indented_block(prefix, move || statement(prefix)))
        .or(chumsky::primitive::any()
            .rewind()
            .then_with(|_| statement(0))
            .map(|s| vec![s]));

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
                Vec<Spanned<_>>,                // ["@" <primary>]*
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

pub fn if_stmt(indent: usize) -> p!(Spanned<montyc_ast::ifstmt::IfChain>) {
    let indent = indent + 4;
    let body = |prefix: usize| {
        tokens::newline()
            .ignore_then(indented_block(prefix, move || statement(prefix)))
            .or(chumsky::primitive::any()
                .rewind()
                .then_with(|_| statement(0))
                .map(|s| vec![s]))
            .then_ignore(tokens::newline().or_not())
    };

    let rest = expr().then_ignore(tokens::colon().padded_by(tokens::whitespace().repeated()));

    let head = tokens::if_()
        .then_ignore(tokens::whitespace().repeated().at_least(1))
        .ignore_then(rest.clone().then(body(indent)));

    let mid = head.then(
        tokens::elif()
            .then_ignore(tokens::whitespace().repeated().at_least(1))
            .ignore_then(rest.clone().then(body(indent)))
            .repeated(),
    );

    let tail = mid.then(
        tokens::else_()
            .then_ignore(tokens::whitespace().repeated())
            .ignore_then(tokens::colon())
            .then_ignore(tokens::whitespace().repeated())
            .ignore_then(body(indent))
            .or_not(),
    );

    tail.map(
        |(((if_test, if_body), elif), else_): (
            (
                (
                    montyc_ast::spanned::Spanned<_>,
                    Vec<montyc_ast::spanned::Spanned<Statement>>,
                ),
                Vec<(
                    montyc_ast::spanned::Spanned<_>,
                    Vec<montyc_ast::spanned::Spanned<Statement>>,
                )>,
            ),
            Option<Vec<montyc_ast::spanned::Spanned<Statement>>>,
        )| { (if_test, if_body, elif, else_) },
    )
    .map(|(if_test, if_body, elif, orelse)| {
        let start = if_test.span.start;
        let end = orelse
            .as_ref()
            .and_then(|e| e.last())
            .map(|l| l.span.end)
            .or(elif
                .last()
                .as_ref()
                .and_then(|l| l.1.last())
                .map(|l| l.span.end))
            .unwrap_or(if_test.span.end);

        let span = start..end;

        let mut branches: Vec<_> = elif
            .into_iter()
            .map(|(test, body)| Spanned::new(If { test, body }, span.clone()))
            .collect();

        branches.insert(
            0,
            Spanned::new(
                If {
                    test: if_test,
                    body: if_body,
                },
                span.clone(),
            ),
        );

        let ifch = IfChain { branches, orelse };

        Spanned::new(ifch, span)
    })
}

pub fn while_(indent: usize) -> p!(Spanned<while_::While>) {
    let prefix = indent + 4;

    let while_loop = tokens::while_()
        .ignore_then(tokens::whitespace().repeated())
        .ignore_then(expr())
        .then_ignore(tokens::colon())
        .then(tokens::newline().ignore_then(indented_block(prefix, move || statement(prefix))));

    while_loop.map(
        |(test, body): (
            montyc_ast::spanned::Spanned<montyc_ast::expr::Expr>,
            Vec<montyc_ast::spanned::Spanned<montyc_ast::statement::Statement>>,
        )| {
            let span = test.span.start..body.last().map(|s| s.span.end).unwrap_or(test.span.end);
            let while_ = while_::While { test, body };

            Spanned::new(while_, span)
        },
    )
}

pub fn classdef(indent: usize) -> p!(Spanned<classdef::ClassDef>) {
    let prefix = indent + 4;

    tokens::classdef()
        .then_ignore(tokens::whitespace().repeated())
        .then(ident())
        .then_ignore(tokens::whitespace().repeated())
        .then(
            expr()
                .padded_by(tokens::whitespace().repeated())
                .separated_by(tokens::comma())
                .or_not(),
        )
        .then_ignore(tokens::whitespace().repeated())
        .then_ignore(tokens::colon())
        .then(tokens::newline().ignore_then(indented_block(prefix, move || statement(prefix))))
        .map(
            |(((_, name), _), body): (
                (
                    (
                        montyc_ast::spanned::Spanned<montyc_lexer::PyToken>,
                        montyc_ast::spanned::Spanned<Atom>,
                    ),
                    Option<Vec<montyc_ast::spanned::Spanned<montyc_ast::expr::Expr>>>,
                ),
                Vec<montyc_ast::spanned::Spanned<Statement>>,
            )| {
                let span = 0..0;
                let classdef = classdef::ClassDef { name, body };
                Spanned::new(classdef, span)
            },
        )
}

pub fn return_() -> p!(Spanned<return_::Return>; Clone) {
    tokens::return_()
        .then_ignore(tokens::whitespace().repeated().at_least(1))
        .ignore_then(expr().or_not())
        .map(|ex| {
            let span = ex.as_ref().map(|ex| ex.span.clone()).unwrap_or(0..0);
            let ret = return_::Return { value: ex };
            Spanned::new(ret, span)
        })
}

pub fn statement(indent: usize) -> p!(Spanned<Statement>; Clone) {
    let comment = select! {
        (PyToken::Comment, _) => PyToken::Comment,
    };

    let w_expr = expr()
        .map(|expr| expr.replace_with(Statement::Expr))
        .then_ignore(comment.or_not())
        .boxed()
        .debug("statement().expr");

    let fndef = funcdef(indent)
        .map(|fndef| fndef.replace_with(Statement::FnDef))
        .boxed()
        .debug("statement().fndef");

    let import = tokens::import()
        .ignore_then(tokens::whitespace().repeated())
        .ignore_then(
            ident()
                .map(|id| id.replace_with(Primary::Atomic))
                .separated_by(tokens::dot()),
        )
        .map(|dotted_names| {
            let span = dotted_names.first().map(|f| f.span.clone()).unwrap_or(0..0);
            let import = montyc_ast::import::Import::Names(dotted_names);
            Spanned::new(import, span).replace_with(Statement::Import)
        })
        .boxed()
        .debug("statement().import");

    let ifch = if_stmt(indent)
        .map(|ifch| ifch.replace_with(Statement::If))
        .boxed()
        .debug("statement().ifch");

    let whl = while_(indent)
        .map(|whl| whl.replace_with(Statement::While))
        .boxed()
        .debug("statement().while");

    let ret = return_()
        .map(|ret| ret.replace_with(Statement::Ret))
        .boxed()
        .debug("statement().return");

    let classdef = classdef(indent)
        .map(|klass| klass.replace_with(Statement::Class))
        .boxed()
        .debug("statement().classdef");

    let pass = tokens::pass()
        .map(|p| p.replace_with(Statement::Pass))
        .boxed()
        .debug("statement().pass");

    let annotation = ident()
        .then_ignore(tokens::whitespace().repeated())
        .then(
            tokens::colon()
                .then(tokens::whitespace().repeated())
                .ignore_then(expr())
                .then_ignore(tokens::whitespace().repeated()),
        )
        .map(|(name, ann)| {
            let span = 0..0;
            let assign = ann::Annotation {
                name,
                annotation: ann,
            };

            Spanned::new(assign, span).replace_with(Statement::Ann)
        })
        .boxed()
        .debug("statement().annotation");

    let assign = ident()
        .map(|id| id.replace_with(Primary::Atomic))
        .then_ignore(tokens::whitespace().repeated())
        .then(
            tokens::colon()
                .then(tokens::whitespace().repeated())
                .ignore_then(expr())
                .then_ignore(tokens::whitespace().repeated())
                .or_not(),
        )
        .then_ignore(tokens::equal())
        .then_ignore(tokens::whitespace().repeated())
        .then(expr())
        .map(|((name, ann), val)| {
            let span = 0..0;
            let assign = assign::Assign {
                name,
                annotation: ann,
                value: val,
            };

            Spanned::new(assign, span).replace_with(Statement::Asn)
        })
        .boxed()
        .debug("statement().assign");

    assign
        .or(annotation)
        .or(pass)
        .or(classdef)
        .or(ret)
        .or(whl)
        .or(ifch)
        .or(import)
        .or(fndef)
        .or(w_expr)
}

pub fn module() -> p!(Spanned<crate::ast::module::Module>) {
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
            _ => unimplemented!(),
        }

        let stream = lex("import x.y");
        let out = super::statement(0).parse(stream).unwrap().inner;

        match out {
            Statement::Import(imp) => match imp.inner {
                montyc_ast::import::Import::Names(names) => assert_eq!(names.len(), 2),
                montyc_ast::import::Import::From { .. } => panic!(),
            },
            _ => unimplemented!(),
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
            _ => unimplemented!(),
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
            _ => unimplemented!(),
        }

        let stream = lex("if x:\n    1\nelse:\n    2");
        let out = super::statement(0).parse(stream).unwrap().inner;

        match out {
            Statement::If(ifch) => {
                let ifch = ifch.inner;
                assert_eq!(ifch.branches.len(), 1);
                assert!(ifch.orelse.is_some());
            }
            _ => unimplemented!(),
        }

        let stream = lex("if x:\n    1\nelif True:\n    2");
        let out = super::statement(0).parse(stream).unwrap().inner;

        match out {
            Statement::If(ifch) => {
                let ifch = ifch.inner;
                assert_eq!(ifch.branches.len(), 2);
                assert!(ifch.orelse.is_none());
            }
            _ => unimplemented!(),
        }

        let stream = lex("if x:\n    1\nelif True:\n    4\nelse:\n    2");
        let out = super::statement(0).parse(stream).unwrap().inner;

        match out {
            Statement::If(ifch) => {
                let ifch = ifch.inner;
                assert_eq!(ifch.branches.len(), 2);
                assert!(ifch.orelse.is_some());
            }
            _ => unimplemented!(),
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
            _ => unreachable!(),
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
}
