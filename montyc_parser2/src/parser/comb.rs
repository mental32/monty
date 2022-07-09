use chumsky::prelude::*;

use crate::{
    prelude::{Atom, Expr, Primary, Spanned},
    token::PyToken,
};

fn atom() -> impl Parser<
    (PyToken, montyc_core::Span),
    Spanned<crate::ast::Atom>,
    Error = Simple<(PyToken, montyc_core::Span)>,
> {
    recursive(|atom| {
        let val = select! {
            (PyToken::Ident(sr), span) => Spanned::new(Atom::Name(sr), span),
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
    use chumsky::Parser;
    use montyc_core::ModuleRef;

    use crate::{
        prelude::{Atom, Spanned},
        token::PyToken,
    };

    fn lex(source: &str) -> Vec<(PyToken, montyc_core::Span)> {
        let spanner = crate::span_interner::SpanInterner::new();
        let bound = spanner.get(source, ModuleRef(0)).unwrap();
        let tokens = crate::token_iter::TokenStreamIter {
            bound,
            lexer: <PyToken as logos::Logos>::lexer(source),
        }
        .map(|each| each.unwrap())
        .collect::<Vec<_>>();
        tokens
    }

    #[test]
    pub fn atom_parser_works() {
        let (out, errors) = (super::atom()).parse_recovery(lex("1"));
        assert!(errors.is_empty());
        assert!(out.is_some());
        match out {
            Some(Spanned {
                inner: Atom::Int(_),
                ..
            }) => (),
            _ => panic!("{:?}", out),
        }

        let (out, errors) = (super::atom()).parse_recovery(lex("(1,2,3)"));
        assert!(errors.is_empty());
        assert!(out.is_some());
        match out {
            Some(Spanned {
                inner: Atom::Int(_),
                ..
            }) => (),
            _ => panic!("{:?}", out),
        }
    }
}
