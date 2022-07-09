use std::fmt::Debug;

use logos::Lexer;

use crate::{span_interner::BoundMutInterner, token::PyToken};

pub struct TokenStreamIter<'source, 'data> {
    pub(crate) bound: BoundMutInterner<'source, 'data>,
    pub(crate) lexer: Lexer<'source, PyToken>,
}

impl Debug for TokenStreamIter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenStreamIter")
            .field("bound", &self.bound)
            .finish()
    }
}

impl<'source, 'data> Iterator for TokenStreamIter<'source, 'data> {
    type Item = Result<super::Token, &'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next()?;

        let span = (self.lexer.span(), self.lexer.slice());
        let span_range = span.0.clone();

        let (token, span_range) = match token {
            PyToken::RawIdent => {
                let ident = self.bound.insert(span_range.clone());

                (PyToken::Ident(ident), span_range)
            }

            PyToken::ByteLiteral => {
                todo!("byte literals.")
            }

            PyToken::StringLiteral => {
                let n = self.bound.insert(span_range.clone());
                (PyToken::StringRef(n), span_range)
            }

            PyToken::Comment => {
                let n = self.bound.insert(span_range.clone());
                (PyToken::CommentRef(n), span_range)
            }

            PyToken::Invalid => unreachable!(),

            _ => (token, span_range),
        };

        assert_ne!(token, PyToken::Invalid, "{:?}", span);

        Some(Ok((token, span_range)))
    }
}
