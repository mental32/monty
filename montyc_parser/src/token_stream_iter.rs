use montyc_lexer::{Lexer, PyToken};

use crate::span_interner::BoundMutInterner;

type Token = PyToken;

pub struct TokenStreamIter<'source, 'data, L, M>
where
    L: Lexer + Iterator<Item = Token> + 'source,
{
    pub bound: BoundMutInterner<'source, 'data, M>,
    pub lexer: L,
    pub(crate) previous: Option<crate::Token>,
}

impl<'source, 'data, L, M> TokenStreamIter<'source, 'data, L, M>
where
    L: Lexer + Iterator<Item = Token> + 'source,
{
    pub fn new(bound: BoundMutInterner<'source, 'data, M>, lexer: L) -> Self {
        Self {
            bound,
            lexer,
            previous: None,
        }
    }
}

impl<'source, 'data, L, M> Iterator for TokenStreamIter<'source, 'data, L, M>
where
    L: Lexer + Iterator<Item = Token> + 'source,
    M: Clone,
{
    type Item = Result<crate::Token, &'static str>;

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next()?;

        let span = self.lexer.slice();
        let span_range = self.lexer.span();

        let (token, span_range) = match token {
            PyToken::RawIdent => {
                let ident = self.bound.insert(span_range.clone());

                (PyToken::IdentRef(ident), span_range)
            }

            PyToken::ByteLiteral => {
                let n = self.bound.insert(span_range.clone());
                (PyToken::ByteLiteralRef(n), span_range)
            }

            PyToken::StringLiteral => {
                let n = self.bound.insert(span_range.clone());
                (PyToken::StringRef(n), span_range)
            }

            PyToken::Comment => {
                let n = self.bound.insert(span_range.clone());
                (PyToken::CommentRef(n), span_range)
            }

            PyToken::Invalid => panic!("invalid token! {:?}", span),

            PyToken::Newline
                if self
                    .previous
                    .as_ref()
                    .map_or(false, |(t, _)| *t == PyToken::Newline) =>
            {
                return self.next();
            }

            _ => (token, span_range),
        };

        assert_ne!(token, PyToken::Invalid, "{:?}", span);
        self.previous.replace((token.clone(), span_range.clone()));

        Some(Ok((token, span_range)))
    }
}
