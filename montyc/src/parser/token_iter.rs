use std::{cell::RefCell, rc::Rc};

use lazy_static::lazy_static;
// use logos::Logos;
use regex::Regex;

use crate::{context::ModuleRef, prelude::PyToken};

use super::span_ref::SpanInterner;

lazy_static! {
    // Single quote string literals.
    static ref MULTI_SQ_STRING: Regex = Regex::new(r###"r?'''[^']*'''"###).unwrap();
    static ref SINGLE_SQ_STRING: Regex = Regex::new(r###"r?'[^"]*'"###).unwrap();
    // Double quote string literals.
    static ref MULTI_DQ_STRING: Regex = Regex::new(r###"r?"""[^"]*""""###).unwrap();
    static ref SINGLE_DQ_STRING: Regex = Regex::new(r###"r?"[^"]*""###).unwrap();
    // Comments.
    static ref COMMENT: Regex = Regex::new(r"^#[^\n]*").unwrap();
}

pub struct TokenStreamIter<'a> {
    pub(super) module_ref: ModuleRef,
    pub(super) source: Rc<str>,
    pub(super) span_ref: Rc<RefCell<SpanInterner>>,
    pub(super) lexer: logos::Lexer<'a, PyToken>,
}

impl<'a> Iterator for TokenStreamIter<'a> {
    type Item = Result<super::Token, &'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next()?;

        let span = (self.lexer.span(), self.lexer.slice());
        let span_range = span.0.clone();

        let (token, span_range) = match token {
            PyToken::RawIdent => {
                let ident = self.span_ref.borrow_mut().push_grouped(
                    span_range.clone(),
                    &self.source,
                    self.module_ref.clone(),
                );

                (PyToken::Ident(ident), span_range)
            }

            // we're not letting logos handle string literal or comment
            // parsing so `Invalid` may be produced when encountering
            // this. we deal with parsing and interning the string spans
            // manually.
            PyToken::Invalid => match &span {
                (range, slice) if slice.len() == 1 => {
                    let ch = slice.chars().nth(0).unwrap();
                    let rest = &self.lexer.source().get(range.start..).unwrap();

                    let (capture, is_comment) = match ch {
                        '\'' | '"' | 'r' => (
                            MULTI_DQ_STRING
                                .find(rest)
                                .or_else(|| MULTI_SQ_STRING.find(rest))
                                .or_else(|| SINGLE_SQ_STRING.find(rest))
                                .or_else(|| SINGLE_DQ_STRING.find(rest)),
                            false,
                        ),

                        '#' => (COMMENT.find(rest), true),

                        _ => return Some(Err("fatal[0]: unrecoverable lexing error.")),
                    };

                    let capture = match capture {
                        Some(c) => (c.range(), c.as_str()),
                        None => return Some(Err("fatal[1]: unrecoverable lexing error.")),
                    };

                    let span_range = range.start..(range.start + capture.0.end);

                    let (n, offset) = {
                        let n = self.span_ref.borrow_mut().push(span_range.clone());
                        let bump = capture.1.len();
                        (n, bump)
                    };

                    let token = if is_comment {
                        PyToken::CommentRef(n)
                    } else {
                        PyToken::StringRef(n)
                    };

                    self.lexer.bump(offset - 1);

                    (token, span_range)
                }

                _ => return Some(Err("fatal[2]: unrecoverable lexing error.")),
            },

            _ => (token, span_range),
        };

        assert_ne!(token, PyToken::Invalid, "{:?}", span);

        Some(Ok((token, span_range)))
    }
}
