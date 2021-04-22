use std::{cell::RefCell, num::NonZeroUsize, ops::Range};
use std::{iter, rc::Rc};

use lazy_static::lazy_static;
use logos::Logos;
use nom::IResult;
use regex::Regex;

use crate::{ast::{AstObject, Spanned}, class::Class};

pub mod comb;
pub mod token;

mod span_ref;

pub use span_ref::{SpanEntry, SpanRef};
use token::PyToken;

// -- Parser

pub type Span = Range<usize>;

pub(crate) type Token = (PyToken, Span);

pub(crate) type TokenSlice<'a> = &'a [Token];

#[derive(Debug)]
pub struct Parser {
    source: Rc<str>,
    span_ref: Rc<RefCell<SpanRef>>,
}

impl From<(Rc<str>, Rc<RefCell<SpanRef>>)> for Parser
{
    fn from((source, span_ref): (Rc<str>, Rc<RefCell<SpanRef>>)) -> Self {
        Self {
            source,
            span_ref,
        }
    }
}

impl Parser {
    #[inline]
    pub fn resolve_ref(&self, reference: Option<NonZeroUsize>) -> Option<&str> {
        let span_ref = self.span_ref.borrow();
        let range = span_ref
            .seq
            .get(usize::from(reference?).saturating_sub(1))?
            .clone();

        Some(&self.source[range])
    }

    // #[inline]
    // pub fn parse_module(&self) -> AstObject {
    //     let stream = self.token_sequence();
    //     let (stream, object) = comb::statements(&stream).unwrap();

    //     assert!(stream.is_empty(), "{:?}", stream);

    //     object
    // }

    #[inline]
    fn token_sequence(&self) -> Box<[Token]> {
        self.token_stream()
            .map(Result::unwrap)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    #[inline]
    pub fn token_stream<'a>(&'a self) -> impl Iterator<Item = Result<Token, &'static str>> + 'a {
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

        let span_ref = Rc::clone(&self.span_ref);
        let mut lexer = <PyToken as Logos>::lexer(&self.source);

        let f = move || {
            let mut token = lexer.next()?;
            let span = (lexer.span(), lexer.slice());

            let mut span_range = span.0.clone();

            if let PyToken::Ident(inner) = &mut token {
                let mut ident = span_ref
                    .borrow_mut()
                    .push_noclobber(span_range.clone(), &self.source);
                std::mem::swap(inner, &mut ident);
            } else if let PyToken::Invalid = token {
                // we're not letting logos handle string literal or comment
                // parsing so `Invalid` may be produced when encountering
                // this. we deal with parsing and interning the string spans
                // manually.

                match &span {
                    (range, slice) if slice.len() == 1 => {
                        let ch = slice.chars().nth(0).unwrap();
                        let rest = &lexer.source().get(range.start..).unwrap();

                        let (capture, is_comment) = match ch {
                            '\'' | '"' | 'r' => (
                                MULTI_DQ_STRING
                                    .find(rest)
                                    .or_else(|| MULTI_SQ_STRING.find(rest))
                                    .or_else(|| SINGLE_SQ_STRING.find(rest))
                                    .or_else(|| SINGLE_DQ_STRING.find(rest)), false
                            ),

                            '#' => (COMMENT.find(rest), true),

                            _ => return Some(Err("fatal[0]: unrecoverable lexing error.")),
                        };

                        let capture = match capture {
                            Some(c) => (c.range(), c.as_str()),
                            None => return Some(Err("fatal[1]: unrecoverable lexing error.")),
                        };

                        span_range = range.start..(range.start + capture.0.end);

                        let (n, offset) = {
                            let n = span_ref.borrow_mut().push(span_range.clone());
                            let bump = capture.1.len();
                            (n, bump)
                        };

                        token = if is_comment {
                            PyToken::CommentRef(n)
                        } else {
                            PyToken::StringRef(n)
                        };

                        lexer.bump(offset - 1);
                    }

                    _ => return Some(Err("fatal[2]: unrecoverable lexing error.")),
                }
            }

            assert_ne!(token, PyToken::Invalid, "{:?}", span);

            Some(Ok((token, span_range)))
        };

        iter::from_fn(f)
    }
}

pub fn parse<P, R>(source: Rc<str>, func: P, span_ref: Option<Rc<RefCell<SpanRef>>>) -> R
where
    P: for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, R>,
    R: AstObject,
{
    let parser = match span_ref {
        Some(span_ref) => Parser::from((source, span_ref)),
        None => Parser { source, span_ref: Default::default() },
    };

    let seq = parser.token_sequence();

    let (stream, result) = func(&seq).unwrap();
    assert!(stream.is_empty(), "{:?}\n{:#?}", stream, result);

    result
}

pub type ParserT<R> = for<'a> fn(TokenSlice<'a>) -> nom::IResult<TokenSlice<'a>, R>;

pub trait Parseable: AstObject {
    const PARSER: ParserT<Self>;
}

impl<R> From<(Rc<str>, Rc<RefCell<SpanRef>>)> for Spanned<R> where R: Parseable + Clone {
    fn from((st, sr): (Rc<str>, Rc<RefCell<SpanRef>>)) -> Self {
        let output = parse(st.clone(), R::PARSER, Some(sr));

        Spanned {
            span: 0..st.len(),
            inner: output,
        }
    }
}

