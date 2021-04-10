use std::{cell::RefCell, num::NonZeroUsize, ops::Range};
use std::{iter, rc::Rc};

use lazy_static::lazy_static;
use logos::Logos;
use nom::{IResult};
use regex::Regex;

use crate::ast::{AstObject, Spanned};

pub mod comb;
pub mod token;

mod span_ref {
    use std::{num::NonZeroUsize, ops::Range};

    pub type SpanEntry = Option<NonZeroUsize>;

    // -- SpanRef

    #[derive(Debug)]
    pub struct SpanRef {
        ptr: usize,
        pub(crate) seq: Vec<Range<usize>>,
    }

    impl Default for SpanRef {
        fn default() -> Self {
            Self {
                ptr: 1,
                seq: vec![0..0],
            }
        }
    }

    impl SpanRef {
        #[inline]
        pub fn push_noclobber(&mut self, value: Range<usize>, source: &str) -> SpanEntry {
            let expected = source.get(value.clone()).unwrap();

            for (idx, range) in self.seq.iter().enumerate() {
                if source
                    .get(range.clone())
                    .map(|sl| sl == expected)
                    .unwrap_or(false)
                {
                    return NonZeroUsize::new(idx + 1);
                }
            }

            self.ptr += 1;
            let key = self.ptr;
            self.seq.push(value);

            NonZeroUsize::new(key)
        }

        #[inline]
        pub fn push(&mut self, value: Range<usize>) -> SpanEntry {
            self.ptr += 1;
            let key = self.ptr;
            self.seq.push(value);

            NonZeroUsize::new(key)
        }

        #[inline]
        pub fn resolve_ref<'a>(
            &self,
            reference: Option<NonZeroUsize>,
            source: &'a str,
        ) -> Option<&'a str> {
            let range = self
                .seq
                .get(usize::from(reference?).saturating_sub(1))?
                .clone();

            source.get(range)
        }
    }
}

pub use span_ref::{SpanEntry, SpanRef};
use token::PyToken;

// -- Parser

pub(crate) type Span = Range<usize>;

pub(crate) type Token = (PyToken, Span);

pub(crate) type TokenSlice<'a> = &'a [Token];

#[derive(Debug, Default)]
pub struct Parser {
    source: String,
    span_ref: Rc<RefCell<SpanRef>>,
}

impl<S> From<(S, Rc<RefCell<SpanRef>>)> for Parser
where
    S: ToString,
{
    fn from((source, span_ref): (S, Rc<RefCell<SpanRef>>)) -> Self {
        Self {
            source: source.to_string(),
            span_ref,
        }
    }
}

impl Parser {
    #[inline]
    pub fn new<I>(input: I) -> Self
    where
        I: ToString,
    {
        Self {
            source: input.to_string(),
            span_ref: Default::default(),
        }
    }

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

                        let capture = match ch {
                            '\'' | '"' | 'r' => MULTI_DQ_STRING
                                .find(rest)
                                .or_else(|| MULTI_SQ_STRING.find(rest))
                                .or_else(|| SINGLE_SQ_STRING.find(rest))
                                .or_else(|| SINGLE_DQ_STRING.find(rest)),

                            '#' => COMMENT.find(rest),

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

                        token = PyToken::SpanRef(n);
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

pub fn parse<P, R>(source: &str, func: P, span_ref: Option<Rc<RefCell<SpanRef>>>) -> R
where
    P: for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, R>,
    R: AstObject,
{
    let parser = match span_ref {
        Some(span_ref) => Parser::from((source, span_ref)),
        None => Parser::new(source),
    };

    let seq = parser.token_sequence();

    let (stream, result) = func(&seq).unwrap();
    assert!(stream.is_empty(), "{:?}", stream);

    result
}

pub type ParserT<R> = for<'a> fn(TokenSlice<'a>) -> nom::IResult<TokenSlice<'a>, R>;

pub trait Parseable: AstObject
{
    const PARSER: ParserT<Self>;
}

impl<R> From<&str> for Spanned<R>
where
    R: Parseable + Clone,
{
    fn from(st: &str) -> Self {
        let r = parse(st, R::PARSER, Default::default());

        Spanned {
            span: 0..st.len(),
            inner: r,
        }
    }
}

impl<R, S> From<(S, Rc<RefCell<SpanRef>>)> for Spanned<R>
where
    R: Parseable + Clone,
    S: AsRef<str>,
{
    fn from((s, sr): (S, Rc<RefCell<SpanRef>>)) -> Self {
        let st = s.as_ref();
        let r = parse(st, R::PARSER, Some(sr));

        Spanned {
            span: 0..st.len(),
            inner: r,
        }
    }
}
