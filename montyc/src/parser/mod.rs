use std::{cell::RefCell, ops::Range};
use std::{iter, rc::Rc};



use lazy_static::lazy_static;
use logos::Logos;
use regex::Regex;

mod ast;
mod comb;
mod token;

mod span_ref {
    use std::{num::NonZeroUsize, ops::Range};

    pub(crate) type SpanEntry = Option<NonZeroUsize>;

    // -- SpanRef

    #[derive(Debug)]
    pub(crate) struct SpanRef {
        ptr: usize,
        seq: Vec<Range<usize>>,
    }

    impl Default for SpanRef {
        fn default() -> Self {
            Self {
                ptr: 1,
                seq: vec![],
            }
        }
    }

    impl SpanRef {
        #[inline]
        pub fn push(&mut self, value: Range<usize>) -> SpanEntry {
            self.ptr += 1;
            let key = self.ptr;
            self.seq.push(value);

            NonZeroUsize::new(key)
        }
    }
}

use span_ref::{SpanEntry, SpanRef};
use token::PyToken;

// -- PyParse

pub(crate) type Span = Range<usize>;

pub(crate) type Token = (PyToken, Span);

pub(crate) type TokenSlice<'a> = &'a [Token];

#[derive(Debug, Default)]
pub struct PyParse {
    source: String,
    span_ref: Rc<RefCell<SpanRef>>,
}

impl PyParse {
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

    // #[inline]
    // pub fn statement(&self) -> AstObject {
    //     let stream = self.token_sequence();
    //     let (stream, object) = comb::statement(&stream).unwrap();

    //     assert!(stream.is_empty(), "{:?}", stream);

    //     object
    // }

    // #[inline]
    // pub fn expression(&self) -> AstObject {
    //     let stream = self.token_sequence();
    //     let (stream, object) = comb::expression(&stream).unwrap();
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
            static ref MULTI_SQ_STRING: Regex = Regex::new(r###"^r?'''[\w\W]*'''"###).unwrap();
            static ref SINGLE_SQ_STRING: Regex = Regex::new(r###"^r?'[^"]*'"###).unwrap();
            // Double quote string literals.
            static ref MULTI_DQ_STRING: Regex = Regex::new(r###"^r?"""[\w\W]*""""###).unwrap();
            static ref SINGLE_DQ_STRING: Regex = Regex::new(r###"^r?"[^"]*""###).unwrap();
            // Comments.
            static ref COMMENT: Regex = Regex::new(r"^#[^\n]*").unwrap();
        }

        let span_ref = Rc::clone(&self.span_ref);
        let mut lexer = <PyToken as Logos>::lexer(&self.source);

        let f = move || {
            let mut token = lexer.next()?;
            let span = (lexer.span(), lexer.slice());

            if let PyToken::Ident(inner) = &mut token {
                match inner {
                    None => {
                        *inner = span_ref.borrow_mut().push(span.0.clone());
                    }

                    _ => unreachable!(),
                }
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

                        let (n, offset) = {
                            let n = span_ref.borrow_mut().push(range.clone());
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

            Some(Ok((token, span.0)))
        };

        iter::from_fn(f)
    }
}

#[test]
#[ignore]
fn test_lex_stdlib() {
    use glob::glob;
    use rayon::prelude::*;

    fn brrrrrr(source: String) -> Option<()> {
        let parser = PyParse::new(source);
        let mut stream = parser.token_stream();

        while let Some(oh) = stream.next() {
            oh.ok()?;
        }

        Some(())
    }

    let stdlib_path = std::env::var("STDLIB").expect("`STDLIB` was not set in envvars!");

    let paths: Vec<_> = glob(stdlib_path.as_str())
        .expect("Failed to glob.")
        .into_iter()
        .collect();

    let n = paths
        .par_iter()
        .filter_map(|path| path.as_ref().ok())
        .filter_map(|path| std::fs::read_to_string(path).ok())
        .filter_map(brrrrrr)
        .count();

    let cov = (n as f64 / paths.len() as f64) * 100f64;

    eprintln!(
        "stdlib-lex-fuzz: {:?} total files, {:?} correctly lexed. ({2:.2}%)",
        paths.len(),
        n,
        cov,
    );
}
