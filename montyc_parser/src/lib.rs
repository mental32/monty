#![forbid(unsafe_code)]
#![deny(warnings)]

use std::{cell::RefCell, fmt::Debug, rc::Rc};

use nom::IResult;

use montyc_core::ModuleRef;

pub mod ast;
pub mod comb;
pub mod span_interner;
pub mod spanned;
pub mod token;
pub mod token_iter;

pub use ast::{AstNode, AstObject, AstVisitor};
pub use span_interner::SpanInterner;
use spanned::Spanned;
use token::PyToken;
use token_iter::TokenStreamIter;

pub(crate) type Token = (PyToken, logos::Span);

#[derive(Debug)]
pub(crate) struct TokenStream<'source, 'data> {
    pub(crate) stream_iter: TokenStreamIter<'source, 'data>,
    pub(crate) tokens: Vec<Token>,
    stream_iter_complete: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct TokenStreamRef<'this, 'source, 'data> {
    stream: &'this RefCell<TokenStream<'source, 'data>>,
    tokens_slice_start: usize,
}

impl<'this, 'source, 'data> TokenStreamRef<'this, 'source, 'data> {
    pub(crate) fn grow(&self) -> Option<Token> {
        let mut this = self.stream.borrow_mut();

        let token = match this.stream_iter.next() {
            Some(t) => t.unwrap(),
            None => {
                this.stream_iter_complete = true;
                return None;
            }
        };

        this.tokens.push(token.clone());

        Some(token)
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.stream.borrow().stream_iter_complete
    }
}

impl<'t, 's, 'd> PartialEq for TokenStreamRef<'t, 's, 'd> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert_eq!(self.stream as *const _, other.stream as *const _);
        self.tokens_slice_start == other.tokens_slice_start
    }
}

pub fn parse<P, R>(
    source: impl AsRef<str>,
    func: P,
    span_ref: Option<SpanInterner>,
    mref: ModuleRef,
) -> R
where
    P: for<'this, 'source, 'data> Fn(
        TokenStreamRef<'this, 'source, 'data>,
    ) -> IResult<TokenStreamRef<'this, 'source, 'data>, R>,
    R: Debug,
{
    let source = source.as_ref();
    let span_ref = span_ref.unwrap_or_else(|| SpanInterner::new());

    let stream = {
        let bound = span_ref.contextualize(&source, mref).unwrap();

        let token_stream = TokenStreamIter {
            bound,
            lexer: <PyToken as logos::Logos>::lexer(&source),
        };

        let stream = TokenStream {
            stream_iter: token_stream,
            tokens: Vec::with_capacity(1024),
            stream_iter_complete: false,
        };

        RefCell::new(stream)
    };

    let result = {
        let stream_ref = TokenStreamRef {
            stream: &stream,
            tokens_slice_start: 0,
        };

        let (_, result) = func(stream_ref).unwrap();

        result
    };

    let stream = stream.into_inner();

    assert!(
        stream.stream_iter_complete,
        "{:?}\n{:#?}",
        stream.tokens, result
    );

    result
}

pub type ParserT<R> =
    for<'this, 'source, 'data> fn(
        TokenStreamRef<'this, 'source, 'data>,
    ) -> nom::IResult<TokenStreamRef<'this, 'source, 'data>, R>;

pub trait Parseable {
    const PARSER: ParserT<Self>;
}

impl<R> From<(Rc<str>, SpanInterner, ModuleRef)> for Spanned<R>
where
    R: Parseable + Clone + Debug,
{
    fn from((st, sr, mref): (Rc<str>, SpanInterner, ModuleRef)) -> Self {
        let output = parse(st.clone(), R::PARSER, Some(sr), mref);

        Spanned {
            span: 0..st.len(),
            inner: output,
        }
    }
}
