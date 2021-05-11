use std::rc::Rc;
use std::{cell::RefCell, ops::Range};

use nom::IResult;
use token_iter::TokenStreamIter;

use crate::{
    ast::{AstObject, Spanned},
    context::ModuleRef,
};

pub mod comb;
pub mod token;
pub mod token_iter;

mod span_ref;

pub use span_ref::{SpanInterner, SpanRef};
use token::PyToken;

// -- Parser

pub type Span = Range<usize>;

pub(crate) type Token = (PyToken, Span);

pub(crate) type TokenSlice<'a> = &'a [Token];

#[derive(Debug)]
pub struct Parser {
    source: Rc<str>,
    span_ref: Rc<RefCell<SpanInterner>>,
}

impl From<(Rc<str>, Rc<RefCell<SpanInterner>>)> for Parser {
    fn from((source, span_ref): (Rc<str>, Rc<RefCell<SpanInterner>>)) -> Self {
        Self { source, span_ref }
    }
}

impl Parser {
    #[inline]
    fn token_sequence(&self, module_ref: ModuleRef) -> Box<[Token]> {
        let token_stream = TokenStreamIter {
            module_ref,
            span_ref: Rc::clone(&self.span_ref),
            lexer: <PyToken as logos::Logos>::lexer(&self.source),
            source: Rc::clone(&self.source),
        };

        token_stream
            .map(Result::unwrap)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

pub fn parse<P, R>(
    source: Rc<str>,
    func: P,
    span_ref: Option<Rc<RefCell<SpanInterner>>>,
    mref: ModuleRef,
) -> R
where
    P: for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, R>,
    R: AstObject,
{
    let parser = match span_ref {
        Some(span_ref) => Parser::from((source, span_ref)),
        None => Parser {
            source,
            span_ref: Default::default(),
        },
    };

    let seq = parser.token_sequence(mref);

    let (stream, result) = func(&seq).unwrap();
    assert!(stream.is_empty(), "{:?}\n{:#?}", stream, result);

    result
}

pub type ParserT<R> = for<'a> fn(TokenSlice<'a>) -> nom::IResult<TokenSlice<'a>, R>;

pub trait Parseable: AstObject {
    const PARSER: ParserT<Self>;
}

impl<R> From<(Rc<str>, Rc<RefCell<SpanInterner>>, ModuleRef)> for Spanned<R>
where
    R: Parseable + Clone,
{
    fn from((st, sr, mref): (Rc<str>, Rc<RefCell<SpanInterner>>, ModuleRef)) -> Self {
        let output = parse(st.clone(), R::PARSER, Some(sr), mref);

        Spanned {
            span: 0..st.len(),
            inner: output,
        }
    }
}
