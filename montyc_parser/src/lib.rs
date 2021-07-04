#![forbid(unsafe_code)]

use std::{fmt::Debug, rc::Rc};

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

pub(crate) type TokenSlice<'a> = &'a [Token];

#[derive(Debug)]
struct Parser<'s> {
    source: &'s str,
    span_ref: SpanInterner,
}

impl Parser<'_> {
    #[inline]
    fn token_sequence(&self, module: ModuleRef) -> Box<[Token]> {
        let bound = self.span_ref.contextualize(&self.source, module).unwrap();

        let token_stream = TokenStreamIter {
            bound,
            lexer: <PyToken as logos::Logos>::lexer(&self.source),
        };

        token_stream
            .map(Result::unwrap)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

pub fn parse<P, R>(
    source: impl AsRef<str>,
    func: P,
    span_ref: Option<SpanInterner>,
    mref: ModuleRef,
) -> R
where
    P: for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, R>,
    R: Debug,
{
    let source = source.as_ref();
    let parser = match span_ref {
        Some(span_ref) => Parser { source, span_ref },

        None => Parser {
            source,
            span_ref: SpanInterner::new(),
        },
    };

    let seq = parser.token_sequence(mref);

    let (stream, result) = func(&seq).unwrap();

    assert!(stream.is_empty(), "{:?}\n{:#?}", stream, result);

    result
}

pub type ParserT<R> = for<'a> fn(TokenSlice<'a>) -> nom::IResult<TokenSlice<'a>, R>;

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
