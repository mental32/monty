//! Tokenizer, which is helpfully generated for us by Logos.

use std::iter::FromIterator;

use logos::Logos;

pub type Span = logos::Span;

/// `SpanRef`s are lightweight references to `SpanData`s
///
/// They are made up of two `u32`s the first is a group identifier
/// and the second is a distinct identifier.
///
/// Every distinct identifier uniquely refers to a span in a file somewhere,
/// the group identifiers allow for cheap string and identifier equality checks
/// e.g. all variable names `a` will have the same group identifier but different
/// distinct identifiers.
///
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct SpanRef(u32, u32);

impl From<(u32, u32)> for SpanRef {
    fn from((a, b): (u32, u32)) -> Self {
        Self(a, b)
    }
}

impl SpanRef {
    /// Get the group identifier of the span ref.
    ///
    /// Multiple spans with the same hash are considered to be in the same group.
    ///
    #[inline]
    pub fn group(&self) -> u32 {
        self.0
    }

    /// Get the distinct identifier of the span ref.
    ///
    /// This is a unique identifier for the span in the file.
    ///
    #[inline]
    pub fn distinct(&self) -> u32 {
        self.1
    }
}

/// parse the input string with the lexer and collect it into the generic output type `O`
///
/// # Examples
///
/// ```
/// use montyc_lexer::{tokens, PyToken};
///
/// let tok: Vec<_> = tokens("foo(1, 2, await)");
/// assert_eq!(tok.len(), 10);
/// assert!(matches!(
///     tok.as_slice(),
///     [
///      (PyToken::RawIdent, _),
///      (PyToken::LParen, _),
///      (PyToken::Digits(1), _),
///      (PyToken::Comma, _),
///      (PyToken::Whitespace, _),
///      (PyToken::Digits(2), _),
///      (PyToken::Comma, _),
///      (PyToken::Whitespace, _),
///      (PyToken::Await, _),
///      (PyToken::RParen, _),
///     ]
/// ));
/// ```
pub fn tokens<I, O>(input: I) -> O
where
    I: AsRef<str>,
    O: FromIterator<(PyToken, Span)>,
{
    lex(input.as_ref()).spanned().collect::<O>()
}

pub fn lex<'a>(input: &'a str) -> logos::Lexer<'a, PyToken> {
    PyToken::lexer(input)
}

pub trait Lexer {
    fn span(&self) -> Span;

    fn slice(&self) -> &str;
}

impl<'source> Lexer for logos::Lexer<'source, PyToken> {
    fn span(&self) -> Span {
        self.span()
    }

    fn slice(&self) -> &str {
        self.slice()
    }
}

#[derive(Debug, Logos, PartialEq, Copy, Clone, Hash, Eq)]
pub enum PyToken {
    #[error]
    Invalid,

    // -- Token literals
    #[token("True")]
    True,

    #[token("False")]
    False,

    #[token("None")]
    None,

    #[token("is")]
    Is,

    #[token("or")]
    Or,

    #[token("not")]
    Not,

    #[token("await")]
    Await,

    #[token("async")]
    Async,

    #[token("if")]
    If,

    #[token("elif")]
    Elif,

    #[token("else")]
    Else,

    #[token("class")]
    ClassDef,

    #[token("def")]
    FnDef,

    #[token("return")]
    Return,

    #[token("while")]
    While,

    #[token("pass")]
    Pass,

    #[token("continue")]
    Continue,

    #[token("in")]
    In,

    #[token("break")]
    Break,

    #[token("from")]
    From,

    #[token("import")]
    Import,

    #[token("raise")]
    Raise,

    #[token("assert")]
    Assert,

    #[token("del")]
    Del,

    #[token("global")]
    Global,

    #[token("yield")]
    Yield,

    #[token("nonlocal")]
    Nonlocal,

    #[token("...")]
    Ellipsis,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("@")]
    At,

    #[token(r"\")]
    Escape,

    #[token(r"/")]
    Div,

    #[token("^")]
    Caret,

    #[token(",")]
    Comma,

    #[token("=")]
    Equal,

    #[token(".")]
    Dot,

    #[token("&")]
    And,

    #[token(":")]
    Colon,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("~")]
    Tilde,

    #[token("%")]
    Modulo,

    #[token("<")]
    LessThan,

    #[token("|")]
    Pipe,

    #[token(">")]
    GreaterThan,

    #[token("!")]
    Bang,

    #[token("\n")]
    Newline,

    #[token("\u{c}")]
    FormFeed,

    #[token(";")]
    Disappointment,

    // -- Regex rules
    #[regex(r"\t| ")]
    Whitespace,

    #[regex(r"-?\d+", |lex| lex.slice().parse())]
    #[regex(r"-?0[xX][0-9a-fA-F]+", |lex| lex.slice().parse())]
    Digits(i64),

    #[regex(r"#[^\n]*")]
    Comment,

    // -- String regex
    #[regex(r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?'((\\.)|[^'\\\r\n])*'"#)]
    #[regex(
        r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?'''((\\.)|[^\\']|'((\\.)|[^\\'])|''((\\.)|[^\\']))*'''"#
    )]
    #[regex(r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?"((\\.)|[^"\\\r\n])*""#)]
    #[regex(
        r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?"""((\\.)|[^\\"]|"((\\.)|[^\\"])|""((\\.)|[^\\"]))*""""#
    )]
    StringLiteral,

    #[regex(r#"([bB]|[rR][bB]|[bB][rR])'((\\\p{ASCII})|[\p{ASCII}&&[^'\\\r\n]])*'"#)]
    #[regex(r#"([bB]|[rR][bB]|[bB][rR])'''((\\\p{ASCII})|[\p{ASCII}&&[^\\']]|'((\\\p{ASCII})|[\p{ASCII}&&[^\\']])|''((\\\p{ASCII})|[\p{ASCII}&&[^\\']]))*'''"#)]
    ByteLiteral,

    #[regex("[a-zA-Z_][_a-zA-Z0-9]*")]
    RawIdent,

    // -- used for interning
    IdentRef(SpanRef),
    CommentRef(SpanRef),
    StringRef(SpanRef),
}
