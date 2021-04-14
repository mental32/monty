//! Tokenizer & lexer interface.

use logos::{Logos, Span};

use super::SpanEntry;

#[derive(Debug, Logos, PartialEq, Copy, Clone)]
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
    Dissapointment,

    // -- Regex rules

    #[regex(r"\t| ")]
    Whitespace,

    #[regex("[a-zA-Z_][_a-zA-Z0-9]*", |_| SpanEntry::None)]
    Ident(SpanEntry),

    #[regex(r"\d+", |lex| str::parse::<isize>(lex.slice()).unwrap())]     // TODO(mental): try avoid panicking here...
    Digits(isize),

    // -- Dynamic rules

    /// SpanRef tokens are generated lazily when lexing over the source.
    ///
    /// They are generated upon encountering:
    ///
    ///   * comments
    ///   * string literals (including multiline literals)
    ///   * identifier literals
    ///
    SpanRef(SpanEntry),
}

impl From<PyToken> for SpanEntry {
    fn from(token: PyToken) -> Self {
        match token {
            PyToken::SpanRef(n) => n,
            _ => unreachable!(),
        }
    }
}
