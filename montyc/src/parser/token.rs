//! Tokenizer & lexer interface.

use logos::Logos;

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
    /// SpanRef tokens are inserted in a pre-processing phase of parsing source.
    ///
    /// Why? because it was such a pain in the ass to properly include all the
    /// different ways of lexing a string in Python in this logos Lexer, no I',
    /// not saying logos is responsible for this but I definitely am too much
    /// of a dumb dumb to figure out how to do it properly.
    ///
    /// so for now we have spanref's which is a token generated dynamically
    /// when a string (any kind of string literal) is parsed `${n}` and we just
    /// keep a big table where you can use `n` to get the original string
    /// happy? good.
    ///
    SpanRef(SpanEntry),

    #[regex(r"[\t ]+")]
    Whitespace,

    #[regex("[a-zA-Z_][_a-zA-Z0-9]*", |_| SpanEntry::None)]
    Ident(SpanEntry),

    #[regex(r"\d+", |lex| str::parse::<isize>(lex.slice()).unwrap())]     // TODO(mental): try avoid panicking here...
    Digits(isize),
}
