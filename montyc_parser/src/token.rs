//! Tokenizer, which is helpfully generated for us by Logos.

use logos::Logos;
use montyc_core::SpanRef;

#[derive(Debug, Logos, PartialEq, Copy, Clone)]
#[cfg_attr(rustfmt, rustfmt_skip)]
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

    // -- String regex's (thank god I managed to nerdsnipe Quirl to do this for me.)
    #[regex(r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?'((\\.)|[^'\\\r\n])*'"#)]
    #[regex(r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?'''((\\.)|[^\\']|'((\\.)|[^\\'])|''((\\.)|[^\\']))*'''"#)]
    #[regex(r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?"((\\.)|[^"\\\r\n])*""#)]
    #[regex(r#"([rR]|[fF]|u|[rR][fF]|[fF][rR])?"""((\\.)|[^\\"]|"((\\.)|[^\\"])|""((\\.)|[^\\"]))*""""#)]
    StringLiteral,

    #[regex(r#"([bB]|[rR][bB]|[bB][rR])'((\\\p{ASCII})|[\p{ASCII}&&[^'\\\r\n]])*'"#)]
    #[regex(r#"([bB]|[rR][bB]|[bB][rR])'''((\\\p{ASCII})|[\p{ASCII}&&[^\\']]|'((\\\p{ASCII})|[\p{ASCII}&&[^\\']])|''((\\\p{ASCII})|[\p{ASCII}&&[^\\']]))*'''"#)]
    ByteLiteral,

    // -- SpanRef tokens
    #[regex("[a-zA-Z_][_a-zA-Z0-9]*")]
    RawIdent,

    // These tokens don't get a parser but they're used
    // and generated lazilly.
    Ident(SpanRef),

    StringRef(SpanRef),

    CommentRef(SpanRef),
}

impl From<PyToken> for Option<SpanRef> {
    fn from(token: PyToken) -> Self {
        match token {
            PyToken::StringRef(n) | PyToken::CommentRef(n) | PyToken::Ident(n) => Some(n),
            _ => None,
        }
    }
}
