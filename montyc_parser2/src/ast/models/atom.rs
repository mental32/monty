use super::*;

#[derive(Debug, Clone, derive_more::Unwrap)]
pub enum Atom {
    None,
    Ellipsis,
    Int(i64),
    Str(SpanRef),
    Bool(bool),
    Float(f64),
    Tuple(Vec<Spanned<Expr>>),
    Comment(SpanRef),
    Name(SpanRef),
}

impl AstObject for Atom {
    fn into_ast_node(&self) -> AstNode {
        todo!()
    }

    fn type_name(&self) -> &str {
        todo!()
    }

    fn call_visitor_handler<T>(
        &self,
        visitor: &dyn AstVisitor<T>,
        span: Option<montyc_core::Span>,
    ) -> T
    where
        Self: Sized,
    {
        todo!()
    }
}

impl Atom {
    pub fn as_name(&self) -> Option<SpanRef> {
        match self {
            Self::Name(t) => Some(t.clone()),
            _ => None,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(a), Self::Bool(k)) => a == k,
            (Self::Int(n), Self::Int(i)) => n == i,
            (Self::Str(s), Self::Str(t)) => s == t,
            (Self::Name(a), Self::Name(b)) => a == b,
            (Self::Ellipsis, Self::Ellipsis) | (Self::None, Self::None) => true,
            _ => unimplemented!(),
        }
    }
}

impl From<PyToken> for Atom {
    fn from(value: PyToken) -> Self {
        match value {
            PyToken::Ellipsis => Self::Ellipsis,
            PyToken::None => Self::None,
            PyToken::True => Self::Bool(true),
            PyToken::False => Self::Bool(false),
            PyToken::Digits(n) => Self::Int(n),
            PyToken::CommentRef(n) => Self::Comment(n),
            PyToken::StringRef(n) => Self::Str(n),
            PyToken::Ident(n) => Self::Name(n),
            _ => unreachable!("{:?}", value),
        }
    }
}
