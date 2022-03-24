use super::AstObject;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T, S = logos::Span> {
    pub span: S,
    pub inner: T,
}

impl<T> Spanned<T, logos::Span> {
    pub fn new(inner: T, span: logos::Span) -> Self {
        Self { span, inner }
    }

    pub fn reveal<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.span.clone())
    }
}

impl<T, S> Spanned<T, S>
where
    S: Clone,
{
    pub fn map<U, F>(self, f: F) -> Spanned<U, S>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }

    pub fn replace<U>(self, u: U) -> Spanned<U, S> {
        Spanned {
            span: self.span,
            inner: u,
        }
    }

    pub fn replace_with<U, F>(self, f: F) -> Spanned<U, S>
    where
        F: FnOnce(Spanned<T, S>) -> U,
    {
        Spanned {
            span: self.span.clone(),
            inner: f(Self {
                inner: self.inner,
                span: self.span,
            }),
        }
    }
}

impl<T> AstObject for Spanned<T>
where
    T: AstObject,
{
    fn into_ast_node(&self) -> super::AstNode {
        self.inner.into_ast_node()
    }

    fn type_name(&self) -> &str {
        self.inner.type_name()
    }

    fn call_visitor_handler<U>(
        &self,
        visitor: &dyn super::AstVisitor<U>,
        span: Option<montyc_core::Span>,
    ) -> U
    where
        Self: Sized,
    {
        self.inner
            .call_visitor_handler(visitor, span.or(Some(self.span.clone())))
    }
}
