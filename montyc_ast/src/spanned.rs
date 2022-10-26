#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub span: montyc_lexer::Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn reveal<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.span.clone())
    }
}

impl<T> Spanned<T> {
    pub fn new(t: T, span: montyc_lexer::Span) -> Self {
        Self { inner: t, span }
    }

    pub fn span_to<U>(&self, other: &Spanned<U>) -> montyc_lexer::Span {
        self.span.start..other.span.end
    }

    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }

    pub fn replace<U>(self, u: U) -> Spanned<U> {
        Spanned {
            span: self.span,
            inner: u,
        }
    }

    pub fn replace_with<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(Spanned<T>) -> U,
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
