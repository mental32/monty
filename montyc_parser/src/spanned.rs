#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub span: logos::Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn reveal<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.span.clone())
    }
}

impl<T> Spanned<T> {
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
