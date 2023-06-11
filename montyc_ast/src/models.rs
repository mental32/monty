use std::convert::TryFrom;

use montyc_core::SpanRef;

use super::*;
pub use super::{atom::*, expr::*, funcdef::*, ifstmt::*, import::*, primary::*};

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Result<Spanned<Expr>, Spanned<()>>,
}

impl AstObject for Return {
    fn into_ast_node(&self) -> AstNode {
        AstNode::Ret(self.clone())
    }

    fn span(&self) -> Option<Span> {
        Some(match &self.value {
            Ok(val) => val.span.clone(),
            Err(tok) => tok.span.clone(),
        })
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_return(self, span)
    }
}

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: Spanned<Atom>,
    pub decorator_list: Vec<Spanned<Primary>>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for ClassDef {
    fn into_ast_node(&self) -> AstNode {
        AstNode::ClassDef(self.clone())
    }

    fn span(&self) -> Option<Span> {
        Some(
            self.name.span.start
                ..self
                    .body
                    .last()
                    .map(|node| node.span.end)
                    .unwrap_or(self.name.span.end),
        )
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_classdef(self, span)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Spanned<Primary>,
    pub value: Spanned<Expr>,
    pub kind: Option<Spanned<Expr>>,
}

impl AstObject for Assign {
    fn into_ast_node(&self) -> AstNode {
        AstNode::Assign(self.clone())
    }

    fn span(&self) -> Option<Span> {
        Some(self.name.span.start..self.value.span.end)
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_assign(self, span)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Annotation {
    pub name: Spanned<Atom>,
    pub kind: Spanned<Expr>,
}

impl AstObject for Annotation {
    fn into_ast_node(&self) -> AstNode {
        AstNode::Annotation(self.clone())
    }

    fn span(&self) -> Option<Span> {
        None
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_annotation(self, span)
    }
}

pub use super::statement::Statement;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringRef(pub SpanRef);

impl TryFrom<Atom> for StringRef {
    type Error = Atom;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        match value {
            Atom::Str(n) => Ok(Self(n)),
            _ => Err(value),
        }
    }
}

impl From<StringRef> for SpanRef {
    fn from(st: StringRef) -> Self {
        st.0
    }
}
