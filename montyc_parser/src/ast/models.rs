use std::convert::TryFrom;

use montyc_core::SpanRef;

use super::*;
pub use super::{atom::*, expr::*, ifstmt::*, import::*, primary::*};

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub reciever: Option<Spanned<Atom>>,
    pub name: Spanned<Atom>,
    pub args: Option<Vec<(SpanRef, Option<Spanned<Expr>>)>>,
    pub body: Vec<Spanned<Statement>>,
    pub decorator_list: Vec<Spanned<Primary>>,
    pub returns: Option<Spanned<Expr>>,
    // type_comment: Option<Expr>,
}

impl AstObject for FunctionDef {
    fn into_ast_node(&self) -> AstNode {
        AstNode::FuncDef(self.clone())
    }

    fn span(&self) -> Option<Span> {
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_funcdef(self)
    }
}

impl FunctionDef {
    pub fn is_dynamically_typed(&self) -> bool {
        self.args
            .as_ref()
            .map(|args| args.iter().any(|arg| arg.1.is_none()))
            .unwrap_or(false)
    }

    pub fn is_ellipsis_stubbed(&self) -> bool {
        match self.body.as_slice() {
            [] => unreachable!(),
            [head] => matches!(head.into_ast_node(), AstNode::Ellipsis(_)),
            [head, tail] => {
                matches!(head.into_ast_node(), AstNode::Str(_))
                    && matches!(tail.into_ast_node(), AstNode::Ellipsis(_))
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct While {
    pub test: Spanned<Expr>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for While {
    fn into_ast_node(&self) -> AstNode {
        AstNode::While(self.clone())
    }

    fn span(&self) -> Option<Span> {
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_while(self)
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Spanned<Expr>>,
}

impl AstObject for Return {
    fn into_ast_node(&self) -> AstNode {
        AstNode::Ret(self.clone())
    }

    fn span(&self) -> Option<Span> {
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_return(self)
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
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_classdef(self)
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
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_assign(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Annotation {
    pub name: Spanned<Atom>,
    pub kind: Spanned<Expr>,
}

impl AstObject for Annotation {
    fn into_ast_node(&self) -> AstNode {
        todo!()
    }

    fn span(&self) -> Option<Span> {
        None
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, _visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        todo!()
    }
}

pub use super::statement::Statement;

#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Spanned<Statement>>,
}

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
