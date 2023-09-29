use crate::{atom::Atom, expr::Expr, spanned::Spanned};

use super::*;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Statement {
    Expr(Spanned<expr::Expr>),
    FnDef(Spanned<funcdef::FunctionDef>),
    Ret(Spanned<return_::Return>),
    Asn(Spanned<assign::Assign>),
    Ann(Spanned<ann::Annotation>),
    Import(Spanned<import::Import>),
    Class(Spanned<classdef::ClassDef>),
    If(Spanned<ifstmt::IfChain>),
    While(Spanned<while_::While>),
    Pass(Spanned<Pass>),
    TypeAlias(Spanned<Atom>, Spanned<Expr>),
}

impl AstObject for Statement {
    fn into_ast_node(&self) -> AstNode {
        match self {
            Self::Pass(_) => AstNode::Pass(super::Pass),
            Self::Expr(node) => node.into_ast_node(),
            Self::FnDef(node) => node.into_ast_node(),
            Self::Ret(node) => node.into_ast_node(),
            Self::Asn(node) => node.into_ast_node(),
            Self::Ann(node) => node.into_ast_node(),
            Self::Import(node) => node.into_ast_node(),
            Self::Class(node) => node.into_ast_node(),
            Self::If(node) => node.into_ast_node(),
            Self::While(node) => node.into_ast_node(),
            Self::TypeAlias(_, _) => todo!(),
        }
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        match self {
            Statement::Expr(ref e) => e,
            Statement::FnDef(ref f) => f,
            Statement::Ret(ref r) => r,
            Statement::Asn(ref a) => a,
            Statement::Ann(ref a) => a,
            Statement::Import(ref i) => i,
            Statement::Class(ref c) => c,
            Statement::If(ref i) => i,
            Statement::While(ref w) => w,
            Statement::Pass(_) => self,
            Statement::TypeAlias(_, _) => todo!(),
        }
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        match self {
            Statement::Expr(inner) => inner.visit_with(visitor, span),
            Statement::FnDef(inner) => inner.visit_with(visitor, span),
            Statement::Ret(inner) => inner.visit_with(visitor, span),
            Statement::Asn(inner) => inner.visit_with(visitor, span),
            Statement::Ann(inner) => inner.visit_with(visitor, span),
            Statement::Import(inner) => inner.visit_with(visitor, span),
            Statement::Class(inner) => inner.visit_with(visitor, span),
            Statement::If(inner) => inner.visit_with(visitor, span),
            Statement::While(inner) => inner.visit_with(visitor, span),
            Statement::Pass(_) => super::Pass.visit_with(visitor, span),
            Statement::TypeAlias(_, _) => todo!(),
        }
    }
}
