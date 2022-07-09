use super::*;
use crate::ast::{AstNode, AstObject, AstVisitor};

#[derive(Debug, Clone)]
pub enum BranchTail {
    Else(Spanned<Statement>),
    If(Spanned<If>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Spanned<Expr>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for If {
    fn into_ast_node(&self) -> AstNode {
        unimplemented!()
    }

    fn type_name(&self) -> &str {
        "If"
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

#[derive(Debug, Clone)]
pub struct IfChain {
    pub branches: Vec<Spanned<If>>,
    pub orelse: Option<Vec<Spanned<Statement>>>,
}

impl AstObject for IfChain {
    fn into_ast_node(&self) -> AstNode {
        AstNode::If(self.clone())
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
