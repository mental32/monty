use super::*;
use crate::ast::{AstNode, AstObject, AstVisitor};

#[derive(Debug, Clone)]
pub struct While {
    pub test: Spanned<Expr>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for While {
    fn into_ast_node(&self) -> AstNode {
        AstNode::While(self.clone())
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
