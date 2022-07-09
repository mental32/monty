use super::*;
use crate::ast::{AstNode, AstObject, AstVisitor};

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

    fn type_name(&self) -> &str {
        "ClassDef"
    }

    fn call_visitor_handler<T>(
        &self,
        visitor: &dyn AstVisitor<T>,
        span: Option<montyc_core::Span>,
    ) -> T
    where
        Self: Sized,
    {
        visitor.visit_ClassDef(self, span)
    }
}
