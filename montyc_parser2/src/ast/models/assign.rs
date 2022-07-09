use super::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Spanned<Primary>,
    pub value: Spanned<Expr>,
    pub kind: Option<Spanned<Expr>>,
}

impl AstObject for Assign {
    fn type_name(&self) -> &str {
        "Assign"
    }

    fn call_visitor_handler<T>(
        &self,
        visitor: &dyn AstVisitor<T>,
        span: Option<montyc_core::Span>,
    ) -> T
    where
        Self: Sized,
    {
        visitor.visit_Assign(self, span)
    }

    fn into_ast_node(&self) -> AstNode {
        AstNode::Assign(self.clone())
    }
}
