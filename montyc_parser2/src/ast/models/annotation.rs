use super::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Annotation {
    pub name: Spanned<Atom>,
    pub kind: Spanned<Expr>,
}

impl AstObject for Annotation {
    fn into_ast_node(&self) -> AstNode {
        todo!()
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
