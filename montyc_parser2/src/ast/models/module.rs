use super::*;

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for Module {
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
