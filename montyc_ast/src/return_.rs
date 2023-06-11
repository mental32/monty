use crate::expr::Expr;
use crate::spanned::Spanned;
use crate::AstObject;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Return {
    pub value: Option<Spanned<Expr>>,
}

impl AstObject for Return {
    fn into_ast_node(&self) -> crate::AstNode {
        crate::AstNode::Ret(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(
        &self,
        visitor: &mut dyn crate::AstVisitor<U>,
        span: Option<montyc_lexer::Span>,
    ) -> U
    where
        Self: Sized,
    {
        visitor.visit_return(self, span)
    }
}
