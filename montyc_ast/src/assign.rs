use crate::atom::Atom;
use crate::expr::Expr;
use crate::primary::Primary;
use crate::spanned::Spanned;
use crate::AstObject;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Assign {
    pub name: Spanned<Primary>,
    pub annotation: Option<Spanned<Expr>>,
    pub value: Spanned<Expr>,
}

impl AstObject for Assign {
    fn into_ast_node(&self) -> crate::AstNode {
        crate::AstNode::Assign(self.clone())
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
        visitor.visit_assign(self, span)
    }
}
