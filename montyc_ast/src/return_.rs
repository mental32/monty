use crate::expr::Expr;
use crate::spanned::Spanned;
use crate::AstObject;

#[derive(Debug, Clone)]
pub struct Return {
    pub val: Spanned<Expr>,
}

impl AstObject for Return {
    fn into_ast_node(&self) -> crate::AstNode {
        crate::AstNode::Ret(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }
}
