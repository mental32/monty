use crate::atom::Atom;
use crate::expr::Expr;
use crate::spanned::Spanned;
use crate::AstObject;

#[derive(Debug, Clone)]
pub struct Assign {
    name: Spanned<Atom>,
    annotation: Option<Spanned<Expr>>,
    value: Spanned<Expr>,
}

impl AstObject for Assign {
    fn into_ast_node(&self) -> crate::AstNode {
        crate::AstNode::Assign(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }
}
