use crate::atom::Atom;
use crate::expr::Expr;
use crate::spanned::Spanned;
use crate::AstObject;

#[derive(Debug, Clone)]
pub struct Annotation {
    pub name: Spanned<Atom>,
    pub annotation: Spanned<Expr>,
}

impl AstObject for Annotation {
    fn into_ast_node(&self) -> crate::AstNode {
        crate::AstNode::Annotation(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }
}
