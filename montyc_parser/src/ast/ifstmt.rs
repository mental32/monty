use super::*;

#[derive(Debug, Clone)]
pub enum BranchTail {
    Else(Spanned<Statement>),
    If(Spanned<If>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Spanned<Expr>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for If {
    fn into_ast_node(&self) -> AstNode {
        AstNode::If(self.clone())
    }

    fn span(&self) -> Option<Span> {
        None
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_expr(&self.test.inner)
    }
}

#[derive(Debug, Clone)]
pub struct IfChain {
    pub branches: Vec<Spanned<If>>,
    pub orelse: Option<Vec<Spanned<Statement>>>,
}

impl AstObject for IfChain {
    fn into_ast_node(&self) -> AstNode {
        unreachable!()
    }

    fn span(&self) -> Option<Span> {
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_ifstmt(&self)
    }
}
