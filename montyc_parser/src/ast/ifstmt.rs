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
        unimplemented!()
    }

    fn span(&self) -> Option<Span> {
        Some(
            self.test.span.start
                ..self
                    .body
                    .last()
                    .map(|node| node.span.end)
                    .unwrap_or(self.test.span.end),
        )
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_expr(&self.test.inner, self.span())
    }
}

#[derive(Debug, Clone)]
pub struct IfChain {
    pub branches: Vec<Spanned<If>>,
    pub orelse: Option<Vec<Spanned<Statement>>>,
}

impl AstObject for IfChain {
    fn into_ast_node(&self) -> AstNode {
        AstNode::If(self.clone())
    }

    fn span(&self) -> Option<Span> {
        let head = &self.branches[0];
        let tail = self
            .orelse
            .as_ref()
            .and_then(|or| or.last().map(|node| node.span.end))
            .or_else(|| self.branches.last().map(|br| br.span.end))
            .unwrap_or(head.span.end);

        Some(head.span.start..tail)
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_ifstmt(self, self.span())
    }
}
