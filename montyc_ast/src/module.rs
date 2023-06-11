use montyc_lexer::Span;

use super::statement::Statement;
use super::{AstNode, AstObject};

use crate::spanned::Spanned;
use crate::AstVisitor;

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub body: Vec<Spanned<Statement>>,
}

impl Module {
    pub fn span(&self) -> Option<Span> {
        let fst = self.body.first()?;
        let lst = self.body.last()?;

        Some(fst.span.start..lst.span.end)
    }
}

impl AstObject for Module {
    fn into_ast_node(&self) -> AstNode {
        todo!()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        todo!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_module(self, span)
    }
}
