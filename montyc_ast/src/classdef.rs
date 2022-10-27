use montyc_lexer::SpanRef;

use crate::spanned::Spanned;
use crate::{atom::Atom, expr::Expr};

use super::statement::Statement;
use super::{AstNode, AstObject};

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: Spanned<Atom>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for ClassDef {
    fn into_ast_node(&self) -> AstNode {
        AstNode::ClassDef(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    // fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    // where
    //     Self: Sized,
    // {
    //     visitor.visit_funcdef(self, span.or(self.span()))
    // }
}
