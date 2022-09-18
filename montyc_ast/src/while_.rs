use crate::expr::Expr;
use crate::spanned::Spanned;
use crate::statement::Statement;
use crate::{AstNode, AstObject};

#[derive(Debug, Clone)]
pub struct While {
    pub test: Spanned<Expr>,
    pub body: Vec<Spanned<Statement>>,
}

impl AstObject for While {
    fn into_ast_node(&self) -> AstNode {
        AstNode::While(self.clone())
    }

    // fn span(&self) -> Option<Span> {
    //     Some(
    //         self.test.span.start
    //             ..self
    //                 .body
    //                 .last()
    //                 .map(|node| node.span.end)
    //                 .unwrap_or(self.test.span.end),
    //     )
    // }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    // fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    // where
    //     Self: Sized,
    // {
    //     visitor.visit_while(self, span)
    // }
}
