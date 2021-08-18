use montyc_core::SpanRef;

use super::*;

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub reciever: Option<Spanned<Atom>>,
    pub name: Spanned<Atom>,
    pub args: Option<Vec<(SpanRef, Option<Spanned<Expr>>)>>,
    pub body: Vec<Spanned<Statement>>,
    pub decorator_list: Vec<Spanned<Primary>>,
    pub returns: Option<Spanned<Expr>>,
    // type_comment: Option<Expr>,
}

impl AstObject for FunctionDef {
    fn into_ast_node(&self) -> AstNode {
        AstNode::FuncDef(self.clone())
    }

    fn span(&self) -> Option<Span> {
        Some(self.name.span.start..self.body.last()?.span.end)
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        visitor.visit_funcdef(self, self.span())
    }
}

impl FunctionDef {
    pub fn is_dynamically_typed(&self) -> bool {
        self.args
            .as_ref()
            .map(|args| args.iter().any(|arg| arg.1.is_none()))
            .unwrap_or(false)
    }

    pub fn is_ellipsis_stubbed(&self) -> bool {
        match self.body.as_slice() {
            [] => unreachable!(),
            [head] => matches!(head.into_ast_node(), AstNode::Ellipsis(_)),
            [head, tail] => {
                matches!(head.into_ast_node(), AstNode::Str(_))
                    && matches!(tail.into_ast_node(), AstNode::Ellipsis(_))
            }
            _ => false,
        }
    }
}
