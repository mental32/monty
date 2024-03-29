use montyc_lexer::{Span, SpanRef};

use crate::spanned::Spanned;
use crate::AstVisitor;
use crate::{atom::Atom, expr::Expr};

use super::statement::Statement;
use super::{AstNode, AstObject};

// some "name : type"
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FunctionDefParam {
    pub named: SpanRef,
    pub annotation: Option<Spanned<Expr>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FunctionDef {
    pub receiver: Option<Spanned<Atom>>,
    pub name: Spanned<Atom>,
    pub generic_params: Option<Vec<Spanned<FunctionDefParam>>>,
    pub args: Vec<Spanned<FunctionDefParam>>,
    pub body: Vec<Spanned<Statement>>,
    pub decorator_list: Vec<Spanned<Expr>>,
    pub returns: Option<Spanned<Expr>>,
    // type_comment: Option<Expr>,
}

impl AstObject for FunctionDef {
    fn into_ast_node(&self) -> AstNode {
        AstNode::FuncDef(self.clone())
    }

    // fn span(&self) -> Option<Span> {
    //     Some(self.name.span.start..self.body.last()?.span.end)
    // }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_funcdef(self, span)
    }
}

impl FunctionDef {
    pub fn is_dynamically_typed(&self) -> bool {
        self.args.iter().any(|arg| arg.inner.annotation.is_none())
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
