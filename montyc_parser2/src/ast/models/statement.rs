use super::*;
use crate::ast::{AstNode, AstObject, AstVisitor};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    FnDef(FunctionDef),
    Ret(Return),
    Asn(Assign),
    Ann(Annotation),
    Import(Import),
    Class(ClassDef),
    If(IfChain),
    While(While),
    Pass,
}

impl AstObject for Statement {
    fn into_ast_node(&self) -> AstNode {
        match self {
            Self::Pass => todo!(),
            Self::Expr(node) => node.into_ast_node(),
            Self::FnDef(node) => node.into_ast_node(),
            Self::Ret(node) => node.into_ast_node(),
            Self::Asn(node) => node.into_ast_node(),
            Self::Ann(node) => node.into_ast_node(),
            Self::Import(node) => node.into_ast_node(),
            Self::Class(node) => node.into_ast_node(),
            Self::If(node) => node.into_ast_node(),
            Self::While(node) => node.into_ast_node(),
        }
    }

    fn type_name(&self) -> &str {
        todo!()
    }

    fn call_visitor_handler<T>(
        &self,
        visitor: &dyn AstVisitor<T>,
        span: Option<montyc_core::Span>,
    ) -> T
    where
        Self: Sized,
    {
        todo!()
    }
}
