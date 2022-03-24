use super::*;
use crate::ast::{AstNode, AstObject, AstVisitor};

use super::Primary;

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub parent: Import,
    pub name: Spanned<Primary>,
    pub alias: Option<SpanRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    Names(Vec<Spanned<Primary>>),
    From {
        module: Spanned<Primary>,
        names: Vec<Spanned<Primary>>,
        level: usize,
    },
}

impl AstObject for Import {
    fn into_ast_node(&self) -> AstNode {
        AstNode::Import(self.clone())
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

impl Import {
    pub fn decls(&self) -> Vec<ImportDecl> {
        let decls: Vec<ImportDecl> = match self.clone() {
            Import::Names(names) => names
                .iter()
                .map(|target| ImportDecl {
                    parent: self.clone(),
                    name: target.clone(),
                    alias: None,
                })
                .collect(),

            Import::From {
                module: _,
                names,
                level: _,
            } => names
                .iter()
                .map(|target| ImportDecl {
                    parent: self.clone(),
                    name: target.clone(),
                    alias: None,
                })
                .collect(),
        };

        decls
    }
}
