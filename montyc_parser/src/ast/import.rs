use logos::Span;
use montyc_core::SpanRef;

use crate::{spanned::Spanned, AstNode, AstObject, AstVisitor};

use super::Primary;

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub parent: Import,
    pub name: Spanned<Primary>,
    pub alias: Option<SpanRef>,
}

#[derive(Debug, Clone)]
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
        visitor.visit_import(self)
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
