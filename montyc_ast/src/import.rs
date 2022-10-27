use montyc_lexer::{Span, SpanRef};

use crate::primary::Primary;
use crate::spanned::Spanned;
use crate::AstVisitor;

use super::{AstNode, AstObject};

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

    // fn span(&self) -> Option<Span> {
    //     match self {
    //         Import::Names(names) => {
    //             let (head, tail) = names.first().zip(names.last())?;
    //             Some(head.span.start..tail.span.end)
    //         }

    //         Import::From { module, names, .. } => {
    //             let head = &module.span;
    //             let tail = &names.last()?.span;

    //             Some(head.start..tail.end)
    //         }
    //     }
    // }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_import(self, span)
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
