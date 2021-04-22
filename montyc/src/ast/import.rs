use std::rc::Rc;

use crate::prelude::*;
use super::{atom::Atom, primary::Primary, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub parent: Rc<Import>,
    pub name: Rc<Spanned<Primary>>,
    pub alias: Option<SpanEntry>,
}

#[derive(Debug, Clone)]
pub enum Import {
    Names(Vec<Rc<Spanned<Primary>>>),
    From {
        module: Rc<Spanned<Primary>>,
        names: Vec<Rc<Spanned<Primary>>>,
        level: usize,
    },
}

impl Import {
    pub fn decls(self: Rc<Self>) -> Vec<ImportDecl> {
        let decls: Vec<ImportDecl> = match self.as_ref().clone() {
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

impl AstObject for Import {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        todo!()
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        match self {
            Import::Names(names) => Some(Box::new(names.clone().into_iter().map(|t| t as Rc<_>))),
            Import::From {
                module,
                names,
                level: _,
            } => Some(Box::new(
                Some(module.clone() as Rc<_>)
                    .into_iter()
                    .chain(names.clone().into_iter().map(|t| t as Rc<_>)),
            )),
        }
    }
}

impl TypedObject for Import {
    fn infer_type<'a>(
        &self,
        _ctx: &crate::context::LocalContext<'a>,
    ) -> crate::Result<LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<()> {
        Ok(())
    }
}

impl LookupTarget for Import {
    fn is_named(&self, target: SpanEntry) -> bool {
        let names = match self {
            Import::Names(names) | Import::From { names, .. } => names,
        };

        names
            .iter()
            .any(|name| matches!(name.inner, Primary::Atomic(Spanned { inner: Atom::Name(n), .. }) if n == target))
    }

    fn name(&self) -> SpanEntry {
        todo!()
    }
}
