use std::rc::Rc;

use crate::typing::TypedObject;

use super::{primary::Primary, AstObject, Spanned};

#[derive(Debug, Clone)]
pub enum Import {
    Names(Vec<Rc<Spanned<Primary>>>),
    From {
        module: Rc<Spanned<Primary>>,
        names: Vec<Rc<Spanned<Primary>>>,
        level: usize,
    },
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
                level,
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
        ctx: &crate::context::LocalContext<'a>,
    ) -> Option<crate::typing::LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, ctx: crate::context::LocalContext<'a>) {
        todo!()
    }
}
