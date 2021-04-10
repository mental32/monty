use std::rc::Rc;

use crate::typing::TypedObject;

use super::{AstObject, Spanned, atom::Atom, primary::Primary, stmt::Statement};


#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: Rc<Spanned<Atom>>,
    pub decorator_list: Vec<Rc<Spanned<Primary>>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl AstObject for ClassDef {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        todo!()
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        Some(Box::new(std::iter::empty()))
    }
}

impl TypedObject for ClassDef {
    fn infer_type<'a>(&self, ctx: &crate::context::LocalContext<'a>) -> Option<crate::typing::LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, ctx: crate::context::LocalContext<'a>) {
        todo!()
    }
}
