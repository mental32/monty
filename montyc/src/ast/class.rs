use std::rc::Rc;

use crate::{
    prelude::*,
    typing::{ClassType, TypeDescriptor},
};

use super::{atom::Atom, primary::Primary, stmt::Statement, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: Rc<Spanned<Atom>>,
    pub decorator_list: Vec<Rc<Spanned<Primary>>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl ClassDef {
    pub fn as_type_descriptor(&self, kind: LocalTypeId, mref: ModuleRef) -> TypeDescriptor {
        TypeDescriptor::Class(ClassType {
            name: self.name.name().unwrap(),
            kind,
            mref: mref.clone(),
        })
    }
}

impl AstObject for ClassDef {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        Some(Box::new(std::iter::empty()))
    }
}

impl TypedObject for ClassDef {
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(TypeMap::TYPE)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        let scope = LocalScope::from(self.clone());

        for node in scope.inner.nodes.iter().map(|n| n.unspanned()) {
            node.typecheck(&ctx)?;
        }

        Ok(())
    }
}

impl LookupTarget for ClassDef {
    fn is_named(&self, target: SpanRef) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> Option<SpanRef> {
        self.name.name()
    }
}
