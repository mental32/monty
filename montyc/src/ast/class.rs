use std::rc::Rc;

use crate::{
    class::Class,
    context::LocalContext,
    scope::{downcast_ref, LocalScope, LookupTarget},
    typing::{LocalTypeId, TypedObject},
};

use super::{atom::Atom, primary::Primary, stmt::Statement, AstObject, Spanned};

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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        let this = ctx.this.as_ref().unwrap();

        if let Some(type_id) = ctx
            .global_context
            .is_builtin(this.as_ref(), &ctx.module_ref)
        {
            return Some(type_id);
        } else {
            todo!();
        }
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        let scope = LocalScope::from(self.clone());

        for node in scope.inner.nodes.iter().map(|n| n.unspanned()) {
            node.typecheck(ctx.clone())
        }
    }
}

impl LookupTarget for ClassDef {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
        self.name.name()
    }
}
