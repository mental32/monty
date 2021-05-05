use std::rc::Rc;

use crate::prelude::*;

#[derive(Debug)]
pub struct Renamed {
    pub(in crate) inner: Rc<dyn AstObject>,
    pub(in crate) name: SpanEntry,
    pub(in crate) mref: ModuleRef,
}

impl TypedObject for Renamed {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        self.inner.infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        self.inner.typecheck(ctx)
    }
}

impl LookupTarget for Renamed {
    fn is_named(&self, target: SpanEntry) -> bool {
        self.name == target
    }

    fn renamed_properties(&self) -> Option<ModuleRef> {
        Some(self.mref.clone())
    }

    fn name(&self) -> SpanEntry {
        self.name.clone()
    }
}

impl AstObject for Renamed {
    fn span(&self) -> Option<logos::Span> {
        self.inner.span()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        self.inner.unspanned()
    }

    fn walk(&self) -> Option<crate::ast::ObjectIter> {
        self.inner.walk()
    }
}
