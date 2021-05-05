use std::{fmt, num::NonZeroUsize, rc::Rc};

use crate::prelude::*;

#[derive(Clone)]
pub struct PhantomObject {
    pub(crate) name: SpanRef,
    pub(crate) infer_type: for<'a> fn(&'a LocalContext<'a>) -> crate::Result<LocalTypeId>,
}

impl fmt::Debug for PhantomObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PhantomObject").finish()
    }
}

impl AstObject for PhantomObject {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<crate::ast::ObjectIter> {
        None
    }
}

impl LookupTarget for PhantomObject {
    fn is_named(&self, target: SpanRef) -> bool {
        Some(target == self.name).unwrap_or(false)
    }

    fn name(&self) -> Option<SpanRef> {
        Some(self.name)
    }
}

impl TypedObject for PhantomObject {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        (self.infer_type)(ctx)
    }

    fn typecheck<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<()> {
        Ok(())
    }
}
