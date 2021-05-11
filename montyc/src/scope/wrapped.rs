use std::rc::Rc;

use crate::prelude::*;

use super::{LookupOrder, ScopeIter, ScopeRoot, ScopedObject};

// -- ChainedScope

#[derive(Debug)]
pub struct ChainedScope {
    pub inner: Rc<dyn Scope>,
    pub parent: Option<Rc<dyn Scope>>,
}

impl Scope for ChainedScope {
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject> + 'b)> {
        self.inner.iter()
    }

    fn root(&self) -> ScopeRoot {
        self.inner.root()
    }

    fn module_ref(&self) -> ModuleRef {
        self.inner.module_ref()
    }

    fn walk<'a, 'b>(&'b self, global_context: &'a GlobalContext) -> ScopeIter<'a, 'b, 'b> {
        self.inner.walk(global_context)
    }

    fn lookup_with(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        self.inner.lookup_with(key)
    }

    fn lookup_any(
        &self,
        target: SpanRef,
        global_context: &GlobalContext,
        order: LookupOrder,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        self.inner.lookup_any(target, global_context, order)
    }

    fn parent(&self) -> Option<Rc<dyn Scope>> {
        self.parent.clone()
    }
}
