use std::{cell::RefCell, marker::PhantomData, rc::Rc};

use crate::prelude::*;

use super::{collect_subnodes, LookupOrder, ScopeIter, ScopeRoot, ScopedObject};

// -- LocalScope

#[derive(Debug, Clone)]
pub struct LocalScope<T> {
    pub _t: PhantomData<Rc<T>>,
    pub inner: OpaqueScope,
}

impl<T> From<OpaqueScope> for LocalScope<T>
where
    T: AstObject,
{
    fn from(scope: OpaqueScope) -> Self {
        assert_matches!(&*scope.root.borrow(), ScopeRoot::AstObject(o) if o.as_ref().downcast_ref::<T>().is_some());

        Self {
            inner: scope,
            _t: PhantomData,
        }
    }
}

impl<T> From<T> for LocalScope<T>
where
    T: AstObject,
{
    fn from(root: T) -> Self {
        let root = Rc::new(root);
        let nodes = collect_subnodes(root.as_ref());

        Self {
            inner: OpaqueScope {
                root: RefCell::new(ScopeRoot::AstObject(root as Rc<dyn AstObject>)),
                nodes,
                module_ref: None,
                parent: None,
            },
            _t: PhantomData,
        }
    }
}

impl<T: std::fmt::Debug> Scope for LocalScope<T> {
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject> + 'b)> {
        self.inner.iter()
    }

    fn root<'b>(&'b self) -> ScopeRoot {
        self.inner.root()
    }

    fn lookup_with(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        self.inner.lookup_with(key)
    }

    fn walk<'a, 'b>(&'b self, global_context: &'a GlobalContext) -> ScopeIter<'a, 'b, 'b> {
        self.inner.walk(global_context)
    }

    fn lookup_any(
        &self,
        target: SpanRef,
        global_context: &GlobalContext,
        order: LookupOrder,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        self.inner.lookup_any(target, global_context, order)
    }

    fn module_ref(&self) -> ModuleRef {
        self.inner.module_ref.clone().unwrap()
    }

    fn parent(&self) -> Option<Rc<dyn Scope>> {
        self.inner.parent()
    }
}
