use std::{marker::PhantomData, rc::Rc};

use crate::prelude::*;

use super::{ScopeIter, ScopeRoot, ScopedObject, collect_subnodes};


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
        assert_matches!(&scope.root, ScopeRoot::AstObject(o) if downcast_ref::<T>(o.as_ref()).is_some());

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
                root: ScopeRoot::AstObject(root as Rc<dyn AstObject>),
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
        self.inner.root.clone()
    }

    fn lookup_with(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        self.inner.lookup_with(key)
    }

    fn walk<'a, 'b>(&'b self, global_context: &'a GlobalContext) -> ScopeIter<'a, 'b, 'b> {
        self.inner.walk(global_context)
    }

    fn lookup_any(
        &self,
        target: SpanEntry,
        global_context: &GlobalContext,
    ) -> Vec<Rc<dyn AstObject>> {
        self.inner.lookup_any(target, global_context)
    }

    fn module_ref(&self) -> ModuleRef {
        self.inner.module_ref.clone().unwrap()
    }

    fn parent(&self) -> Option<Rc<dyn Scope>> {
        self.inner.parent()
    }
}
