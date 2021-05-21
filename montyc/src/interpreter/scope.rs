use std::{any::Any, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::prelude::*;

use super::object::Object;


#[derive(Debug)]
pub struct DynamicScope {
    pub(super) root: ScopeRoot,
    pub(super) mref: ModuleRef,
    pub(super) namespace: DashMap<(NonZeroUsize, NonZeroUsize), Rc<Object>>,
}

impl Scope for DynamicScope {
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = crate::scope::ScopedObject> + 'b)> {
        box std::iter::empty()
    }

    fn define(&self, name: (NonZeroUsize, NonZeroUsize), value: &dyn Any) {
        self.namespace.insert(name, value.downcast_ref::<Rc<Object>>().unwrap().clone());
    }

    fn root(&self) -> ScopeRoot {
        self.root.clone()
    }

    fn parent(&self) -> Option<Rc<dyn Scope>> {
        None
    }

    fn module_ref(&self) -> ModuleRef {
        self.mref.clone()
    }

    fn walk<'a, 'b>(
        &'b self,
        _global_context: &'a GlobalContext,
    ) -> crate::scope::ScopeIter<'a, 'b, 'b> {
        todo!()
    }

    fn lookup_with(&self, _key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        todo!()
    }

    fn lookup_any(
        &self,
        _target: SpanRef,
        _global_context: &GlobalContext,
        _order: crate::scope::LookupOrder,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        todo!()
    }
}
