use std::rc::Rc;

use crate::prelude::*;

// -- ScopeObject

pub struct ScopedObject {
    pub scope: Rc<dyn Scope>,
    pub object: Rc<dyn AstObject>,
}

impl ScopedObject {
    pub fn with_context<F, T>(&self, global_context: &GlobalContext, f: F) -> T
    where
        F: Fn(LocalContext, Rc<dyn AstObject>) -> T,
    {
        let scope = Rc::new(WrappedScope {
            inner: Rc::clone(&self.scope),
            parent: self.scope.parent(),
        });

        let ctx = LocalContext {
            scope,
            this: Some(Rc::clone(&self.object)),
            global_context,
            module_ref: self.scope.module_ref(),
        };

        f(ctx, Rc::clone(&self.object))
    }
}

impl std::fmt::Debug for ScopedObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ScopedObject")
            .field("object", &self.object)
            .field("scope", &self.scope.root())
            .finish()
    }
}
