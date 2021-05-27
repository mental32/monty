use std::{num::NonZeroUsize, path::PathBuf, rc::Rc};

use crate::{
    ast::{expr::Expr, module::Module},
    prelude::Spanned,
    scope::Scope,
};

use super::{global::GlobalContext, local::LocalContext, ModuleRef};

use dashmap::DashMap;

#[derive(Debug, Clone)]
pub struct ModuleContext {
    pub(super) path: PathBuf,
    pub module: Rc<Module>,
    pub scope: Rc<dyn Scope>,
    pub source: Rc<str>,
    pub globals: DashMap<NonZeroUsize, Rc<Spanned<Expr>>>,
}

impl ModuleContext {
    pub fn unbound_local_context<'a>(&'a self, global_context: &'a GlobalContext) -> LocalContext {
        LocalContext {
            global_context,
            module_ref: ModuleRef::from(self.path.clone()),
            scope: self.scope.clone(),
            this: None,
            current_branch: None,
        }
    }

    pub fn module_ref(&self) -> ModuleRef {
        ModuleRef(self.path.clone())
    }

    pub fn scope(&self) -> Rc<dyn Scope> {
        Rc::clone(&self.scope)
    }
}
