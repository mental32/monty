use std::{path::PathBuf, rc::Rc};

use crate::{ast::module::Module, scope::Scope};

use super::{ModuleRef, global::GlobalContext, local::LocalContext};

#[derive(Debug, Clone)]
pub struct ModuleContext {
    pub(in super) path: PathBuf,
    pub(in super) module: Rc<Module>,
    pub(in super) scope: Rc<dyn Scope>,
    pub source: Rc<str>,
}

impl ModuleContext {
    pub fn local_context<'a>(&'a self, global_context: &'a GlobalContext) -> LocalContext {
        LocalContext {
            global_context,
            module_ref: ModuleRef::from(self.path.clone()),
            scope: self.scope.clone(),
            this: None,
            parent: None,
        }
    }
}
