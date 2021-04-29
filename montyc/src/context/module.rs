use std::{path::PathBuf, rc::Rc};

use crate::{ast::module::Module, scope::Scope};

use super::{ModuleRef, global::GlobalContext, local::LocalContext};

use bitflags::bitflags;

bitflags! {
    pub struct ModuleFlags: u32 {
        const EMPTY = 0u32;
        const EXTRA_BUILTINS = 0b0000_0000_0000_0001_u32;
    }
}

#[derive(Debug, Clone)]
pub struct ModuleContext {
    pub(in super) path: PathBuf,
    pub(in super) module: Rc<Module>,
    pub(in super) scope: Rc<dyn Scope>,
    pub source: Rc<str>,
    pub flags: ModuleFlags,
}

impl ModuleContext {
    pub fn local_context<'a>(&'a self, global_context: &'a GlobalContext) -> LocalContext {
        LocalContext {
            global_context,
            module_ref: ModuleRef::from(self.path.clone()),
            scope: self.scope.clone(),
            this: None,
        }
    }
}
