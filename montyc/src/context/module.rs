use std::{num::NonZeroUsize, path::PathBuf, rc::Rc};

use crate::{
    ast::{expr::Expr, module::Module},
    prelude::Spanned,
    scope::Scope,
};

use super::{global::GlobalContext, local::LocalContext, ModuleRef};

use bitflags::bitflags;
use dashmap::DashMap;

bitflags! {
    pub struct ModuleFlags: u32 {
        const EMPTY = 0u32;
        const EXTRA_BUILTINS = 0b0000_0000_0000_0001_u32;
    }
}

#[derive(Debug, Clone)]
pub struct ModuleContext {
    pub(super) path: PathBuf,
    pub(crate) module: Rc<Module>,
    pub(super) scope: Rc<dyn Scope>,
    pub source: Rc<str>,
    pub flags: ModuleFlags,
    pub globals: DashMap<NonZeroUsize, Rc<Spanned<Expr>>>,
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

    pub fn module_ref(&self) -> ModuleRef {
        ModuleRef(self.path.clone())
    }

    pub fn scope(&self) -> Rc<dyn Scope> {
        Rc::clone(&self.scope)
    }
}
