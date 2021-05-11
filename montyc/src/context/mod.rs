use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::PathBuf,
};

pub(crate) mod global;
pub(crate) mod local;
pub(crate) mod module;
pub(crate) mod resolver;

pub use self::{
    global::GlobalContext,
    local::LocalContext,
    module::{ModuleContext, ModuleFlags},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
pub struct ModuleRef(pub(crate) PathBuf);

impl ModuleRef {
    pub fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish()
    }
}

use std::fmt;

impl fmt::Display for ModuleRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<&str> for ModuleRef {
    fn from(s: &str) -> Self {
        Self(s.into())
    }
}

impl From<ModuleRef> for PathBuf {
    fn from(ModuleRef(inner): ModuleRef) -> Self {
        inner
    }
}
