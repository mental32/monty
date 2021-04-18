use std::path::PathBuf;

pub(crate) mod global;
pub(crate) mod local;
pub(crate) mod module;
pub(crate) mod resolver;

pub use self::{global::GlobalContext, local::LocalContext, module::ModuleContext};

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
pub struct ModuleRef(pub(crate) PathBuf);

impl From<ModuleRef> for PathBuf {
    fn from(ModuleRef(inner): ModuleRef) -> Self {
        inner
    }
}

impl ModuleRef {
    pub fn exists(&self) -> bool {
        self.0
            .file_name()
            .map(|name| name.to_string_lossy().starts_with("__monty"))
            .unwrap_or_else(|| self.0.exists())
    }
}
