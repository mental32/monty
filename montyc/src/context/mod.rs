use std::path::PathBuf;

pub(crate) mod global;
pub(crate) mod local;
pub(crate) mod module;
pub(crate) mod resolver;
pub mod codegen;

pub use self::{global::GlobalContext, local::LocalContext, module::{ModuleContext, ModuleFlags}};

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
            .and_then(|name| if name.to_string_lossy().starts_with("__monty") { Some(true) } else { None })
            .unwrap_or_else(|| self.0.exists())
    }
}
