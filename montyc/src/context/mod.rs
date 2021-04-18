use std::path::PathBuf;

pub(crate) mod global;
pub(crate) mod local;
pub(crate) mod module;
pub(crate) mod resolver;

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
pub struct ModuleRef(pub(crate) PathBuf);

impl From<ModuleRef> for PathBuf {
    fn from(ModuleRef(inner): ModuleRef) -> Self {
        inner
    }
}

pub use self::{global::GlobalContext, local::LocalContext, module::ModuleContext};
