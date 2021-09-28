use std::{
    convert::{TryFrom, TryInto},
    path::PathBuf,
};

derive_everything! {
    #[repr(transparent)]
    pub struct ModuleRef(pub u32);
}

impl TryFrom<ModuleRef> for usize {
    type Error = std::num::TryFromIntError;

    fn try_from(ModuleRef(n): ModuleRef) -> Result<Self, Self::Error> {
        n.try_into()
    }
}

impl TryFrom<usize> for ModuleRef {
    type Error = std::num::TryFromIntError;

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        Ok(Self(n.try_into()?))
    }
}

#[derive(Debug, Clone)]
/// Associated data of a module.
pub struct ModuleData {
    /// The path of the module on disk.
    pub path: PathBuf,

    /// equivalent to `__name__`
    pub name: String,

    /// The module reference, used to refer to the module.
    pub mref: ModuleRef,

    // The modules qualified name.
    pub qualname: Vec<String>,
}
