//! Module objects.

use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

use montyc_core::ModuleRef;
use montyc_parser::ast;

#[derive(Debug, Clone)]
/// Associated data of a module.
pub struct ModuleData {
    /// The AST node of the module, like `body` but as a traditional AST.
    pub ast: ast::Module,

    /// The path of the module on disk.
    pub path: PathBuf,

    /// equivalent to `__name__`
    pub name: String,
}

/// A module object is a representation of a Python module.
#[derive(Debug, Clone)]
pub struct ModuleObject {
    /// The AST body of the module, as a graph of nodes.
    pub data: Rc<ModuleData>,

    /// The module reference, used to refer to the module.
    pub mref: ModuleRef,
}

impl ModuleObject {
    /// Create a new ModuleObject from a path and AST and module reference.
    #[inline]
    pub fn new(path: PathBuf, ast: ast::Module, mref: ModuleRef, name: String) -> Self {
        Self {
            data: Rc::new(ModuleData { path, name, ast }),
            mref,
        }
    }

    /// The path to the module.
    #[inline]
    pub fn path(&self) -> &Path {
        &self.data.path
    }
}
