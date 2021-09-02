//! Module objects.

use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

use montyc_core::{ModuleData, ModuleRef};
use montyc_parser::ast;

/// A module object is a representation of a Python module.
#[derive(Debug, Clone)]
pub struct ModuleObject {
    /// The AST body of the module, as a graph of nodes.
    pub data: Rc<ModuleData>,

    /// The module reference, used to refer to the module.
    pub mref: ModuleRef,

    /// The module ast object.
    pub ast: Rc<ast::Module>,
}

impl ModuleObject {
    /// Create a new ModuleObject from a path and AST and module reference.
    #[inline]
    pub fn new(
        path: PathBuf,
        ast: ast::Module,
        mref: ModuleRef,
        name: String,
        qualname: Vec<String>,
    ) -> Self {
        Self {
            data: Rc::new(ModuleData {
                path,
                name,
                mref,
                qualname,
            }),
            mref,
            ast: Rc::new(ast),
        }
    }

    /// The path to the module.
    #[inline]
    pub fn path(&self) -> &Path {
        &self.data.path
    }
}
