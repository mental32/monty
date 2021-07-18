use std::{path::{Path, PathBuf}, rc::Rc};

use montyc_core::ModuleRef;
use montyc_parser::ast;

use crate::grapher;


/// A module object is a representation of a Python module.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ModuleObject {
    /// The path of the module on disk.
    pub path: PathBuf,

    /// equivalent to `__name__`
    pub name: String,

    /// The AST body of the module, as a graph of nodes.
    pub body: Rc<grapher::AstNodeGraph>,

    /// The AST node of the module, like `body` but as a traditional AST.
    pub ast: ast::Module,

    /// The module reference, used to refer to the module.
    pub mref: ModuleRef,
}

impl ModuleObject {
    /// Create a new ModuleObject from a path and AST and module reference.
    #[inline]
    pub fn new(path: PathBuf, ast: ast::Module, mref: ModuleRef, name: String) -> Self {
        let body = grapher::NewType(ast.clone()).into();

        Self {
            path,
            body: Rc::new(body),
            ast,
            mref,
            name,
        }
    }

    /// The path to the module.
    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }
}
