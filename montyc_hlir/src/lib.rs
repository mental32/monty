//! High Level Intermediate Representation (HLIR) for Monty.
//!
//! Previously `montyc` operated almost exclusively only on the AST
//! representation of a program for interpretation, verification
//! (e.g. typechecking), and lowering for codegen. And on the
//! surface it worked okay but it was becoming increasingly
//! uncomfortable to work with the programs structure (for
//! instance when evalutating it with the comptime interpreter.)
//!
//! HLIR was designed to be constructable from any AST and is made
//! to alleviate the problems working purely with an AST-based representation.

mod grapher;
pub mod interpreter;
pub mod typing;

use std::path::{Path, PathBuf};

use montyc_core::{ModuleRef, TypeId};
use montyc_parser::ast;
use petgraph::graph::NodeIndex;

pub type ObjectGraph = petgraph::graph::DiGraph<Object, ()>;
pub type ObjectGraphIndex = NodeIndex<u32>;

/// HLIR objects are dynamic/reflective representations of objects that we can typecheck and compile.
///
/// They represent an object during compilation and keep track of
/// properties such as the object's type or attributes.
///
#[derive(Debug)]
pub struct Object {
    type_id: TypeId,

    properties: crate::interpreter::PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
}

#[derive(Debug, Clone)]
pub struct ModuleObject {
    path: PathBuf,
    pub(crate) body: grapher::AstNodeGraph,
    ast: ast::Module,
    mref: ModuleRef,
}

impl ModuleObject {
    pub fn new(path: PathBuf, ast: ast::Module, mref: ModuleRef) -> Self {
        let body = grapher::NewType(ast.clone()).into();

        Self {
            path,
            body,
            ast,
            mref,
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

#[derive(Debug)]
pub enum Value {}
