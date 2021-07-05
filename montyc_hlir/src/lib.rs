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

use std::{
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
};

use montyc_core::{ModuleRef, TypeId};
use montyc_parser::ast;
use petgraph::graph::NodeIndex;

use crate::interpreter::PyDictRaw;

#[derive(Debug, Default)]
pub struct ObjectGraph {
    graph: petgraph::graph::DiGraph<Value, ()>,
    strings: ahash::AHashMap<u64, ObjectGraphIndex>,
}

impl ObjectGraph {
    fn add_string_node(&mut self, hash: u64, string: crate::Value) -> ObjectGraphIndex {
        if let Some(idx) = self.strings.get(&hash) {
            return idx.clone();
        }

        let idx = self.graph.add_node(string);

        self.strings.insert(hash, idx);

        idx
    }
}

impl DerefMut for ObjectGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl Deref for ObjectGraph {
    type Target = petgraph::graph::DiGraph<Value, ()>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

pub type ObjectGraphIndex = NodeIndex<u32>;

/// HLIR objects are dynamic/reflective representations of objects that we can typecheck and compile.
///
/// They represent an object during compilation and keep track of
/// properties such as the object's type or attributes.
///
#[derive(Debug)]
pub struct Object {
    type_id: TypeId,

    properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
}

#[derive(Debug, Clone)]
pub struct ModuleObject {
    pub path: PathBuf,
    pub body: grapher::AstNodeGraph,
    pub ast: ast::Module,
    pub mref: ModuleRef,
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
pub enum Value {
    Object(self::Object),

    Module {
        mref: ModuleRef,
        properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
    },

    String(String),
    Integer(i64),

    Dict {
        object: self::Object,
        data: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
    },

    Function {
        name: String,
        properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        annotations: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        defsite: Option<ObjectGraphIndex>,
    },

    Class {
        name: String,
        properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        // def: ObjectGraphIndex,
    },
}
