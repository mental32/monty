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
#![warn(missing_docs)]

mod grapher;
pub mod interpreter;
pub mod typing;
pub mod code;

pub use interpreter::{HostGlue, ObjAllocId};

use std::path::{Path, PathBuf};

use montyc_core::{ModuleRef, TypeId};
use montyc_parser::ast;

use crate::interpreter::PyDictRaw;

mod object_graph;

pub use object_graph::{ObjectGraph, ObjectGraphIndex};

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

/// A module object is a representation of a Python module.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ModuleObject {
    pub path: PathBuf,
    pub body: grapher::AstNodeGraph,
    pub ast: ast::Module,
    pub mref: ModuleRef,
}

impl ModuleObject {
    /// Create a new ModuleObject from a path and AST and module reference.
    #[inline]
    pub fn new(path: PathBuf, ast: ast::Module, mref: ModuleRef) -> Self {
        let body = grapher::NewType(ast.clone()).into();

        Self {
            path,
            body,
            ast,
            mref,
        }
    }

    /// The path to the module.
    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
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
        parent: Option<ObjectGraphIndex>,
    },

    Class {
        name: String,
        properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        // defsite: ObjectGraphIndex,
        // parent: ObjectGraphIndex,
    },
}

impl Value {
    /// Get an iterator over the properties of this value, i.e. the `__dict__` of the value.
    ///
    /// If `self` is a `Dict` then `iter()` will return an iterator over the hash, keys, and values of the dictionary.
    ///
    pub fn iter(&self) -> impl Iterator<Item = (&u64, &(ObjectGraphIndex, ObjectGraphIndex))> + '_ {
        match self {
            Value::Dict {
                data: properties, ..
            }
            | Value::Module { properties, .. }
            | Value::Function { properties, .. }
            | Value::Class { properties, .. } => properties.iter(),

            Value::Object(obj) => obj.properties.iter(),

            Value::String(_) => todo!(),
            Value::Integer(_) => todo!(),
        }
    }
}
