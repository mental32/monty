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
#![deny(warnings)]

mod grapher;

pub mod code;
pub mod flatcode;
pub mod interpreter;
pub mod module_object;
pub mod typing;

use montyc_core::{ModuleRef, TypeId};

use crate::interpreter::PyDictRaw;

pub use interpreter::runtime::object_graph::{ObjectGraph, ObjectGraphIndex};
pub use interpreter::{HostGlue, ObjAllocId};
pub use module_object::*;

/// HLIR objects are dynamic/reflective representations of objects that we can typecheck and compile.
///
/// They represent an object during compilation and keep track of
/// properties such as the object's type or attributes.
///
#[derive(Debug, Clone)]
pub struct Object {
    type_id: TypeId,

    properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
}

#[derive(Debug, Clone)]
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
        object: ObjectGraphIndex,
        data: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
    },

    Function {
        name: String,
        properties: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        annotations: PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        // defsite: Option<UniqueNodeIndex>,
        // parent: Option<ObjectGraphIndex>,
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
        self.properties().iter()
    }

    /// Get the properties dict of the value.
    pub fn properties(&self) -> &PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)> {
        match self {
            Value::Dict {
                data: properties, ..
            }
            | Value::Module { properties, .. }
            | Value::Function { properties, .. }
            | Value::Class { properties, .. } => properties,

            Value::Object(obj) => &obj.properties,

            Value::String(_) => todo!(),
            Value::Integer(_) => todo!(),
        }
    }
}
