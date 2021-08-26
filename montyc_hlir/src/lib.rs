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

mod flatcode;
mod func;
pub mod interpreter;
pub mod module_object;
pub mod typing;
pub mod value_store;

use montyc_core::{ModuleRef, SpanRef, TypeId};
use value_store::ValueGraphIx;

use crate::interpreter::PyDictRaw;

pub use flatcode::{
    raw_inst::{Const, Dunder, RawInst},
    FlatCode, FlatInst, FlatSeq,
};

pub use func::Function;

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

    properties: PyDictRaw<(ValueGraphIx, ValueGraphIx)>,
}

pub(crate) type CallableSignature<T> = Option<(Option<SpanRef>, Box<[(SpanRef, Option<T>)]>)>;

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Value {
    Object(self::Object),

    Module {
        mref: ModuleRef,
        properties: PyDictRaw<(ValueGraphIx, ValueGraphIx)>,
    },

    String(String),
    Integer(i64),

    Dict {
        object: ValueGraphIx,
        data: PyDictRaw<(ValueGraphIx, ValueGraphIx)>,
    },

    Function {
        name: String,

        ret_t: ValueGraphIx,
        args_t: CallableSignature<ValueGraphIx>,

        source: Option<(ModuleRef, usize)>,

        properties: PyDictRaw<(ValueGraphIx, ValueGraphIx)>,
        annotations: PyDictRaw<(ValueGraphIx, ValueGraphIx)>,
    },

    Class {
        name: String,
        properties: PyDictRaw<(ValueGraphIx, ValueGraphIx)>,
    },
}

impl Value {
    /// Get an iterator over the properties of this value, i.e. the `__dict__` of the value.
    ///
    /// If `self` is a `Dict` then `iter()` will return an iterator over the hash, keys, and values of the dictionary.
    ///
    pub fn iter(&self) -> impl Iterator<Item = (&u64, &(ValueGraphIx, ValueGraphIx))> + '_ {
        self.properties().iter()
    }

    /// Get the properties dict of the value.
    pub fn properties(&self) -> &PyDictRaw<(ValueGraphIx, ValueGraphIx)> {
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
