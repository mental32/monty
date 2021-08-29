//! An AST-based interpreter runtime for const evaluation.
#![allow(warnings)]

mod exception;
pub(crate) mod object;
pub mod runtime;

pub(self) type HashKeyT = u64;

/// The interpreters Result type.
pub type PyResult<T> = Result<T, exception::PyException>;

use std::{fmt::Debug, path::Path};

use montyc_core::{ModuleRef, SpanRef};
use montyc_parser::ast::ImportDecl;

pub use {object::PyDictRaw, runtime::Runtime};

pub use object::alloc::ObjAllocId;

use crate::{ModuleData, ModuleObject};
