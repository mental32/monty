#![allow(warnings)]

use crate::typing::LocalTypeId;

pub mod context;
pub mod structbuf;
pub mod lower_ast;

pub type TypePair = (LocalTypeId, cranelift_codegen::ir::types::Type);
