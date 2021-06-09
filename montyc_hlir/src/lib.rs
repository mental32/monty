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

use std::path::PathBuf;

use montyc_core::TypeId;
use montyc_parser::{AstVisitor, AstObject};

use petgraph::graph::DiGraph;

/// HLIR objects can be roughly thought of as the analogue to `PyObject`
///
/// They represent an object dynamically during compilation and keep track of
/// properties such as the object's type or attributes.
///
#[derive(Debug)]
pub struct Object {
    type_id: TypeId,
}


type AstNodeGraph = DiGraph<montyc_parser::ast::AstNode, ()>;

#[derive(Debug)]
pub struct ModuleObject {
    pub path: PathBuf,
    pub ast: montyc_parser::ast::Module,
    pub body: AstNodeGraph,
}

struct HLIRVisitor(AstNodeGraph);

impl AstVisitor for HLIRVisitor {

}

pub fn to_hlir_object_graph(module: &montyc_parser::ast::Module) -> AstNodeGraph {
    let mut visitor = HLIRVisitor(AstNodeGraph::new());

    for stmt in module.body.iter() {
        stmt.visit_with(&mut visitor);
    }

    todo!();
}

#[derive(Debug)]
pub enum Value {}
