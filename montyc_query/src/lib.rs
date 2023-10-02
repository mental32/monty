//! Query trait implementation.
#![deny(warnings)]

use std::{alloc::Layout, fmt};

use montyc_core::span::SpanRef;
use montyc_core::value::{TaggedValueId, ValueId};
use montyc_core::{Function, ModuleData, ModuleRef, MontyError, TypeId, TypingContext, FUNCTION};
use montyc_flatcode::{FlatCode, FlatSeq};
use montyc_parser::ast::Constant;

pub(crate) type MontyResult<T> = Result<T, MontyError>;

/// A query system trait making incremental and lazy compilation easier.
///
/// This is usually implemented for the `GlobalContext` type but it's
/// designed to be used mainly through a `&dyn Queries` trait object reference
///
/// All query methods take `self` by an immutable reference, it is left up
/// to the implementing type to handle its own internal mutability.
///
pub trait Queries {
    /// Get the associated typing context with this query provider.
    fn tcx<'a>(&'a self) -> &'a dyn TypingContext;

    /// Given some `ValueId` compute its type and return the corresponding `TypeId`.
    fn get_type_of(&self, val: ValueId) -> MontyResult<TypeId>;

    /// Given a `ValueId` with a non-trivial memory layout (such that TypingContext on its own can not compute it.) calculate its layout.
    fn get_layout_of(&self, val: ValueId) -> MontyResult<Layout>;

    /// Given a `ModuleRef` try and get its associated module data.
    fn get_module_data(&self, mref: ModuleRef) -> MontyResult<ModuleData>;

    /// Get specified modules flatcode.
    fn get_module_flatcode(&self, mref: ModuleRef) -> MontyResult<FlatCode>;

    /// Get the flatcode sequence for function.
    fn get_function_flatcode(&self, fid: TaggedValueId<FUNCTION>) -> MontyResult<FlatSeq>;

    /// Get a functions control flow graph for codegen.
    fn get_function_cg_cfg(
        &self,
        fid: TaggedValueId<FUNCTION>,
    ) -> MontyResult<montyc_core::codegen::CgBlockCFG<Constant>>;

    /// Get the associated function for this value.
    fn get_function(&self, value_id: ValueId) -> MontyResult<Function>;

    /// Given a `SpanRef` try and resolve it to its corresponding string slice.
    fn spanref_to_str(&self, sref: SpanRef) -> MontyResult<String>;

    /// Create a new `SpanRef` from a given string slice.
    fn str_to_spanref(&self, st: &str) -> SpanRef;

    /// For a given path to some function, return a vec of all functions found in the call graph.
    fn call_graph_of(
        &self,
        entry_path: &str,
    ) -> MontyResult<(Vec<TaggedValueId<{ FUNCTION }>>, ValueId)>;
}

impl<'a> fmt::Debug for &'a mut dyn Queries {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("&mut Queries").finish()
    }
}

impl<'a> fmt::Debug for &'a dyn Queries {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("&Queries").finish()
    }
}
