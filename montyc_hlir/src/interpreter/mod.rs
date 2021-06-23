mod exception;
mod object;
mod runtime;

pub(in self) type HashKeyT = u64;

pub type PyResult<T> = Result<T, exception::PyException>;

use montyc_core::{ModuleRef, SpanRef};
use montyc_parser::ast::ImportDecl;

pub use {runtime::Runtime, object::PyDictRaw};

use object::alloc::ObjAllocId;

use crate::{ModuleObject, typing::TypingContext};

pub trait HostGlue {
    fn name_to_spanref(&self, name: &str) -> SpanRef;

    fn spanref_to_str(&self, sref: SpanRef) -> &str;

    fn import_module(&self, decl: ImportDecl) -> Vec<(ModuleRef, SpanRef)>;

    fn tcx(&self) -> &TypingContext;

    fn with_module(
        &self,
        mref: ModuleRef,
        f: &mut dyn FnMut(&ModuleObject) -> PyResult<()>,
    ) -> PyResult<()>;

    fn with_module_mut(
        &self,
        mref: ModuleRef,
        f: &mut dyn FnMut(&mut ModuleObject) -> PyResult<()>,
    ) -> PyResult<()>;
}
