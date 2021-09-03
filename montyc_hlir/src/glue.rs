//! A "glue" trait to be used by users of this crate for the interpreter.

use std::path::Path;
use std::rc::Rc;
use std::{cell::RefCell, fmt};

use montyc_core::{ModuleRef, SpanRef};

use crate::value_store::{GlobalValueStore, ValueGraphIx};
use crate::{typing::TypingContext, ModuleObject};

/// A trait to be implemented by the owner of a runtime.
pub trait HostGlue {
    /// The debug formatter hook.
    fn debug_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HostGlue").finish()
    }

    /// convert a string to a span ref.
    fn str_to_spanref(&self, string: &str) -> SpanRef;

    /// convert a span ref to a string.
    fn spanref_to_str(&self, sref: SpanRef) -> &str;

    /// Trigger the importing mechansim to import the given module.
    ///
    /// Returns a vec of all modules that were imported and their names.
    ///
    /// The order of the items in the vec correlates to the provided input path.
    /// so `a.b.c` will produce vec![(mref(a), a), (mref(b), b), (mref(c), c)]
    ///
    fn import_module(
        &mut self,
        path: &[SpanRef],
        base: Option<(usize, &Path)>,
    ) -> Vec<(ModuleRef, SpanRef)>;

    /// Try and get the associated module object to the supplied `mref`
    fn get_module(&self, mref: ModuleRef) -> Option<ModuleObject>;

    /// get a refcell to the typing context.
    fn tcx(&self) -> &RefCell<TypingContext>;

    /// get a reference to the global value store.
    fn value_store(&self) -> Rc<RefCell<GlobalValueStore>>;

    /// get the fully qualified name for this function.
    fn get_qualname(&self, func_ix: ValueGraphIx) -> Vec<String>;
}

impl<'a> fmt::Debug for &'a mut dyn HostGlue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug_fmt(f)
    }
}
