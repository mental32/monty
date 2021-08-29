use std::fmt;
use std::path::Path;

use crate::{span::SpanRef, module::ModuleRef};

/// A trait to be implemented by the owner of a runtime.
pub trait HostGlue {
    fn debug_fmt(&self, f: fmt::Formatter<'_>) -> fmt::Result {
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
}

impl fmt::Debug for dyn HostGlue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug_fmt(f)
    }
}
