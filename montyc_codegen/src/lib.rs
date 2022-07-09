// #![deny(warnings)]

use std::path::PathBuf;

use montyc_core::{opts::CompilerOptions, Function, MontyResult};
use montyc_query::Queries;

#[cfg(feature = "cranelift")]
pub mod cranelift;

#[cfg(feature = "llvm")]
pub mod llvm;

pub(crate) mod tvalue;

pub struct CgBackend<B>(pub(crate) B);

impl CgBackend<cranelift::BackendImpl> {
    /// Construct a new codegen backend from provided options.
    pub fn new(opts: &CompilerOptions) -> Self {
        let backend = cranelift::BackendImpl::new(opts);

        Self(backend)
    }

    /// Include `func` into the current codegen session.
    #[inline]
    pub fn include_function(&mut self, queries: &dyn Queries, func: Function) -> MontyResult<()> {
        self.0.include_function(queries, func.value_id)
    }

    /// Lower all included functions and produce an executable with `entry_ix` being the value id of the "main" function.
    #[inline]
    pub fn finish(self, queries: &dyn Queries) -> MontyResult<PathBuf> {
        self.0.finish(queries)
    }
}

#[inline]
pub fn compile(opts: &CompilerOptions, queries: &dyn Queries) -> MontyResult<PathBuf> {
    CgBackend::new(&opts).finish(queries)
}
