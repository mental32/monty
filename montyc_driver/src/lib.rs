#![warn(warnings)]

pub(crate) mod import;
pub(crate) mod pretty_printer;
pub(crate) mod typeck;
pub(crate) mod value_store;

pub mod prelude {
    use super::*;

    pub use global_context::SessionContext;
    pub use montyc_core::opts::{CompilerOptions, VerifiedCompilerOptions};
}

mod global_context;
