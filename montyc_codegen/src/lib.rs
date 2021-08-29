pub(crate) mod module;
pub(crate) mod structbuf;
pub(crate) mod tvalue;
pub(crate) mod pointer;

pub mod prelude {
    pub use crate::{
        module::{GlobalDataRef, CodegenModule},
    }
}
