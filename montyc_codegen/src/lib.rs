use cranelift_frontend::FunctionBuilder;

pub(crate) mod module;
pub(crate) mod pointer;
pub(crate) mod structbuf;
pub(crate) mod tvalue;

pub(crate) type CxArg<'a> = ((), &'a mut FunctionBuilder<'a>);

pub mod prelude {
    pub use crate::module::CodegenModule;
}
