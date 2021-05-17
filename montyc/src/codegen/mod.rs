use self::context::{CodegenLowerArg, RValueAlloc};

pub mod context;
pub mod lower_ast;
pub mod structbuf;

mod module;
pub mod pointer;
mod storage;
mod tvalue;

type Value<'global> = Result<TypedValue, (std::alloc::Layout, Box<RValueAlloc<'global>>)>;

trait LowerCodegen {
    fn lower<'global>(&self, _: CodegenLowerArg<'global, '_, '_>) -> Option<Value<'global>>;
}

pub use module::CodegenModule;
pub use tvalue::{TypePair, TypedValue};
