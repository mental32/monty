use self::context::{CodegenLowerArg, RValueAlloc};

pub mod context;
pub mod lower_ast;
pub mod structbuf;

mod tvalue;
mod pointer;
mod storage;
mod module;

type Value<'global> = Result<TypedValue, (std::alloc::Layout, Box<RValueAlloc<'global>>)>;

trait LowerCodegen {
    fn lower<'global>(&self, _: CodegenLowerArg<'global, '_, '_>) -> Option<Value<'global>>;
}

pub use tvalue::{TypedValue, TypePair};
pub use module::CodegenModule;
