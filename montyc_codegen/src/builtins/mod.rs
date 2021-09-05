mod int;
mod bool;

use ahash::AHashMap;
use cranelift_codegen::ir::Function;
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;
use montyc_core::TypeId;

use crate::module::CodegenModule;

type BuiltinStub = fn(&mut CodegenModule, &mut ObjectModule) -> (Function, TypeId, &'static str);

const BUILTINS: &[BuiltinStub] = &[
    int::int_add as BuiltinStub,
    int::int_sub as BuiltinStub,
    int::int_bool as BuiltinStub,
    int::int_eq as BuiltinStub,
    int::int_ne as BuiltinStub,

    bool::bool_bool as BuiltinStub,
] as &[_];

pub fn install_builtins(cx: &mut CodegenModule, object_module: &mut ObjectModule) -> AHashMap<(TypeId, &'static str), Function> {
    log::trace!("[builtins::install_builtins] Installing builtins and intrinsics...");

    let mut fids = AHashMap::new();

    for bltn in BUILTINS.iter() {
        let (fid, tid, name) = bltn(cx, object_module);

        fids.insert((tid, name), fid);
    }

    fids
}
