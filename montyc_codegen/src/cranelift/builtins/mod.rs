mod bool;
mod int;

use cranelift_object::ObjectModule;

use montyc_core::{MapT, TypeId};

use crate::backend::BackendImpl;

type BuiltinStub =
    fn(&BackendImpl, &mut ObjectModule) -> (cranelift_codegen::ir::Function, TypeId, &'static str);

macro_rules! decl_builtins {
    [ $( $p:path ),* ] => {
        const BUILTINS: &[self::BuiltinStub] = &[
            $( $p as self::BuiltinStub, )*
        ];
    };
}

decl_builtins![
    // int
    int::int_add,
    int::int_sub,
    int::int_bool,
    int::int_eq,
    int::int_ne,
    // bool
    bool::bool_bool
];

pub(crate) fn create_builtins(
    cx: &BackendImpl,
    object_module: &mut ObjectModule,
) -> MapT<(TypeId, &'static str), cranelift_codegen::ir::Function> {
    log::trace!("[builtins::install_builtins] Installing builtins and intrinsics...");

    let mut fids = MapT::new();

    for bltn in BUILTINS.iter() {
        let (fid, tid, name) = bltn(cx, object_module);

        fids.insert((tid, name), fid);
    }

    fids
}
