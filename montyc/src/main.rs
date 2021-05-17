use cranelift_module::{DataContext, Linkage};
use montyc::{
    ast::{
        atom::{Atom, StringRef},
        expr::Expr,
        primary::Primary,
    },
    context::GlobalContext,
    prelude::*,
    CompilerOptions,
};

use structopt::StructOpt;

fn main() {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let isa = opts.codegen_settings();

    let opts = opts.verify();

    let mut global_context = GlobalContext::initialize(&opts);

    global_context.load_module(file.clone(), move |ctx, mref| {
        let mctx = ctx.modules.get(&mref).unwrap();

        ctx.database.insert_module(mctx);

        for (obj, lctx) in ctx.walk(mref.clone()) {
            obj.typecheck(&lctx).unwrap_or_compiler_error(&lctx);
        }
    });

    let mut cctx = montyc::prelude::CodegenModule::new(&global_context, isa);

    for (mref, mctx) in global_context.modules.iter() {
        for (name, global) in mctx
            .globals
            .iter()
            .map(|refm| (refm.key().clone(), refm.value().clone()))
        {
            let symbol = format!(
                "{}.{}",
                mref.module_name(),
                global_context.resolver.resolve_ident(name).unwrap()
            );

            cctx.declare_global_in_module(
                (mref.clone(), name),
                &symbol,
                true,
                Linkage::Hidden,
                |cctx| {
                    let atom = if let Expr::Primary(Spanned {
                        inner: Primary::Atomic(atom),
                        ..
                    }) = &global.inner
                    {
                        atom
                    } else {
                        unimplemented!();
                    };

                    let mut dctx = DataContext::new();

                    let type_id = match &atom.inner {
                        Atom::None => TypeMap::NONE_TYPE,
                        Atom::Ellipsis => TypeMap::ELLIPSIS,
                        Atom::Int(i) => {
                            dctx.set_align(8);
                            dctx.define(Box::new(i.to_ne_bytes()));

                            TypeMap::INTEGER
                        }

                        Atom::Str(st) => {
                            let string = StringRef(*st)
                                .resolve_as_string(&cctx.global_context)
                                .unwrap();
                            let string = std::ffi::CString::new(string).unwrap();
                            let string = string.into_bytes_with_nul();
                            let string = string.into_boxed_slice();

                            dctx.set_align(16);
                            dctx.define(string);

                            TypeMap::STRING
                        }

                        Atom::Bool(b) => {
                            dctx.set_align(1);
                            dctx.define(Box::new(if *b { [1u8] } else { [0u8] }));

                            TypeMap::BOOL
                        }

                        Atom::Float(f) => {
                            dctx.set_align(8);
                            dctx.define(Box::new(f.to_ne_bytes()));

                            TypeMap::FLOAT
                        }

                        Atom::Tuple(_) => unimplemented!(),
                        Atom::Comment(_) => unreachable!(),
                        Atom::Name(_) => unimplemented!(),
                    };

                    (type_id, dctx)
                },
            );
        }
    }

    cctx.declare_functions(global_context.functions.borrow().iter().enumerate().map(
        |(idx, func)| {
            (
                idx,
                func.as_ref(),
                func.scope.module_ref(),
                if func.is_externaly_defined() {
                    Linkage::Import
                } else {
                    Linkage::Export
                },
                cranelift_codegen::isa::CallConv::SystemV,
            )
        },
    ));

    cctx.finish(file.file_stem().unwrap());
}
