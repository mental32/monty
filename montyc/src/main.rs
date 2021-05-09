use cranelift_module::Linkage;
use montyc::{context::GlobalContext, prelude::*, CompilerOptions};

use structopt::StructOpt;

fn main() {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let mut global_context = GlobalContext::from(opts.clone());

    global_context.load_module(file.unwrap(), |ctx, mref| {
        let mctx = ctx.modules.get(&mref).unwrap();

        ctx.database.insert_module(mctx);

        for (obj, lctx) in ctx.walk(mref.clone()) {
            obj.typecheck(&lctx).unwrap_or_compiler_error(&lctx);
        }

        {
            let mut cctx = montyc::codegen::context::CodegenBackend::new(ctx, None);

            cctx.declare_functions(
                ctx.functions
                    .borrow()
                    .iter()
                    .enumerate()
                    .map(|(idx, func)| {
                        (
                            idx,
                            func.as_ref(),
                            func.scope.module_ref(),
                            Linkage::Export,
                            cranelift_codegen::isa::CallConv::SystemV,
                        )
                    }),
            );

            cctx.finish(
                opts.input
                    .clone()
                    .map(|p| p.file_stem().unwrap().into())
                    .unwrap_or(std::path::PathBuf::from("a.out")),
            );
        }
    });
}
