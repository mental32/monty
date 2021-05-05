use std::rc::Rc;

use cranelift_module::Linkage;
use montyc::{context::GlobalContext, func::Function, prelude::*, CompilerOptions};

use structopt::StructOpt;

fn main() {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let mut global_context = GlobalContext::from(opts);

    global_context.load_module(file.unwrap(), |ctx, mref| {
        let mctx = ctx.modules.get(&mref).unwrap();

        ctx.database.insert_module(mctx);

        for (obj, lctx) in ctx.walk(mref.clone()) {
            obj.typecheck(&lctx).unwrap_or_compiler_error(&lctx);
        }

        {
            let funcs = ctx.functions.borrow();
            let mut ctx = montyc::context::codegen::CodegenBackend::new(ctx, None);

            ctx.declare_functions(funcs.iter().enumerate().map(|(idx, func)| {
                (
                    idx,
                    func.as_ref(),
                    func.scope.module_ref(),
                    Linkage::Export,
                    cranelift_codegen::isa::CallConv::SystemV,
                )
            }));

            ctx.finish(None::<&str>);
        }
    });
}
