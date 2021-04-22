use montyc::{
    ast::stmt::Statement,
    context::GlobalContext,
    prelude::*,
    CompilerOptions,
};

use structopt::StructOpt;

fn main() {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let mut global_context = GlobalContext::from(opts);

    global_context.preload_module(file.unwrap(), |ctx, mref| {
        for (obj, lctx) in ctx.walk(mref.clone()) {
            obj.typecheck(&lctx).unwrap_or_compiler_error(&lctx);

            if let Some(stmt) = obj.as_ref().downcast_ref::<Statement>() {
                if let Statement::Expression(e) = stmt {
                    let mut cfg = e.lower();
                    dbg!(&cfg);
                    cfg.reduce_forwarding_edges();
                    dbg!(cfg);
                }
            }
        }
    });
}
