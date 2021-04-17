use std::io;

use montyc::{context::GlobalContext, CompilerOptions};

use structopt::StructOpt;

fn main() -> io::Result<()> {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let mut global_context = GlobalContext::from(opts);

    global_context.preload_module(file.unwrap(), |ctx, mref| {
        for (obj, ctx) in ctx.walk(mref.clone()) {
            obj.typecheck(&ctx);
        }
    });

    Ok(())
}
