use montyc::{CompilerOptions, ast::stmt::Statement, context::GlobalContext, func::Function, prelude::*};

use structopt::StructOpt;

fn main() {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let mut global_context = GlobalContext::from(opts);

    global_context.preload_module(file.unwrap(), |ctx, mref| {
        for (obj, lctx) in ctx.walk(mref.clone()) {
            obj.typecheck(&lctx).unwrap_or_compiler_error(&lctx);

            if let Some(Statement::FnDef(_)) = obj.as_ref().downcast_ref() {
                ctx.functions.borrow_mut().push(Function::new(obj.clone(), &lctx));
            }
        }
    });
}
