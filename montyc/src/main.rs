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

        for object in ctx
            .database
            .query()
            .module(mref.clone())
            .filter(|object| object.as_function().is_some())
            .finish()
            .map(|obj| obj.unspanned())
        {
            let lctx = LocalContext {
                global_context: ctx,
                module_ref: mref.clone(),
                scope: mctx.scope(),
                this: Some(Rc::clone(&object)),
            };

            let func = Function::new(object, &lctx).unwrap_or_compiler_error(&lctx);

            ctx.functions.borrow_mut().push((func, mref.clone()));
        }

        {
            let funcs = ctx.functions.borrow();
            let mut ctx = montyc::context::codegen::CodegenBackend::new(ctx, None);

            ctx.declare_functions(funcs.iter().enumerate().map(|(idx, (f, mref))| {
                (
                    idx,
                    f.as_ref(),
                    mref,
                    Linkage::Export,
                    cranelift_codegen::isa::CallConv::SystemV,
                )
            }));

            ctx.finish(None::<&str>);
        }
    });
}
