use std::rc::Rc;

use montyc::{
    ast::stmt::Statement, context::GlobalContext, func::Function, prelude::*, CompilerOptions,
};

use structopt::StructOpt;

fn main() {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let file = opts.input.clone();

    let mut global_context = GlobalContext::from(opts);

    global_context.preload_module(file.unwrap(), |ctx, mref| {
        for (obj, lctx) in ctx.walk(mref.clone()) {
            if let Some(Statement::FnDef(_)) = obj.as_ref().downcast_ref() {
                let func = Function::new(obj.clone(), &lctx);

                let lctx = LocalContext {
                    global_context: ctx,
                    module_ref: mref.clone(),
                    scope: Rc::new(func.scope.clone()) as Rc<_>,
                    this: lctx.this.clone(),
                    parent: lctx.parent.clone(),
                };

                func.typecheck(&lctx).unwrap_or_compiler_error(&lctx);

                ctx.functions.borrow_mut().push((func, mref.clone()));
            } else {
                obj.typecheck(&lctx).unwrap_or_compiler_error(&lctx);
            }
        }

        {
            let funcs = ctx.functions.borrow();
            let mut ctx = montyc::context::codegen::CodegenBackend::new(None);

            for (func, mref) in funcs.iter() {
                ctx.add_function_to_module(
                    func,
                    mref,
                    cranelift_module::Linkage::Export,
                    cranelift_codegen::isa::CallConv::SystemV,
                );
            }

            ctx.finish(None::<&str>);
        }
    });
}
