use montyc::{
    ast::{funcdef::FunctionDef, module::Module, stmt::Statement},
    context::GlobalContext,
    func::Function,
    scope::downcast_ref,
};

fn main() {
    let mut ctx = GlobalContext::default();

    let module: Module = ctx.parse("def f(): return");

    let module_ref = ctx.register_module(module, "this.py".into());
    let module_context = ctx.modules.get(&module_ref).unwrap();

    for thing in module_context.scope.iter() {
        let o = thing.object.unspanned();
        let object = o.as_ref();
        let local_context = module_context.make_local_context(&ctx);

        thing.typecheck(local_context);

        if let Some(fndef) = downcast_ref::<FunctionDef>(object) {
            eprintln!("FunctionDef! {:?}", fndef)
        } else if let Some(fnc) = downcast_ref::<Function>(object) {
            eprintln!("Function! {:?}", fnc);
        } else if let Some(Statement::FnDef(fnc)) = downcast_ref(object) {
            eprintln!("Fn in a statement! {:?}", fnc)
        }
    }
}
