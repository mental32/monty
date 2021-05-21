#![warn(warnings)]

use std::rc::Rc;

use dashmap::DashMap;

use crate::{
    ast::module::Module,
    interpreter::{runtime::RuntimeContext, scope::DynamicScope},
    prelude::*,
};

use self::object::Object;

pub mod eval;
pub mod object;
pub mod runtime;
pub mod scope;

trait Eval: AstObject {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> Option<Rc<Object>>;
}

pub fn exec_module(global_context: &GlobalContext, mref: ModuleRef) -> Rc<Module> {
    let mut module = Module { body: vec![] };

    let scope = Rc::new(DynamicScope {
        root: ScopeRoot::AstObject(global_context.modules.get(&mref).unwrap().module.clone()),
        mref: mref.clone(),
        namespace: DashMap::new(),
    });

    let mut rt = RuntimeContext::new(global_context);

    rt.stack_frames.push(scope);

    for stmt in global_context
        .modules
        .get(&mref)
        .unwrap()
        .module
        .body
        .iter()
    {
        stmt.eval(&mut rt, &mut module);
    }

    Rc::new(module)
}
