#![warn(warnings)]

use std::{num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{
    ast::{expr::Expr, module::Module, stmt::Statement},
    interpreter::{runtime::RuntimeContext, scope::DynamicScope},
    prelude::*,
};

use self::object::Object;

mod eval;
mod object;
mod runtime;
mod scope;

mod callable {
    use super::{runtime::RuntimeContext, PyObject, PyResult};

    pub(super) enum Callable {
        Object {
            this: PyObject,
            args: Option<PyObject>,
            kwargs: Option<PyObject>,
        },

        BuiltinFn(
            for<'a> fn(PyObject, &'a mut RuntimeContext, Option<PyObject>) -> PyResult<PyObject>,
        ),
    }
}

trait Eval: AstObject {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>>;
}

trait ToAst
where
    Self: Sized,
{
    fn to_expr(self, _rt: &mut RuntimeContext, _span: Span) -> Expr {
        unimplemented!()
    }

    fn to_statement(self, _rt: &mut RuntimeContext) -> Statement {
        unimplemented!()
    }

    fn to_ast_object(self, _rt: &mut RuntimeContext) -> Rc<dyn AstObject> {
        unimplemented!()
    }
}

#[derive(Debug)]
enum PyErr {
    Exception(PyObject),
    Return(PyObject),
}

impl PyErr {
    pub fn is_stop_iter(&self, rt: &RuntimeContext) -> bool {
        matches!(self, Self::Exception(exc) if exc.is_instance(rt.singletons.stop_iter_exc_class.clone()))
    }
}

type Name = (NonZeroUsize, NonZeroUsize);

type PyObject = Rc<Object>;
type PyResult<T> = Result<T, PyErr>;

#[macro_export]
macro_rules! exception {
    ($st:literal) => {{
        let exc = Rc::new(Object {
            type_id: crate::prelude::TypeMap::EXCEPTION,
            members: dashmap::DashMap::default(),
            prototype: None,
        });

        let err = crate::interpreter::PyErr::Exception(exc);

        return Err(err);
    }};
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
        if let Err(_) = stmt.eval(&mut rt, &mut module) {
            todo!();
        }
    }

    Rc::new(module)
}
