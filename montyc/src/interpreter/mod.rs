#![allow(warnings)]

use std::{num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{
    ast::{class::ClassDef, expr::Expr, funcdef::FunctionDef, module::Module, stmt::Statement},
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

    impl std::fmt::Debug for Callable {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Callable").finish()
        }
    }
}

trait AstBody<Node> {
    fn add(&mut self, node: Node);
}

type Stmt = Rc<Spanned<Statement>>;

impl AstBody<Stmt> for Module {
    fn add(&mut self, node: Stmt) {
        self.body.push(node);
    }
}

impl AstBody<Stmt> for ClassDef {
    fn add(&mut self, node: Stmt) {
        self.body.push(node);
    }
}

impl AstBody<Stmt> for FunctionDef {
    fn add(&mut self, node: Stmt) {
        self.body.push(node);
    }
}

trait Eval: AstObject {
    fn eval<A>(&self, rt: &mut RuntimeContext, body: &mut A) -> PyResult<Option<PyObject>>
    where
        A: AstBody<Stmt>;
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
    Exception {
        exc: PyObject,
        traceback: Vec<(ModuleRef, Span)>,
    },
    Return(PyObject),
    Break,
}

impl PyErr {
    pub fn is_stop_iter(&self, rt: &RuntimeContext) -> bool {
        matches!(self, Self::Exception { exc, .. } if exc.is_instance(rt.singletons.stop_iter_exc_class.clone()))
    }
}

type Name = (NonZeroUsize, NonZeroUsize);

type PyObject = Rc<Object>;
type PyResult<T> = Result<T, PyErr>;
type PyAny = Rc<dyn any::AnyDebug>;

mod any {
    use std::any::{Any, TypeId};

    pub(super) trait AnyDebug: std::any::Any + std::fmt::Debug {}

    impl<T> AnyDebug for T where T: std::any::Any + std::fmt::Debug {}

    impl dyn AnyDebug {
        /// Returns `true` if the boxed type is the same as `T`.
        #[inline]
        pub fn is<T: Any>(&self) -> bool {
            // Get `TypeId` of the type this function is instantiated with.
            let t = TypeId::of::<T>();

            // Get `TypeId` of the type in the trait object (`self`).
            let concrete = self.type_id();

            // Compare both `TypeId`s on equality.
            t == concrete
        }

        /// Returns some reference to the boxed value if it is of type `T`, or
        /// `None` if it isn't.
        #[inline]
        pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
            if self.is::<T>() {
                // SAFETY: just checked whether we are pointing to the correct type, and we can rely on
                // that check for memory safety because we have implemented Any for all types; no other
                // impls can exist as they would conflict with our impl.
                unsafe { Some(&*(self as *const dyn AnyDebug as *const T)) }
            } else {
                None
            }
        }
    }
}

#[macro_export]
macro_rules! exception {
    ($message:expr) => {{
        let exc = Rc::new(Object {
            type_id: crate::prelude::TypeMap::EXCEPTION,
            members: dashmap::DashMap::default(),
            prototype: None,
        });

        panic!($message);

        let err = crate::interpreter::PyErr::Exception {
            exc,
            traceback: vec![],
        };

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
        if let Err(exc) = stmt.eval(&mut rt, &mut module) {
            panic!("{:?}", exc);
        }
    }

    Rc::new(module)
}
