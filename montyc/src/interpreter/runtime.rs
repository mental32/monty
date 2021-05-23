use std::{convert::TryInto, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{
    ast::{module::Module, stmt::Statement, Spanned},
    context::{GlobalContext, ModuleRef},
    exception,
    interpreter::{callable::Callable, Eval},
    scope::ScopeRoot,
    typing::TypeMap,
};

use super::{
    object::{MutCell, Object},
    scope::DynamicScope,
    Name, PyAny, PyErr, PyObject,
};

macro_rules! binop {
    ($rt:expr, def $class_name:ident.$name:ident -> $t:path = $e:expr ) => {{
        let rt: &RuntimeContext = $rt;
        let name = rt
            .global_context
            .magical_name_of(stringify!($name))
            .expect("Missing a magic name.");

        let callback = Callable::BuiltinFn(|this, rt, args| {
            let argument = args.expect("dunder should have one argument!");

            log::trace!(
                "interpreter:eval {}.{}(self={:?}, arg={:?})",
                stringify!($class_name),
                stringify!($name),
                format!("<{} @ {:?}>", crate::fmt::Formattable { inner: this.type_id, gctx: &rt.global_context }, Rc::as_ptr(&this)),
                format!("<{} @ {:?}>", crate::fmt::Formattable { inner: argument.type_id, gctx: &rt.global_context }, Rc::as_ptr(&argument)),
            );

            let __value = this
                .get_member(&rt.names.__value)
                .expect("object should have a `__value` property.")
                .into_inner();

            let __value = __value
                .downcast_ref::<$t>()
                .expect("object `__value` should be an isize.");

            let f: fn($t, $t) -> $t = $e;

            if argument.type_id == this.type_id {
                let argument_value = argument
                    .get_member(&rt.names.__value)
                    .expect("object should have a `__value` property.")
                    .into_inner();

                let argument_value = argument_value
                    .downcast_ref::<$t>()
                    .expect("object `__value` should be an isize.");

                let ret = f(*__value, *argument_value);

                Ok(rt.integer(ret))
            } else {
                todo!("type error");
            }

        });

        (name, MutCell::Immutable(Rc::new(callback) as PyAny))
    }};
}

#[derive(Debug)]
pub struct Singletons {
    pub(super) int_class: PyObject,
    pub(super) str_class: PyObject,
    pub(super) bool_class: PyObject,
    pub(super) func_class: PyObject,
    pub(super) type_class: PyObject,
    pub(super) base_exc_class: PyObject,
    pub(super) ret_exc_class: PyObject,
    pub(super) stop_iter_exc_class: PyObject,
    pub(super) none: PyObject,
}

impl Singletons {
    pub fn new() -> Self {
        let type_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: None,
        });

        let int_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(type_class.clone()),
        });

        let str_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(type_class.clone()),
        });

        let bool_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(type_class.clone()),
        });

        let func_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(type_class.clone()),
        });

        let base_exc_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(type_class.clone()),
        });

        let ret_exc_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(base_exc_class.clone()),
        });

        let stop_iter_exc_class = Rc::new(Object {
            type_id: TypeMap::TYPE,
            members: DashMap::new(),
            prototype: Some(base_exc_class.clone()),
        });

        Self {
            ret_exc_class,
            base_exc_class,
            stop_iter_exc_class,
            type_class,
            int_class,
            str_class,
            bool_class,
            func_class,
            none: Rc::new(Object {
                type_id: TypeMap::NONE_TYPE,
                members: DashMap::new(),
                prototype: None,
            }),
        }
    }
}

pub(super) struct SpecialNames {
    pub __call__: Name,
    pub __iter__: Name,
    pub __next__: Name,
    pub __neg__: Name,
    pub __pos__: Name,
    pub __invert__: Name,
    pub __value: Name,
    pub __getitem__: Name,
}

pub(super) struct RuntimeContext<'a> {
    pub(super) global_context: &'a GlobalContext,
    pub(super) singletons: Singletons,
    pub(super) names: SpecialNames,
    pub(super) stack_frames: Vec<Rc<DynamicScope>>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(global_context: &'a GlobalContext) -> Self {
        Self {
            global_context,
            singletons: Singletons::new(),
            names: SpecialNames {
                __value: global_context.magical_name_of("__value").unwrap(),
                __call__: global_context.magical_name_of("__call__").unwrap(),
                __iter__: global_context.magical_name_of("__iter__").unwrap(),
                __next__: global_context.magical_name_of("__next__").unwrap(),
                __getitem__: global_context.magical_name_of("__getitem__").unwrap(),
                __neg__: global_context.magical_name_of("__neg__").unwrap(),
                __pos__: global_context.magical_name_of("__pos__").unwrap(),
                __invert__: global_context.magical_name_of("__invert__").unwrap(),
            },

            stack_frames: vec![],
        }
    }

    pub fn return_exc(&self, value: PyObject) -> PyErr {
        PyErr::Return(value)
    }

    pub fn lookup(&self, name: Name) -> Option<PyObject> {
        self.stack_frames.iter().rev().find_map(|scope| {
            scope
                .namespace
                .iter()
                .find(|refm| *refm.key() == name)
                .map(|refm| refm.value().clone())
        })
    }

    pub fn scope(&self) -> Rc<DynamicScope> {
        self.stack_frames.last().unwrap().clone()
    }

    pub fn is_truthy(&self, v: PyObject) -> bool {
        match v.type_id {
            TypeMap::INTEGER => v
                .get_member(&self.names.__value)
                .unwrap()
                .into_inner()
                .downcast_ref::<Rc<isize>>()
                .map(|c| *c.as_ref() == 1_isize)
                .unwrap_or(false),

            TypeMap::BOOL => v
                .get_member(&self.names.__value)
                .unwrap()
                .into_inner()
                .downcast_ref::<bool>()
                .map(|r| *r == true)
                .unwrap_or(false),

            _ => unimplemented!(),
        }
    }

    pub fn integer(&self, value: isize) -> PyObject {
        Rc::new(Object {
            type_id: TypeMap::INTEGER,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.names.__value.clone(),
                    MutCell::Immutable(Rc::new(value) as PyAny),
                );

                let (attr, method) =
                    binop!(self, def int.__add__ -> isize = |lhs, rhs| lhs.saturating_add(rhs));
                members.insert(attr, method);

                let (attr, method) =
                    binop!(self, def int.__sub__ -> isize = |lhs, rhs| lhs.saturating_sub(rhs));
                members.insert(attr, method);

                let (attr, method) =
                    binop!(self, def int.__mul__ -> isize = |lhs, rhs| lhs.saturating_mul(rhs));

                members.insert(attr, method);

                let (attr, method) = binop!(self, def int.__pow__ -> isize = |lhs, rhs| lhs.saturating_pow(rhs.try_into().unwrap()));
                members.insert(attr, method);

                members
            },

            prototype: Some(self.singletons.int_class.clone()),
        })
    }

    pub fn float(&self, value: f64) -> PyObject {
        Rc::new(Object {
            type_id: TypeMap::FLOAT,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.names.__value.clone(),
                    MutCell::Immutable(Rc::new(value) as PyAny),
                );

                members
            },

            prototype: Some(self.singletons.int_class.clone()),
        })
    }

    pub fn boolean(&self, b: bool) -> PyObject {
        Rc::new(Object {
            type_id: TypeMap::BOOL,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.names.__value.clone(),
                    MutCell::Immutable(Rc::new(b) as PyAny),
                );

                members
            },

            prototype: Some(self.singletons.bool_class.clone()),
        })
    }

    pub fn string(&self, st: NonZeroUsize, _mref: ModuleRef) -> PyObject {
        Rc::new(Object {
            type_id: TypeMap::STRING,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.names.__value.clone(),
                    MutCell::Immutable(Rc::new(st) as PyAny),
                );

                members
            },

            prototype: Some(self.singletons.str_class.clone()),
        })
    }

    pub fn tuple(&self, elements: &[PyObject]) -> PyObject {
        Rc::new(Object {
            type_id: self
                .global_context
                .type_map
                .tuple(elements.iter().map(|o| o.type_id.clone())),
            members: {
                let members = DashMap::new();

                members.insert(
                    self.names.__value.clone(),
                    MutCell::Immutable(Rc::new(elements.to_vec()) as PyAny),
                );

                members
            },

            prototype: Some(self.singletons.str_class.clone()),
        })
    }

    pub fn none(&self) -> PyObject {
        self.singletons.none.clone()
    }

    pub fn function(&self, def: Rc<Spanned<Statement>>, mref: ModuleRef) -> PyObject {
        assert_matches!(&def.inner, Statement::FnDef(_));

        Rc::new(Object {
            type_id: TypeMap::DYN_FUNC,
            members: {
                let members = DashMap::new();

                let call = Callable::BuiltinFn(|obj, rt, args| {
                    let inner = obj.get_member(&rt.names.__value).unwrap().into_inner();
                    let (stmt, mref) = inner
                        .downcast_ref::<(Rc<Spanned<Statement>>, ModuleRef)>()
                        .unwrap();

                    if let Statement::FnDef(def) = &stmt.inner {
                        let scope = Rc::new(DynamicScope {
                            root: ScopeRoot::AstObject(stmt.clone() as _),
                            mref: mref.clone(),
                            namespace: DashMap::new(),
                        });

                        match (&def.args, args) {
                            (None, None) => {}
                            (None, Some(_)) => {
                                exception!("arguments provided where none were expected!")
                            }
                            (Some(_), None) => {
                                exception!("arguments expected but none were provided.")
                            }
                            (Some(params), Some(tuple)) => {
                                let args = tuple.iterable(rt)?;
                                let mut params = params.iter().map(|(k, _)| k);

                                while let Some(name) = params.next() {
                                    let arg = match args.clone().call_method(
                                        rt.names.__next__,
                                        rt,
                                        None,
                                        None,
                                    ) {
                                        Ok(a) => a,
                                        Err(exc) if exc.is_stop_iter(rt) => {
                                            exception!("not enough arguments")
                                        }
                                        e @ Err(_) => return e,
                                    };

                                    scope.namespace.insert(name.clone(), arg);
                                }
                            }
                        }

                        rt.stack_frames.push(scope);

                        let mut value = Ok(rt.none());

                        for stmt in def.body.iter() {
                            if let Err(err) = stmt.eval(rt, &mut Module { body: vec![] }) {
                                if let PyErr::Return(v) = err {
                                    value = Ok(v);
                                } else {
                                    value = Err(err);
                                }

                                break;
                            }
                        }

                        let _ = rt.stack_frames.pop();

                        value
                    } else {
                        unreachable!();
                    }
                });

                members.insert(
                    self.names.__value.clone(),
                    MutCell::Immutable(Rc::new((def, mref)) as PyAny),
                );

                members.insert(
                    self.names.__call__.clone(),
                    MutCell::Immutable(Rc::new(call) as PyAny),
                );

                members
            },
            prototype: Some(self.singletons.func_class.clone()),
        })
    }
}
