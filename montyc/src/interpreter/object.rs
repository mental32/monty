use std::{any::Any, cell::RefCell, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{ast::{atom::Atom, expr::Expr, primary::Primary, Spanned}, exception, interpreter::callable::Callable, prelude::Span, typing::{LocalTypeId, TypeMap}};

use super::{runtime::RuntimeContext, Name, PyObject, PyResult, ToAst};

#[derive(Clone, Debug)]
pub enum MutCell<T> {
    Immutable(T),
    Mutable(RefCell<T>),
}

impl<T> MutCell<T> {
    pub fn into_inner(self) -> T
    where
        T: Clone,
    {
        match self {
            MutCell::Immutable(t) => t,
            MutCell::Mutable(cell) => cell.borrow().clone(),
        }
    }
}

pub(super) struct Object {
    /// The type of this object.
    pub(super) type_id: LocalTypeId,

    /// Basically like `__dict__`
    pub(super) members: DashMap<(NonZeroUsize, NonZeroUsize), MutCell<Rc<dyn Any>>>,

    /// Reference to the class instance of this object.
    pub(super) prototype: Option<Rc<Object>>,
}

impl Object {
    fn setattr<A>(&self, name: &Name, value: A) -> PyResult<Option<Rc<dyn Any>>>
    where
        A: Any,
    {
        let prev = match self.get_member(name) {
            Some(cell) => match &cell {
                MutCell::Immutable(_) => exception!("immutable attribute!"),
                MutCell::Mutable(prev) => Some(prev.replace(Rc::new(value) as Rc<dyn Any>)),
            },

            None => self
                .members
                .insert(
                    name.clone(),
                    MutCell::Mutable(RefCell::new(Rc::new(value) as Rc<dyn Any>)),
                )
                .map(MutCell::into_inner),
        };

        Ok(prev)
    }

    fn get_attribute(&self, name: &(NonZeroUsize, NonZeroUsize)) -> Option<MutCell<Rc<dyn Any>>> {
        match self.members.get(name) {
            Some(attr) => Some(attr.value().clone()),
            None => {
                let proto = self.prototype.as_ref()?;
                proto.get_attribute(name)
            }
        }
    }

    pub fn call(
        self: Rc<Self>,
        py: &mut RuntimeContext,
        args: Option<PyObject>,
        kwargs: Option<PyObject>,
    ) -> PyResult<PyObject> {
        let call = match self.get_attribute(&py.names.__call__) {
            Some(__call__) => __call__.into_inner(),
            None => exception!("no __call__ found."),
        };

        match call.as_ref().downcast_ref::<Callable>() {
            Some(Callable::Object { .. }) => todo!(),
            Some(&Callable::BuiltinFn(f)) => f(self, py, args),
            None => exception!("not a callable."),
        }
    }

    pub fn call_method(
        self: Rc<Self>,
        name: Name,
        py: &mut RuntimeContext,
        args: Option<PyObject>,
        kwargs: Option<PyObject>,
    ) -> PyResult<PyObject> {
        let method = match self.get_attribute(&name) {
            Some(cell) => cell.into_inner(),
            None => exception!("attr not found found."),
        };

        match method.as_ref().downcast_ref::<Callable>() {
            Some(Callable::Object { .. }) => todo!(),
            Some(&Callable::BuiltinFn(f)) => f(self, py, args),
            None => exception!("not a callable."),
        }
    }

    pub fn iterable(self: Rc<Self>, py: &mut RuntimeContext) -> PyResult<PyObject> {
        self.call_method(py.names.__iter__, py, None, None)
    }

    pub fn is_instance(&self, base: PyObject) -> bool {
        match &self.prototype {
            Some(proto) => Rc::ptr_eq(proto, &base),
            None => base.type_id == TypeMap::TYPE && base.prototype.is_none(),
        }
    }

    pub fn class(&self) -> (Option<Rc<Object>>, LocalTypeId) {
        (self.prototype.clone(), self.type_id.clone())
    }

    pub fn get_member(&self, name: &Name) -> Option<MutCell<Rc<dyn Any>>> {
        self.members.get(&name).map(|refm| refm.value().clone())
    }
    pub fn get_member_as_object(&self, name: &Name) -> Option<PyObject> {
        let member = self.get_member(name)?;

        member.into_inner().downcast_ref::<Rc<Object>>().cloned()
    }
}

impl ToAst for Rc<Object> {
    fn to_expr(self, rt: &mut RuntimeContext, span: Span) -> Expr {
        match self.type_id {
            TypeMap::INTEGER => {
                let value = self.get_member(&rt.names.__value).unwrap().into_inner();
                let value = value.downcast_ref::<isize>().unwrap();

                Expr::Primary(Spanned {
                    span: span.clone(),
                    inner: Primary::Atomic(Rc::new(Spanned {
                        span,
                        inner: Atom::Int(*value),
                    })),
                })
            }

            TypeMap::BOOL => {
                let value = self.get_member(&rt.names.__value).unwrap().into_inner();
                let value = value.downcast_ref::<bool>().unwrap();

                Expr::Primary(Spanned {
                    span: span.clone(),
                    inner: Primary::Atomic(Rc::new(Spanned {
                        span,
                        inner: Atom::Bool(*value),
                    })),
                })
            }

            TypeMap::STRING => {
                let value = self.get_member(&rt.names.__value).unwrap().into_inner();
                let value = value.downcast_ref::<String>().unwrap();

                let st = rt.global_context.span_ref.borrow_mut().push_grouped(
                    0..value.len(),
                    &*value,
                    std::path::PathBuf::from(value).into(),
                );

                Expr::Primary(Spanned {
                    span: span.clone(),
                    inner: Primary::Atomic(Rc::new(Spanned {
                        span,
                        inner: Atom::Str(st.0),
                    })),
                })
            }

            _ => unimplemented!(),
        }
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Object").finish()
    }
}
