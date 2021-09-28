use std::rc::Rc;

use montyc_core::{dict::PyDictRaw, patma, SpanRef};
use montyc_flatcode::FlatCode;

use crate::{eval::ctx::CallCx, exception::PyResult, rt::ModuleKey, ObjectId};

use super::{raw_object::RawObject, shared_object::SharedObject};

pub(crate) mod sealed {
    use std::rc::Rc;

    use crate::{
        eval::ctx::CallCx,
        exception::PyResult,
        object::{CallableBuilder, ReadyCallable},
        ObjectId,
    };

    use super::{AnyFunc, PyValue};

    pub trait IntoPyValue {
        fn into_py_val(self) -> PyValue;
    }

    impl<T> IntoPyValue for T
    where
        T: Into<PyValue>,
    {
        fn into_py_val(self) -> PyValue {
            self.into()
        }
    }

    impl IntoPyValue for ReadyCallable {
        fn into_py_val(self) -> PyValue {
            match self {
                CallableBuilder::Ready(func) => PyValue::Callable(func),
                CallableBuilder::Building { .. } => {
                    panic!("can not turn an unfinished callable into a pyvalue.")
                }
            }
        }
    }

    impl IntoPyValue for String {
        fn into_py_val(self) -> PyValue {
            PyValue::Str(self.into_boxed_str())
        }
    }

    pub trait IntoAnyFunc {
        fn into_any_func(self) -> AnyFunc;
    }

    impl<F> IntoAnyFunc for F
    where
        F: Fn(CallCx) -> PyResult<ObjectId> + 'static,
    {
        fn into_any_func(self) -> AnyFunc {
            macro_rules! callable {
                (dyn) => {
                    dyn Fn($crate::eval::ctx::CallCx) -> $crate::exception::PyResult<ObjectId>
                }
            }

            AnyFunc::Boxed {
                inner: Rc::new(self) as Rc<callable!(dyn)>,
                hook: None,
            }
        }
    }

    impl IntoAnyFunc for AnyFunc {
        fn into_any_func(self) -> AnyFunc {
            self
        }
    }
}

pub use sealed::IntoPyValue;

pub type NativeFn<Rv = ObjectId> = fn(CallCx) -> PyResult<Rv>;

impl From<NativeFn<()>> for PyValue {
    fn from(func: NativeFn<()>) -> Self {
        let body = AnyFunc::Boxed {
            inner: Rc::new(move |cx| {
                let none_v = cx.ecx.runtime_mut().singletons.none_v;

                func(cx)?;

                Ok(none_v)
            }),

            hook: None,
        };

        Self::Function {
            body,
            params: vec![].into_boxed_slice(),
            parent: None,
            returns: None,
            inner: RawObject::default(),
        }
    }
}

impl From<NativeFn<ObjectId>> for PyValue {
    fn from(func: NativeFn<ObjectId>) -> Self {
        Self::Function {
            body: AnyFunc::Native {
                inner: func,
                hook: None,
            },

            params: vec![].into_boxed_slice(),

            parent: None,
            returns: None,
            inner: RawObject::default(),
        }
    }
}

#[derive(Clone)]
pub enum GlobalsHook {
    Globals(ObjectId),
    ComputeWith(NativeFn),
}

#[derive(Clone)]
pub enum AnyFunc {
    Native {
        inner: NativeFn,
        hook: Option<GlobalsHook>,
    },

    Boxed {
        inner: Rc<dyn Fn(CallCx) -> PyResult<ObjectId>>,
        hook: Option<GlobalsHook>,
    },

    Code {
        module: Rc<FlatCode>,
        seq_id: usize,
    },
}

impl std::fmt::Debug for AnyFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { inner, .. } => f
                .debug_tuple("Native")
                .field(&(&*inner as *const _))
                .finish(),

            Self::Boxed { inner, .. } => f
                .debug_tuple("Boxed")
                .field(&(&*inner as *const _))
                .finish(),

            Self::Code { module, seq_id, .. } => f
                .debug_struct("Code")
                .field("module", module)
                .field("seq", seq_id)
                .finish(),
        }
    }
}

/// Inspection friendly view of function-like objects.
#[derive(Debug)]
#[non_exhaustive]
pub enum FuncLike<'a> {
    Def {
        body: &'a AnyFunc,
        params: &'a [(SpanRef, Option<ObjectId>)],
        returns: Option<ObjectId>,
    },
}

/// Representation of any Python object.
#[derive(Debug, Clone, derive_more::From)]
pub enum PyValue {
    // basically every other runtime object.
    Any(RawObject),

    // primitives
    Int(i64),
    Float(f64),
    Bool(bool),
    None,
    Ellipsis,

    // basic compounds
    Bytes(Vec<u8>),
    Str(Box<str>),
    List(Vec<ObjectId>),
    Dict(PyDictRaw<(ObjectId, ObjectId)>),
    Callable(AnyFunc),

    // Complex types
    Module {
        mkey: ModuleKey,
        inner: RawObject,
    },

    Function {
        body: AnyFunc,
        params: Box<[(SpanRef, Option<ObjectId>)]>,
        parent: Option<ObjectId>,
        returns: Option<ObjectId>,
        inner: RawObject,
    },

    Class {
        name: Option<SpanRef>,
        parent: Option<ObjectId>,
        inner: RawObject,
    },

    /// `SharedObject`s are cloneable objects to `PyObject` trait objects.
    ///
    /// They are used when implementing a structure natively in Rust but necessary
    /// to express within the runtime.
    ///
    Dynamic(SharedObject),
}

impl PyValue {
    #[inline]
    pub fn as_int(&self) -> Option<i64> {
        patma!(*n, PyValue::Int(n) in self)
    }

    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        patma!(st.as_ref(), PyValue::Str(st) in self)
    }

    #[inline]
    pub fn as_list(&self) -> Option<&[ObjectId]> {
        patma!(elem.as_slice(), PyValue::List(ref elem) in self)
    }

    #[inline]
    pub fn as_func<'a>(&'a self) -> Option<FuncLike<'a>> {
        match self {
            PyValue::Function {
                body,
                params,
                returns,
                ..
            } => {
                let returns = returns.clone();
                let def = FuncLike::Def {
                    body,
                    params,
                    returns,
                };

                Some(def)
            }

            _ => None,
        }
    }

    /// Collect all internal references of this value into a vec.
    #[inline]
    pub fn refs(&self) -> Vec<ObjectId> {
        let mut bucket = Vec::new();
        self.refs_with(&mut bucket);
        bucket
    }

    /// Like `refs` but allows the caller to provide a vec to extend.
    #[inline]
    pub fn refs_with(&self, bucket: &mut Vec<ObjectId>) {
        fn func_refs(func: &AnyFunc, _bucket: &mut Vec<ObjectId>) {
            match func {
                AnyFunc::Native { .. } | AnyFunc::Boxed { .. } | AnyFunc::Code { .. } => (),
            }
        }

        match self {
            PyValue::Class { inner: raw, .. }
            | PyValue::Module { inner: raw, .. }
            | PyValue::Any(raw) => bucket.extend(
                raw.__dict__
                    .iter()
                    .flat_map(|(_, (k, v))| [*k, *v].into_iter())
                    .chain(Some(raw.__class__.clone()).into_iter()),
            ),

            PyValue::List(elems) => bucket.extend_from_slice(elems.as_slice()),
            PyValue::Dict(elems) => {
                bucket.extend(elems.iter().flat_map(|(_, (k, v))| [*k, *v].into_iter()))
            }

            PyValue::Callable(func) => {
                func_refs(func, bucket);
            }

            PyValue::Function {
                body,
                params,
                parent,
                returns,
                inner,
            } => {
                bucket.extend(
                    inner
                        .__dict__
                        .iter()
                        .flat_map(|(_, (k, v))| [*k, *v].into_iter())
                        .chain(Some(inner.__class__.clone()).into_iter())
                        .chain(parent.clone().into_iter()),
                );

                bucket.extend(
                    params
                        .iter()
                        .map(|(_, ann)| ann.clone())
                        .chain(Some(returns.clone()))
                        .filter_map(|ann| ann.clone()),
                );

                func_refs(body, bucket);
            }

            PyValue::Dynamic(_) => todo!(),

            PyValue::Int(_)
            | PyValue::Float(_)
            | PyValue::Bool(_)
            | PyValue::None
            | PyValue::Ellipsis
            | PyValue::Bytes(_)
            | PyValue::Str(_) => (),
        };
    }
}
