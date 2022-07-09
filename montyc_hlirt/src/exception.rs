use std::{io, panic::Location, rc::Rc};

use montyc_core::SpanRef;

use crate::object::ObjectId;

pub type PyResult<T> = ::std::result::Result<T, self::PyException>;

pub trait PyResultExt {
    fn trace(self) -> Self;
}

impl<T> PyResultExt for PyResult<T> {
    #[track_caller]
    fn trace(self) -> Self {
        match self {
            ok @ Ok(_) => ok,
            Err(mut exc) => {
                exc.trace.push(TracePoint {
                    location: Some(Location::caller().clone()),
                });

                return Err(exc);
            }
        }
    }
}

impl Into<PyException> for io::Error {
    fn into(self) -> PyException {
        PyException::new(InnerExc::OsError(Rc::new(self)))
    }
}

#[derive(Debug, Clone)]
pub(crate) enum InnerExc {
    // Object(ObjectId),
    ExceededTicks,

    UnknownObject(ObjectId),

    Return(ObjectId),

    NameError(SpanRef),

    AttributeError(ObjectId, u64),

    OsError(Rc<io::Error>),

    ImportError,

    TypeError,

    NotImplementedError,
}

#[derive(Debug, Clone)]
pub struct TracePoint {
    pub location: Option<Location<'static>>,
}

#[derive(Debug, Clone)]
pub struct PyException {
    pub(crate) message: Option<Box<str>>,
    pub(crate) inner: InnerExc,
    pub(crate) trace: Vec<TracePoint>,
}

impl PyException {
    #[track_caller]
    fn new(inner: InnerExc) -> Self {
        let this = Self {
            message: None,
            inner,
            trace: Vec::with_capacity(2),
        };

        this.trace()
    }

    #[track_caller]
    fn trace(mut self) -> Self {
        self.trace.push(TracePoint {
            location: Some(Location::caller().clone()),
        });

        self
    }
}

impl<T> From<PyException> for Result<T, PyException> {
    fn from(exc: PyException) -> Self {
        Err(exc)
    }
}

impl PyException {
    pub fn is_import_error(&self) -> bool {
        matches!(self.inner, InnerExc::ImportError)
    }

    pub fn is_attribute_arror(&self) -> bool {
        matches!(self.inner, InnerExc::AttributeError(_, _))
    }

    pub fn set_message<S>(mut self, message: S) -> Self
    where
        S: AsRef<str>,
    {
        self.message = Some(message.as_ref().into());
        self
    }

    #[track_caller]
    pub fn tick() -> Self {
        Self::new(InnerExc::ExceededTicks)
    }

    #[track_caller]
    pub fn return_(object: ObjectId) -> Self {
        Self::new(InnerExc::Return(object))
    }

    #[track_caller]
    pub fn no_such_object(alloc: ObjectId) -> Self {
        Self::new(InnerExc::UnknownObject(alloc))
    }

    #[track_caller]
    pub fn import_error() -> Self {
        Self::new(InnerExc::ImportError)
    }

    #[track_caller]
    pub fn name_error(name: SpanRef) -> Self {
        Self::new(InnerExc::NameError(name))
    }

    #[track_caller]
    pub fn attribute_error(obj: ObjectId, hash: u64) -> Self {
        Self::new(InnerExc::AttributeError(obj, hash))
    }

    #[track_caller]
    pub fn not_implemented_error() -> Self {
        Self::new(InnerExc::NotImplementedError)
    }

    #[track_caller]
    pub fn type_error() -> Self {
        Self::new(InnerExc::TypeError)
    }
}
