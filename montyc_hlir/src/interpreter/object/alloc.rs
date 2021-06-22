//! `ObjAllocId`s are unique allocation identifiers for objects which also implement the `PyObject` trait for convenience.

use std::convert::{TryFrom, TryInto};

use crate::interpreter::runtime::eval::ModuleExecutor;

use super::{dict::PyDictRaw, PyObject, PyObjectRef};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(in crate::interpreter) struct ObjAllocId(pub usize);

impl<'rt> PyObject<'rt> for ObjAllocId {
    fn alloc_id(&self) -> ObjAllocId {
        self.clone()
    }

    fn with_dict<T>(&self, ex: &'rt ModuleExecutor, f: impl Fn(&PyDictRaw) -> T) -> T {
        let obj = ex.runtime.get_object(self.clone()).unwrap();

        obj.with_dict(ex, f)
    }
}

impl ObjAllocId {
    pub fn as_object_ref<'rt>(&self, ex: &'rt ModuleExecutor) -> Option<PyObjectRef<'rt>> {
        let raw = ex.runtime.objects.get(self.clone())?;
        Some(PyObjectRef(raw))
    }
}

impl TryFrom<ObjAllocId> for usize {
    type Error = std::num::TryFromIntError;

    fn try_from(ObjAllocId(n): ObjAllocId) -> Result<Self, Self::Error> {
        Ok(n)
    }
}

impl TryFrom<usize> for ObjAllocId {
    type Error = std::num::TryFromIntError;

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        Ok(Self(n.try_into()?))
    }
}
