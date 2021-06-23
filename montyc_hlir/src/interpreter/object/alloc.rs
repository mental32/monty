//! `ObjAllocId`s are unique allocation identifiers for objects which also implement the `PyObject` trait for convenience.

use std::{borrow::Borrow, convert::{TryFrom, TryInto}, hash::Hash};

use crate::interpreter::{Runtime, runtime::eval::ModuleExecutor};

use super::{dict::PyDictRaw, PyObject};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(in crate::interpreter) struct ObjAllocId(pub usize);

impl ObjAllocId {
    pub fn setattr_static(&mut self, rt: &mut Runtime, key: impl Hash, value: ObjAllocId) {
        let hash = rt.hash(key);
        self.set_attribute_direct(rt, hash, value, value);
    }

    pub fn getattr_static(&self, rt: &Runtime, key: impl Hash) -> Option<ObjAllocId> {
        let hash = rt.hash(key);
        self.get_attribute_direct(rt, hash, ObjAllocId(0))
    }
}

impl<'rt> PyObject for ObjAllocId {
    fn alloc_id(&self) -> ObjAllocId {
        self.clone()
    }

    fn set_attribute_direct(
        &mut self,
        rt: &crate::interpreter::Runtime,
        hash: crate::interpreter::HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        rt.get_object(self.alloc_id())
            .unwrap()
            .borrow_mut()
            .set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &crate::interpreter::Runtime,
        hash: crate::interpreter::HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        rt.get_object(self.alloc_id())
            .unwrap()
            .borrow()
            .get_attribute_direct(rt, hash, key)
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
