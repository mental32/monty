//! `ObjAllocId`s are unique allocation identifiers for objects which also implement the `PyObject` trait for convenience.

use std::{
    convert::{TryFrom, TryInto},
    hash::Hash,
};

use crate::interpreter::Runtime;

use super::PyObject;

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

impl PyObject for ObjAllocId {
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

    fn for_each(
        &self,
        rt: &Runtime,
        f: &mut dyn FnMut(crate::interpreter::HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        rt.get_object(self.alloc_id())
            .unwrap()
            .borrow()
            .for_each(rt, f)
    }

    fn into_value(&self, rt: &Runtime, object_graph: &mut crate::ObjectGraph) -> crate::Value {
        rt.objects
            .get(self.alloc_id())
            .map(|obj| obj.borrow().into_value(rt, object_graph))
            .unwrap()
    }

    fn set_item(
        &mut self,
        rt: &crate::interpreter::Runtime,
        key: ObjAllocId,
        value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        rt.objects
            .get(self.alloc_id())
            .map(|obj| obj.borrow_mut().set_item(rt, key, value))
            .unwrap()
    }

    fn get_item(
        &mut self,
        rt: &crate::interpreter::Runtime,
        key: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        rt.objects
            .get(self.alloc_id())
            .map(|obj| obj.borrow_mut().get_item(rt, key))
            .unwrap()
    }

    fn hash(&self, rt: &Runtime) -> Option<crate::interpreter::HashKeyT> {
        rt.get_object(self.alloc_id()).map(|obj| obj.borrow().hash(rt))?
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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
