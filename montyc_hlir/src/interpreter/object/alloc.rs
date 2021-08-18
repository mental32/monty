//! `ObjAllocId`s are unique allocation identifiers for objects which also implement the `PyObject` trait for convenience.

use std::{
    convert::{TryFrom, TryInto},
    hash::Hash,
    rc::Rc,
};

use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{
        runtime::{ceval::ConstEvalContext, SharedMutAnyObject},
        HashKeyT, Runtime,
    },
    ObjectGraph, ObjectGraphIndex,
};

use super::PyObject;

/// An `ObjAllocId` is a unique identifier for an object which also implements the `PyObject` trait for convenience.
///
/// It is used to track the allocation of objects in the interpreter.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjAllocId(pub usize);

impl Default for ObjAllocId {
    fn default() -> Self {
        // `usize::MAX` so it makes a nice loud *crash bang* when it's used ;)
        Self(std::usize::MAX)
    }
}

impl ObjAllocId {
    pub(in crate::interpreter) fn getattr_static(
        &self,
        rt: &Runtime,
        key: impl Hash,
    ) -> Option<ObjAllocId> {
        let hash = rt.hash(key);
        self.get_attribute_direct(rt, hash, ObjAllocId(0))
    }

    pub(in crate::interpreter) fn as_ref<T>(
        &self,
        rt: &Runtime,
        mut f: impl FnMut(&dyn PyObject) -> T,
    ) -> T {
        let object = rt.get_object(self.alloc_id()).unwrap();
        f(&*object)
    }

    pub(in crate::interpreter) fn as_mut<T>(
        &self,
        rt: &Runtime,
        mut f: impl FnMut(&mut dyn PyObject) -> T,
    ) -> T {
        let object = rt
            .objects
            .get(self.alloc_id())
            .expect("Allocated objects must always exist.")
            .clone();

        let object = &mut *object.borrow_mut();
        let object = Rc::get_mut(object).unwrap();

        f(object)
    }
}

impl PyObject for ObjAllocId {
    fn alloc_id(&self) -> ObjAllocId {
        self.clone()
    }

    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.as_mut(rt, |obj| obj.set_attribute_direct(rt, hash, key, value))
    }

    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        rt.get_object(self.alloc_id())
            .unwrap()
            .get_attribute_direct(rt, hash, key)
    }

    fn for_each(
        &self,
        object_graph: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        unimplemented!("use rt.for_each(self, f)");
    }

    fn into_value(
        &self,
        graph: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> ObjectGraphIndex {
        objects
            .get(self.alloc_id())
            .expect("allocated objects should always exist.")
            .borrow()
            .into_value(graph, objects)
    }

    fn set_item(
        &self,
        rt: &Runtime,
        key: ObjAllocId,
        value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        self.as_mut(rt, |obj| obj.set_item(rt, key, value))
    }

    fn get_item(&self, rt: &Runtime, key: ObjAllocId) -> Option<(ObjAllocId, ObjAllocId)> {
        self.as_mut(rt, |obj| obj.get_item(rt, key))
    }

    fn hash(&self, rt: &Runtime) -> Option<HashKeyT> {
        self.as_ref(rt, |obj| obj.hash(rt))
    }

    fn call(
        &self,
        ex: &mut ConstEvalContext,
        args: &[ObjAllocId],
    ) -> crate::interpreter::PyResult<ObjAllocId> {
        ex.runtime
            .get_object(self.alloc_id())
            .map(|obj| obj.call(ex, args))
            .unwrap()
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
