use ahash::AHashMap;
use montyc_core::utils::SSAMap;

use super::{PyObject, RawObject};
use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjAllocId, ObjectGraph,
};

pub type FrameLocals = AHashMap<u32, (HashKeyT, ObjAllocId, ObjAllocId)>;

#[derive(Debug)]
pub(in crate::interpreter) struct FrameObject {
    pub inner: RawObject,
    pub locals: FrameLocals,
}

impl PyObject for FrameObject {
    fn alloc_id(&self) -> ObjAllocId {
        self.inner.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.inner.set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.inner.get_attribute_direct(rt, hash, key)
    }

    fn for_each(
        &self,
        rt: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.inner.for_each(rt, f)
    }

    fn into_value(
        &self,
        rt: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> crate::ObjectGraphIndex {
        unimplemented!()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
