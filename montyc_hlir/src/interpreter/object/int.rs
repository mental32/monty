use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

#[derive(Debug)]
pub(in crate::interpreter) struct IntObj {
    pub(in crate::interpreter) header: RawObject,
    pub(in crate::interpreter) value: i64,
}

impl PyObject for IntObj {
    fn alloc_id(&self) -> super::alloc::ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: HashKeyT,
        key: super::alloc::ObjAllocId,
        value: super::alloc::ObjAllocId,
    ) {
        self.header.set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: HashKeyT,
        key: super::alloc::ObjAllocId,
    ) -> Option<super::alloc::ObjAllocId> {
        self.header.get_attribute_direct(rt, hash, key)
    }

    fn for_each(
        &self,
        rt: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.header.for_each(rt, f)
    }

    fn into_value(
        &self,
        object_graph: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> ObjectGraphIndex {
        object_graph.insert_node_traced(
            self.alloc_id(),
            |_, _| Value::Integer(self.value),
            |_, _| {},
        )
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
