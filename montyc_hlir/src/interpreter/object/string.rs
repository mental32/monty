use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

#[derive(Debug)]
pub(in crate::interpreter) struct StrObj {
    pub(in crate::interpreter) header: RawObject,
    pub(in crate::interpreter) value: String,
    pub(in crate::interpreter) value_hashed: HashKeyT,
}

impl PyObject for StrObj {
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
        if let Some(idx) = object_graph.alloc_to_idx.get(&self.alloc_id()).cloned() {
            return idx;
        }

        let hash = self.value_hashed.clone();

        object_graph.strings.get(&hash).cloned().unwrap_or_else(|| {
            object_graph.insert_node_traced(
                self.alloc_id(),
                |object_graph, index| {
                    object_graph.strings.insert(hash, index);
                    Value::String(self.value.clone())
                },
                |_, _| {},
            )
        })
    }

    fn hash(&self, rt: &Runtime) -> Option<HashKeyT> {
        Some(rt.hash(self.value.clone()))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
