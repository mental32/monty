use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

object! {
    struct StrObj {
        value: String,
        value_hashed: HashKeyT
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
                || Value::String(self.value.clone()),
                |object_graph, index| {
                    object_graph.strings.insert(hash, index);
                },
            )
        })
    }

    fn hash(&self, rt: &Runtime) -> Option<HashKeyT> {
        Some(rt.hash(self.value.clone()))
    }
}
