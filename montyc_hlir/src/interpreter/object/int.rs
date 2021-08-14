use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

object! {
    struct IntObj { value: i64 }

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
}
