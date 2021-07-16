use crate::{
    interpreter::{HashKeyT, Runtime},
    ObjectGraphIndex,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

#[derive(Debug)]
pub(in crate::interpreter) struct ClassObj {
    pub header: RawObject,
}

impl PyObject for ClassObj {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.header.set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.header.get_attribute_direct(rt, hash, key)
    }

    fn for_each(
        &self,
        rt: &Runtime,
        f: &mut dyn FnMut(&Runtime, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.header.for_each(rt, f)
    }

    fn into_value(&self, rt: &Runtime, object_graph: &mut crate::ObjectGraph) -> ObjectGraphIndex {
        if let Some(idx) = object_graph.alloc_to_idx.get(&self.alloc_id()).cloned() {
            return idx;
        } else {
            object_graph.insert_node_traced(
                self.alloc_id(),
                |_, _| crate::Value::Class {
                    name: self
                        .get_attribute_direct(rt, rt.hash("__name__"), self.alloc_id())
                        .map(|obj| rt.try_as_str_value(obj))
                        .unwrap()
                        .unwrap(),

                    properties: Default::default(),
                },
                |object_graph, value| {
                    let props = self.header.into_value_dict(rt, object_graph);

                    match value(object_graph) {
                        crate::Value::Class { properties, .. } => {
                            *properties = props;
                        }
                        v => unreachable!("{:?}", v),
                    }
                },
            )
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
