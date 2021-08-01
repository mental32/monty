use crate::{
    interpreter::{HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

#[derive(Debug)]
pub(in crate::interpreter) struct ClassObj {
    pub header: RawObject,
    pub name: String,
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
        object_graph: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.header.for_each(object_graph, f)
    }

    fn into_value(&self, object_graph: &mut ObjectGraph) -> ObjectGraphIndex {
        if let Some(idx) = object_graph.alloc_to_idx.get(&self.alloc_id()).cloned() {
            return idx;
        } else {
            let Self { header, name } = self;

            object_graph.insert_node_traced(
                self.alloc_id(),
                move |_, _| crate::Value::Class {
                    name: name.clone(),
                    properties: Default::default(),
                },
                |object_graph, value| {
                    let props = self.header.into_value_dict(object_graph);

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
