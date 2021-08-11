use std::{cell::RefCell, rc::Rc};

use montyc_core::utils::SSAMap;
use petgraph::graph::NodeIndex;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    typing::TypingContext,
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{dict::PyDictRaw, ObjAllocId, PyObject};

/// Fundemental object representation.
#[derive(Debug)]
pub(in crate::interpreter) struct RawObject {
    /// Every object is assigned a unique object allocation ID (`ObjAllocId`)
    pub alloc_id: ObjAllocId,

    /// This **the** `__dict__` slot, almost everything Python-centric gets stored here.
    pub __dict__: PyDictRaw<(ObjAllocId, ObjAllocId)>,

    /// The class of the object.
    pub __class__: ObjAllocId,
}

impl RawObject {
    #[inline]
    pub fn into_value_dict(
        &self,
        graph: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> PyDictRaw<(NodeIndex, NodeIndex)> {
        let mut properties: PyDictRaw<_> = Default::default();

        self.properties_into_values(graph, &mut properties, objects);

        properties
    }
}

impl From<RawObject> for RefCell<Box<dyn PyObject>> {
    fn from(raw: RawObject) -> Self {
        RefCell::new(Box::new(raw) as _)
    }
}

impl From<RawObject> for Rc<RefCell<Box<dyn PyObject>>> {
    fn from(object: RawObject) -> Self {
        Rc::new(object.into())
    }
}

impl From<RawObject> for Rc<RefCell<Rc<dyn PyObject>>> {
    fn from(object: RawObject) -> Self {
        Rc::new(RefCell::new(Rc::new(object) as _))
    }
}

impl PyObject for RawObject {
    fn alloc_id(&self) -> ObjAllocId {
        self.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        _rt: &Runtime,
        hash: crate::interpreter::HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.__dict__.insert(hash, (key, value));
    }

    fn get_attribute_direct(
        &self,
        _rt: &Runtime,
        hash: crate::interpreter::HashKeyT,
        _key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.__dict__.get(hash).map(|kv| kv.1)
    }

    fn for_each(
        &self,
        rt: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.__dict__
            .0
            .iter()
            .for_each(|(h, (k, v))| f(rt, *h, *k, *v))
    }

    fn into_value(
        &self,
        object_graph: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> ObjectGraphIndex {
        if let Some(idx) = object_graph.alloc_to_idx.get(&self.alloc_id()).cloned() {
            return idx;
        } else {
            object_graph.insert_node_traced(
                self.alloc_id,
                |_, _| {
                    Value::Object(crate::Object {
                        type_id: TypingContext::Object,
                        properties: Default::default(),
                    })
                },
                |object_graph, value| {
                    let props = self.into_value_dict(object_graph, objects);
                    if let Value::Object(object) = value(object_graph) {
                        object.properties = props;
                    } else {
                        unreachable!()
                    }
                },
            )
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
