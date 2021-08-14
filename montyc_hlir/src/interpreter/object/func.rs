use std::cell::RefCell;

use montyc_core::utils::SSAMap;
use petgraph::graph::NodeIndex;

use crate::{
    interpreter::{
        runtime::{ceval::ConstEvalContext, SharedMutAnyObject},
        HashKeyT, Runtime,
    },
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{ObjAllocId, PyDictRaw, PyObject, PyResult, RawObject};

type NativeFn = for<'rt> fn(&'rt ConstEvalContext, &[ObjAllocId]) -> PyResult<ObjAllocId>;

#[allow(dead_code)] // TODO: Remove once it is used.
pub(crate) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&ConstEvalContext, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef(usize),
    Object(ObjAllocId),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

object! {
    struct Function {
        inner: Callable,
        name: String,
        annotations: PyDictRaw<(ObjAllocId, ObjAllocId)>
    }
    fn into_value(
        &self,
        object_graph: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> ObjectGraphIndex {
        if let Some(idx) = object_graph.alloc_to_idx.get(&self.alloc_id()).cloned() {
            return idx;
        }

        object_graph.insert_node_traced(
            self.alloc_id(),
            |object_graph, _| {
                let name = self.name.clone();

                Value::Function {
                    name,
                    annotations: Default::default(),
                    properties: Default::default(),
                }
            },
            |object_graph, value| {
                let p = self.header.into_value_dict(object_graph, objects);

                let mut ann: PyDictRaw<_> = Default::default();

                self.annotations.0.iter().for_each(|(hash, (key, value))| {
                    let key = key.into_value(object_graph, objects);
                    let value = value.into_value(object_graph, objects);

                    ann.insert(*hash, (key, value));
                });

                let (properties, annotations) = if let Value::Function {
                    properties,
                    annotations,
                    ..
                } = value(object_graph)
                {
                    (properties, annotations)
                } else {
                    unreachable!()
                };

                *properties = p;
                *annotations = ann;
            },
        )
    }
}
