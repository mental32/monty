use std::cell::RefCell;

use montyc_core::{utils::SSAMap, ModuleRef, SpanRef};
use petgraph::graph::NodeIndex;

use crate::{
    interpreter::{
        runtime::{ceval::ConstEvalContext, SharedMutAnyObject},
        HashKeyT, Runtime,
    },
    CallableSignature, ObjectGraph, ObjectGraphIndex, Value,
};

use super::{ObjAllocId, PyDictRaw, PyObject, PyResult, RawObject};

type NativeFn = for<'rt> fn(&'rt ConstEvalContext, &[ObjAllocId]) -> PyResult<ObjAllocId>;

pub(crate) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&ConstEvalContext, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef(ModuleRef, usize),
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
        returns: ObjAllocId,
        params: CallableSignature<ObjAllocId>,
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

        let ret_t = self.returns.into_value(object_graph, objects);
        let args_t = self.params.as_ref().map(|(recv, params)| {
            let mut out = Vec::with_capacity(params.len());

            for (name, ann) in params.iter() {
                let ann = ann.map(|alloc| alloc.into_value(object_graph, objects));
                out.push((name.clone(), ann));
            }

            (recv.clone(), out.into_boxed_slice())
        }).clone();

        let source = match self.inner {
            Callable::SourceDef(a, b) => Some((a, b)),
            _ => None,
        };

        object_graph.insert_node_traced(
            self.alloc_id(),
            || {
                let name = self.name.clone();

                Value::Function {
                    name,
                    source,
                    ret_t,
                    args_t,
                    properties: Default::default(),
                    annotations: Default::default(),
                }
            },

            |object_graph, index| {
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
                } = object_graph.node_weight_mut(index).unwrap()
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
