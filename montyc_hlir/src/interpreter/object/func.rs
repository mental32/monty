use petgraph::graph::NodeIndex;

use crate::interpreter::runtime::eval::ModuleExecutor;

use super::{dict::PyDict, ObjAllocId, PyDictRaw, PyObject, PyResult, RawObject};

pub(in crate::interpreter) type NativeFn =
    for<'rt> fn(&'rt ModuleExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>;

pub(in crate::interpreter) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&ModuleExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef(NodeIndex<u32>),
    Object(ObjAllocId),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct Function {
    pub(in crate::interpreter) header: RawObject,
    pub(in crate::interpreter) inner: Callable,
    pub(in crate::interpreter) defsite: Option<NodeIndex>,
}

impl PyObject for Function {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        rt: &crate::interpreter::Runtime,
        hash: crate::interpreter::HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.header.set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &crate::interpreter::Runtime,
        hash: crate::interpreter::HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.header.get_attribute_direct(rt, hash, key)
    }

    fn for_each(
        &self,
        rt: &crate::interpreter::Runtime,
        f: &mut dyn FnMut(crate::interpreter::HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.header.for_each(rt, f)
    }

    fn into_value(
        &self,
        rt: &crate::interpreter::Runtime,
        object_graph: &mut crate::ObjectGraph,
    ) -> crate::Value {
        let properties = self.header.into_value_dict(rt, object_graph);
        let name = self
            .get_attribute_direct(rt, rt.hash("__name__"), self.alloc_id())
            .map(|obj| rt.try_as_str_value(obj))
            .unwrap()
            .unwrap();

        let mut annotations: PyDictRaw<_> = Default::default();

        let __annotations__ = self
            .get_attribute_direct(rt, rt.hash("__annotations__"), self.alloc_id())
            .unwrap();

        let __annotations__ = dbg!(rt.get_object(__annotations__).unwrap().borrow())
            .as_any()
            .downcast_ref::<PyDict>()
            .unwrap()
            .for_each(rt, &mut |hash, key, value| {
                let key = key.into_value(rt, object_graph);
                let key = object_graph.add_string_node(
                    if let crate::Value::String(st) = &key {
                        rt.hash(st)
                    } else {
                        unreachable!()
                    },
                    key,
                );

                let value = value.into_value(rt, object_graph);
                let value = if let crate::Value::String(st) = &value {
                    object_graph.add_string_node(rt.hash(st), value)
                } else {
                    object_graph.add_node(value)
                };

                annotations.insert(hash, (key, value));
            });

        crate::Value::Function {
            name,
            annotations,
            properties,
            defsite: self.defsite,
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
