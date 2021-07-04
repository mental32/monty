use std::ops::{Deref, DerefMut};

use ahash::AHashMap;

use crate::interpreter::HashKeyT;

use super::{PyObject, RawObject, alloc::ObjAllocId};

#[derive(Debug)]
pub struct PyDictRaw<V>(pub AHashMap<HashKeyT, V>);

impl<V> DerefMut for PyDictRaw<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<V> Deref for PyDictRaw<V> {
    type Target = AHashMap<HashKeyT, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V> Default for PyDictRaw<V> {
    fn default() -> Self {
        Self(AHashMap::new())
    }
}

impl<V> PyDictRaw<V>
where
    V: Clone,
{
    pub fn get(&self, key: HashKeyT) -> Option<V> {
        self.0.get(&key).cloned()
    }

    pub fn insert(&mut self, key: HashKeyT, value: V) -> Option<V> {
        self.0.insert(key, value)
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct PyDict {
    pub header: RawObject,
    pub data: PyDictRaw<(ObjAllocId, ObjAllocId)>
}

impl PyObject for PyDict {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        _rt: &crate::interpreter::Runtime,
        hash: crate::interpreter::HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.data.insert(hash, (key, value));
    }

    fn get_attribute_direct(
        &self,
        _rt: &crate::interpreter::Runtime,
        hash: crate::interpreter::HashKeyT,
        _key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.data.get(hash).map(|kv| kv.0)
    }

    fn for_each(
        &self,
        _rt: &crate::interpreter::Runtime,
        f: &mut dyn FnMut(crate::interpreter::HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.data .0.iter().for_each(|(h, (k, v))| f(*h, *k, *v))
    }

    fn into_value(
        &self,
        rt: &crate::interpreter::Runtime,
        object_graph: &mut crate::ObjectGraph,
    ) -> crate::Value {
        let mut data: PyDictRaw<_> = Default::default();

        self.for_each(rt, &mut |hash, key, value| {
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

            data.insert(hash, (key, value));
        });

        let object = match self.header.into_value(rt, object_graph) {
            crate::Value::Object(obj) => obj,
            _ => unreachable!(),
        };

        crate::Value::Dict {
            object,
            data,
        }
    }

    fn set_item(
        &mut self,
        rt: &crate::interpreter::Runtime,
        key: ObjAllocId,
        value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        self.data.insert(key.hash(rt).unwrap(), (key, value))
    }

    fn get_item(
        &mut self,
        rt: &crate::interpreter::Runtime,
        key: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        todo!();
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
