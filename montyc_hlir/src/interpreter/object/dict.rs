use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
};

use ahash::AHashMap;
use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex, Value,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

/// A Python dict-like object that stores its values by a pre-computed hash.
#[derive(Debug, Clone)]
pub struct PyDictRaw<V>(pub AHashMap<HashKeyT, V>);

#[derive(Debug)]
pub struct PyDictNormal<'a, V> {
    inner: &'a mut PyDictRaw<V>,
    hash_state: ahash::RandomState,
}

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
    /// Get a value from the dictionary.
    pub fn get(&self, key: HashKeyT) -> Option<V> {
        self.0.get(&key).cloned()
    }

    /// Insert a value into the dictionary.
    pub fn insert(&mut self, key: HashKeyT, value: V) -> Option<V> {
        self.0.insert(key, value)
    }

    /// Curry the dictionary with a hashing state so it can be used to insert key-value pairs directly.
    pub fn normalize(&mut self, state: ahash::RandomState) -> PyDictNormal<V> {
        PyDictNormal {
            inner: self,
            hash_state: state,
        }
    }
}

impl PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)> {
    /// Return an iterator of object indecies sorted by old to young.
    pub fn iter_by_alloc_asc(
        &self,
        graph: &ObjectGraph,
    ) -> impl Iterator<Item = (ObjectGraphIndex, ObjectGraphIndex)> {
        let values = {
            let mut values = ahash::AHashMap::new();

            for (key, value) in self.0.values() {
                log::trace!(
                    "[PyDictRaw::iter_by_alloc_asc] {:?} := {:?}",
                    value,
                    graph.node_weight(*value)
                );

                values.insert(*value, *key);
            }

            values
        };

        let mut allocs: Vec<_> = graph
            .alloc_to_idx
            .iter()
            .filter_map(|(alloc, index)| values.get(index).map(|key| (*alloc, *index, *key)))
            .collect();

        allocs.sort_unstable_by(|(a, _, _), (b, _, _)| a.0.cmp(&b.0));

        allocs.into_iter().map(|(_, value, key)| (key, value))
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct PyDict {
    pub header: RawObject,
    pub data: RefCell<PyDictRaw<(ObjAllocId, ObjAllocId)>>,
}

impl PyObject for PyDict {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        _rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.data.borrow_mut().insert(hash, (key, value));
    }

    fn get_attribute_direct(
        &self,
        _rt: &Runtime,
        hash: HashKeyT,
        _key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.data.borrow_mut().get(hash).map(|kv| kv.0)
    }

    fn for_each(
        &self,
        rt: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.data
            .borrow_mut()
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
        }

        object_graph.insert_node_traced(
            self.alloc_id(),
            |_, _| Value::Dict {
                object: Default::default(),
                data: Default::default(),
            },
            |object_graph, v| {
                let mut obj = self.header.into_value(object_graph, objects);
                let mut dat = Default::default();
                self.properties_into_values(object_graph, &mut dat, objects);

                let (object, data) = if let Value::Dict { object, data } = v(object_graph) {
                    (object, data)
                } else {
                    unreachable!();
                };

                std::mem::swap(&mut obj, object);
                std::mem::swap(&mut dat, data);
            },
        )
    }

    fn set_item(
        &self,
        rt: &Runtime,
        key: ObjAllocId,
        value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        self.data
            .borrow_mut()
            .insert(key.hash(rt).unwrap(), (key, value))
    }

    fn get_item(&self, rt: &Runtime, key: ObjAllocId) -> Option<(ObjAllocId, ObjAllocId)> {
        self.data.borrow_mut().get(key.hash(rt).unwrap())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
