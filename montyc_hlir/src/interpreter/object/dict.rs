use std::{
    collections::BTreeSet,
    ops::{Deref, DerefMut},
};

use ahash::AHashMap;

use crate::{
    interpreter::{HashKeyT, Runtime},
    ObjectGraph, ObjectGraphIndex,
};

use super::{alloc::ObjAllocId, PyObject, RawObject};

/// A Python dict-like object that stores its values by a pre-computed hash.
#[derive(Debug)]
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
    pub fn iter_by_alloc_asc(&self, graph: &ObjectGraph) -> impl Iterator<Item = ObjectGraphIndex> {
        let values = {
            let mut values = BTreeSet::new();

            for (_, value) in self.0.values() {
                log::trace!(
                    "[PyDictRaw::iter_by_alloc_asc] {:?} := {:?}",
                    value,
                    graph.node_weight(*value)
                );
                values.insert(*value);
            }

            values
        };

        let mut allocs: Vec<_> = graph
            .alloc_to_idx
            .iter()
            .filter_map(|(alloc, index)| {
                log::trace!(
                    "[PyDictRaw::iter_by_alloc_asc] Checking filter for value pair: {:?} ({:?})",
                    (index, alloc),
                    graph.node_weight(*index)
                );

                let pair = values.contains(index).then(|| (*alloc, *index));

                log::trace!(
                    "[PyDictRaw::iter_by_alloc_asc] Filter result? {}",
                    if pair.is_some() {
                        "Retained"
                    } else {
                        "Discarded"
                    }
                );

                pair
            })
            .collect();

        allocs.sort_unstable_by(|(a, _), (b, _)| a.0.cmp(&b.0));

        allocs.into_iter().map(|(_, b)| b)
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct PyDict {
    pub header: RawObject,
    pub data: PyDictRaw<(ObjAllocId, ObjAllocId)>,
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
        self.data.insert(hash, (key, value));
    }

    fn get_attribute_direct(
        &self,
        _rt: &Runtime,
        hash: HashKeyT,
        _key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.data.get(hash).map(|kv| kv.0)
    }

    fn for_each(
        &self,
        rt: &Runtime,
        f: &mut dyn FnMut(&Runtime, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.data.0.iter().for_each(|(h, (k, v))| f(rt, *h, *k, *v))
    }

    fn into_value(&self, rt: &Runtime, object_graph: &mut crate::ObjectGraph) -> crate::Value {
        let mut data: PyDictRaw<_> = Default::default();

        self.properties_into_values(rt, object_graph, &mut data);

        let object = match self.header.into_value(rt, object_graph) {
            crate::Value::Object(obj) => obj,
            _ => unreachable!(),
        };

        crate::Value::Dict { object, data }
    }

    fn set_item(
        &mut self,
        rt: &Runtime,
        key: ObjAllocId,
        value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        self.data.insert(key.hash(rt).unwrap(), (key, value))
    }

    fn get_item(&mut self, rt: &Runtime, key: ObjAllocId) -> Option<(ObjAllocId, ObjAllocId)> {
        self.data.get(key.hash(rt).unwrap())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
