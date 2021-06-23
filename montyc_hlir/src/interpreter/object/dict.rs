use ahash::AHashMap;

use crate::interpreter::HashKeyT;

use super::{alloc::ObjAllocId, PyObject};

#[derive(Debug)]
pub struct PyDictRaw<V>(AHashMap<HashKeyT, V>);

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
pub(in crate::interpreter) struct PyDict(ObjAllocId, PyDictRaw<ObjAllocId>);

impl<'rt> PyObject for PyDict {
    fn alloc_id(&self) -> ObjAllocId {
        self.0
    }

    fn set_attribute_direct(&mut self, rt: &crate::interpreter::Runtime, hash: crate::interpreter::HashKeyT, key: ObjAllocId, value: ObjAllocId) {
        self.1.insert(hash, value);
    }

    fn get_attribute_direct(&self, rt: &crate::interpreter::Runtime, hash: crate::interpreter::HashKeyT, key: ObjAllocId) -> Option<ObjAllocId> {
        self.1.get(hash)
    }
}
