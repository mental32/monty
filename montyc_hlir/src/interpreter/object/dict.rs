use ahash::AHashMap;

use crate::interpreter::HashKeyT;

use super::{alloc::ObjAllocId, PyObject};

#[derive(Debug, Default)]
pub(in crate::interpreter) struct PyDictRaw(AHashMap<HashKeyT, ObjAllocId>);

impl PyDictRaw {
    pub fn get(&self, key: HashKeyT) -> Option<ObjAllocId> {
        self.0.get(&key).cloned()
    }

    pub fn insert(&mut self, key: HashKeyT, value: ObjAllocId) -> Option<ObjAllocId> {
        self.0.insert(key, value)
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct PyDict(ObjAllocId, PyDictRaw);

impl<'rt> PyObject<'rt> for PyDict {
    fn alloc_id(&self) -> ObjAllocId {
        self.0
    }
}
