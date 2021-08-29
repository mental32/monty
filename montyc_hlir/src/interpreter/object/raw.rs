use std::{cell::RefCell, rc::Rc};

use montyc_core::{patma, utils::SSAMap};
use petgraph::graph::NodeIndex;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    typing::TypingContext,
    Value, ValueGraphIx,
};

use super::{dict::PyDictRaw, ObjAllocId, PyObject, ToValue};

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

impl ToValue for (&Runtime, &RawObject) {
    fn contains(
        &self,
        store: &crate::value_store::GlobalValueStore,
    ) -> Option<crate::value_store::ValueGraphIx> {
        store.alloc_data.get(&self.1.alloc_id()).cloned()
    }

    fn into_raw_value(&self, store: &crate::value_store::GlobalValueStore) -> crate::Value {
        Value::Object {
            type_id: TypingContext::Object,
            properties: Default::default(),
        }
    }

    fn refine_value(
        &self,
        value: &mut crate::Value,
        store: &mut crate::value_store::GlobalValueStore,
        value_ix: ValueGraphIx,
    ) {
        let (rt, this) = self;
        let object = patma!(o, Value::Object(o) in value).unwrap();

        store.metadata(value_ix).alloc_id.replace(this.alloc_id());

        this.__dict__.iter().for_each(|(hash, (key, value))| {
            let key = store.insert(&(*rt, key));
            let value = store.insert(&(*rt, value));

            {
                let key_value = store.value_graph.node_weight(key);
                debug_assert!(
                    matches!(key_value, Some(Value::String(_))),
                    "{:?}",
                    rt.objects.get(store.alloc_id_of(key).unwrap())
                );
            }

            object.properties.insert(*hash, (key, value));
        });
    }

    fn set_cache(&self, store: &mut crate::value_store::GlobalValueStore, ix: ValueGraphIx) {
        store.alloc_data.insert(self.1.alloc_id(), ix);
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

    fn class_alloc_id(&self, rt: &Runtime) -> ObjAllocId {
        self.__class__
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
