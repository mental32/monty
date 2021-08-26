use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    Value, ValueGraphIx,
};

use super::{alloc::ObjAllocId, PyObject, RawObject, ToValue};

object! {
    struct StrObj {
        value: String,
        value_hashed: HashKeyT
    }

    fn class_alloc_id(&self, rt: &Runtime) -> ObjAllocId {
        rt.singletons.string_class
    }

    fn hash(&self, rt: &Runtime) -> Option<HashKeyT> {
        Some(rt.hash(self.value.clone()))
    }
}

impl ToValue for (&Runtime, &StrObj) {
    fn contains(&self, store: &crate::value_store::GlobalValueStore) -> Option<ValueGraphIx> {
        let (rt, this) = self;

        store.string_data.get(&this.hash(rt).unwrap()).cloned()
    }

    fn into_raw_value(&self, _store: &crate::value_store::GlobalValueStore) -> crate::Value {
        Value::String(self.1.value.clone())
    }

    fn refine_value(
        &self,
        value: &mut crate::Value,
        store: &mut crate::value_store::GlobalValueStore,
        value_ix: ValueGraphIx,
    ) {
        let (rt, this) = self;
        let hash = this.hash(rt).unwrap();

        store.string_data.insert(hash, value_ix);
    }

    fn set_cache(&self, store: &mut crate::value_store::GlobalValueStore, ix: ValueGraphIx) {
        store.alloc_data.insert(self.1.alloc_id(), ix);
    }
}
