use montyc_core::{patma, utils::SSAMap};

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    Value, ValueGraphIx,
};

use super::{alloc::ObjAllocId, PyObject, RawObject, ToValue};

object! {
    struct ClassObj {
        name: String
    }
}

impl ToValue for (&Runtime, &ClassObj) {
    fn contains(&self, store: &crate::value_store::GlobalValueStore) -> Option<ValueGraphIx> {
        store.alloc_data.get(&self.1.alloc_id()).cloned()
    }

    fn into_raw_value(&self, store: &crate::value_store::GlobalValueStore) -> Value {
        Value::Class {
            name: self.1.name.clone(),
            properties: Default::default(),
        }
    }

    fn refine_value(
        &self,
        value: &mut Value,
        store: &mut crate::value_store::GlobalValueStore,
        value_ix: ValueGraphIx,
    ) {
        let (rt, this) = self;
        let properties = patma!(p, Value::Class { properties: p, .. } in value).unwrap();

        store.metadata(value_ix).alloc_id.replace(this.alloc_id());

        this.header
            .__dict__
            .iter()
            .for_each(|(hash, (key, value))| {
                let key = store.insert(&(*rt, key));
                let value = store.insert(&(*rt, value));

                if let Value::Function { class, .. } = store.get_mut(value).unwrap() {
                    class.replace(value_ix);
                }

                properties.insert(*hash, (key, value));
            });
    }

    fn set_cache(&self, store: &mut crate::value_store::GlobalValueStore, ix: ValueGraphIx) {
        store.alloc_data.insert(self.1.alloc_id(), ix);
    }
}
