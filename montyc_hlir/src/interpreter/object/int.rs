use montyc_core::utils::SSAMap;

use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    Value, ValueGraphIx,
};

use super::{alloc::ObjAllocId, PyObject, RawObject, ToValue};

object! {
    struct IntObj { value: i64 }
}

impl ToValue for (&Runtime, &IntObj) {
    fn contains(&self, _store: &crate::value_store::GlobalValueStore) -> Option<ValueGraphIx> {
        None
    }

    fn into_raw_value(&self, _store: &crate::value_store::GlobalValueStore) -> crate::Value {
        Value::Integer(self.1.value)
    }

    fn refine_value(
        &self,
        _value: &mut crate::Value,
        _store: &mut crate::value_store::GlobalValueStore,
        value_ix: ValueGraphIx,
    ) {
    }

    fn set_cache(&self, _store: &mut crate::value_store::GlobalValueStore, _ix: ValueGraphIx) {}
}
