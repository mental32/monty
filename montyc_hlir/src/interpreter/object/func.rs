use std::cell::RefCell;

use montyc_core::{patma, utils::SSAMap, ModuleRef, SpanRef};
use petgraph::graph::NodeIndex;

use crate::{
    interpreter::{
        runtime::{ceval::ConstEvalContext, SharedMutAnyObject},
        HashKeyT, Runtime,
    },
    CallableSignature, Value, ValueGraphIx,
};

use super::{ObjAllocId, PyDictRaw, PyObject, PyResult, RawObject, ToValue};

type NativeFn = for<'rt> fn(&'rt ConstEvalContext, &[ObjAllocId]) -> PyResult<ObjAllocId>;

pub(crate) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&ConstEvalContext, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef(ModuleRef, usize),
    Object(ObjAllocId),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

object! {
    struct Function {
        inner: Callable,
        name: String,
        returns: ObjAllocId,
        params: CallableSignature<ObjAllocId>,
        annotations: PyDictRaw<(ObjAllocId, ObjAllocId)>
    }
}

impl ToValue for (&Runtime, &Function) {
    fn contains(&self, store: &crate::value_store::GlobalValueStore) -> Option<ValueGraphIx> {
        store.alloc_data.get(&self.1.alloc_id()).cloned()
    }

    fn into_raw_value(&self, store: &crate::value_store::GlobalValueStore) -> crate::Value {
        let (rt, this) = self;

        let source = match this.inner {
            Callable::SourceDef(a, b) => Some((a, b)),
            _ => None,
        };

        Value::Function {
            source,
            name: this.name.clone(),
            ret_t: Default::default(),
            args_t: Default::default(),
            properties: Default::default(),
            annotations: Default::default(),
        }
    }

    fn refine_value(
        &self,
        value: &mut crate::Value,
        store: &mut crate::value_store::GlobalValueStore,
        value_ix: ValueGraphIx,
    ) {
        let (rt, this) = self;

        store.metadata(value_ix).alloc_id.replace(this.alloc_id());

        let (props, ann, args, ret) =
            patma!((p, a, args_t, ret_t), Value::Function { properties: p, annotations: a, args_t, ret_t, .. } in value).unwrap();

        *ret = store.insert(&(*rt, &this.returns));

        *args = this
            .params
            .as_ref()
            .map(|(recv, params)| {
                let mut out = Vec::with_capacity(params.len());

                for (name, ann) in params.iter() {
                    let ann = ann.map(|alloc| store.insert(&(*rt, &alloc)));
                    out.push((name.clone(), ann));
                }

                (recv.clone(), out.into_boxed_slice())
            })
            .clone();

        this.header
            .__dict__
            .iter()
            .for_each(|(hash, (key, value))| {
                let key = store.insert(&(*rt, key));
                let value = store.insert(&(*rt, value));

                props.insert(*hash, (key, value));
            });

        this.annotations.0.iter().for_each(|(hash, (key, value))| {
            let key = store.insert(&(*rt, key));
            let value = store.insert(&(*rt, value));

            ann.insert(*hash, (key, value));
        });
    }

    fn set_cache(&self, store: &mut crate::value_store::GlobalValueStore, ix: ValueGraphIx) {
        store.alloc_data.insert(self.1.alloc_id(), ix);
    }
}
