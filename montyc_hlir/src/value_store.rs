#![allow(missing_docs)]

use ahash::AHashMap;
use montyc_core::{ModuleRef, TypeId};
use petgraph::graph::{DiGraph, NodeIndex};

use crate::{
    interpreter::object::{ObjAllocId, ToValue},
    Value,
};

pub type ValueGraphIx = NodeIndex<u32>;
pub type RawValueGraph = DiGraph<Value, ()>;

pub trait GVKey {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<(ValueGraphIx, &'a Value)>;

    fn resolve_mut<'a>(
        &self,
        store: &'a mut GlobalValueStore,
    ) -> Option<(ValueGraphIx, &'a mut Value)>;
}

impl GVKey for TypeId {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<(ValueGraphIx, &'a Value)> {
        store.type_data.get(self)?.resolve(store)
    }

    fn resolve_mut<'a>(
        &self,
        store: &'a mut GlobalValueStore,
    ) -> Option<(ValueGraphIx, &'a mut Value)> {
        store.type_data.get(self)?.clone().resolve_mut(store)
    }
}

impl GVKey for ValueGraphIx {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<(ValueGraphIx, &'a Value)> {
        store
            .value_graph
            .node_weight(*self)
            .map(|node| (*self, node))
    }

    fn resolve_mut<'a>(
        &self,
        store: &'a mut GlobalValueStore,
    ) -> Option<(ValueGraphIx, &'a mut Value)> {
        store
            .value_graph
            .node_weight_mut(*self)
            .map(|node| (*self, node))
    }
}

impl GVKey for ModuleRef {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<(ValueGraphIx, &'a Value)> {
        store.module_data.get(self)?.resolve(store)
    }

    fn resolve_mut<'a>(
        &self,
        store: &'a mut GlobalValueStore,
    ) -> Option<(ValueGraphIx, &'a mut Value)> {
        store.module_data.get(self)?.clone().resolve_mut(store)
    }
}

#[derive(Debug, Default)]
pub struct ValueMetadata {
    /// The value index which this metadata applies to.
    pub value_ix: ValueGraphIx,

    /// A module reference.
    pub module_ref: Option<ModuleRef>,

    /// A type id slot.
    pub type_id: Option<TypeId>,

    /// An alloc slot.
    pub alloc_id: Option<ObjAllocId>,

    /// A rib for the value.
    pub rib: Option<AHashMap<u32, TypeId>>,

    /// The class instance of this value.
    pub class: Option<ValueGraphIx>,

    /// The parent scope of this value.
    pub parent_scope: Option<ValueGraphIx>,

    /// Associated code as a function.
    pub function: Option<crate::func::Function>,
}

#[derive(Debug, Default)]
pub struct GlobalValueStore {
    pub value_graph: RawValueGraph,
    pub metadata: AHashMap<ValueGraphIx, ValueMetadata>,

    pub string_data: AHashMap<u64, ValueGraphIx>,
    pub alloc_data: AHashMap<ObjAllocId, ValueGraphIx>,
    pub type_data: AHashMap<TypeId, ValueGraphIx>,
    pub module_data: AHashMap<ModuleRef, ValueGraphIx>,
}

impl GlobalValueStore {
    /// Lookup a value by some key.
    #[inline]
    pub fn get<'a>(&'a self, key: impl GVKey) -> Option<&'a Value> {
        key.resolve(self).map(|(_, node)| node)
    }

    #[inline]
    pub fn get_mut<'a>(&'a mut self, key: impl GVKey) -> Option<&'a mut Value> {
        key.resolve_mut(self).map(|(_, node)| node)
    }

    #[inline]
    pub(crate) fn insert<V>(&mut self, value: &V) -> ValueGraphIx
    where
        V: ToValue,
    {
        if let Some(ix) = value.contains(self) {
            return ix;
        }

        let mut data_value = value.into_raw_value(self);
        let index = self.value_graph.add_node(data_value.clone());

        value.set_cache(self, index);

        value.refine_value(&mut data_value, self, index);

        std::mem::swap(
            self.value_graph.node_weight_mut(index).unwrap(),
            &mut data_value,
        );

        index
    }
}

impl GlobalValueStore {
    /// Get the alloc id of a value.
    #[inline]
    pub fn alloc_id_of(&self, ix: ValueGraphIx) -> Option<ObjAllocId> {
        self.metadata.get(&ix)?.alloc_id
    }

    #[inline]
    pub fn metadata(&mut self, key: impl GVKey + core::fmt::Debug) -> &mut ValueMetadata {
        let (value_ix, _) = match key.resolve(self) {
            Some(p) => p,
            None => panic!("could not resolve key to value index {:?}", key),
        };

        self.metadata
            .entry(value_ix)
            .or_insert_with(|| ValueMetadata {
                value_ix,
                ..Default::default()
            })
    }
}
