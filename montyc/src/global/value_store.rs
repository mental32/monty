use std::rc::Rc;

use montyc_core::{utils::SSAMap, TypeId};
use montyc_hlir::{ObjectGraph, ObjectGraphIndex, interpreter::ObjAllocId};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ValueId(usize);

impl From<ValueId> for usize {
    fn from(ValueId(n): ValueId) -> Self {
        n
    }
}

impl From<usize> for ValueId {
    fn from(n: usize) -> Self {
        Self(n)
    }
}

#[derive(Debug)]
struct WrappedValue {
    alloc_id: ObjAllocId,
    node_index: ObjectGraphIndex,
    type_id: Option<TypeId>,
}

/// A structure used to track and cache the results of `montyc_hlir::Value` typecheck/inference/whatever.
#[derive(Default, Debug)]
pub struct GlobalValueStore {
    values: SSAMap<ValueId, WrappedValue>,
    values_by_global_id: ahash::AHashMap<ObjAllocId, ValueId>,
    object_graphs: Vec<Rc<ObjectGraph>>,
}

impl GlobalValueStore {
    #[inline]
    pub fn insert(&mut self, node_index: ObjectGraphIndex, alloc_id: ObjAllocId) -> ValueId {
        if let Some(value_id) = self.values_by_global_id.get(&alloc_id) {
            return *value_id;
        }

        let value_id = self.values.insert(WrappedValue {
            alloc_id,
            node_index,
            type_id: Default::default(),
        });

        self.values_by_global_id.insert(alloc_id, value_id);

        value_id
    }

    #[inline]
    pub fn insert_object_graph(&mut self, graph: ObjectGraph) -> Rc<ObjectGraph> {
        self.object_graphs.push(Rc::new(graph));
        self.object_graphs.get(self.object_graphs.len().saturating_sub(1)).unwrap().clone()
    }

    #[inline]
    pub fn get_value_from_alloc(&self, alloc_id: ObjAllocId) -> Option<ValueId> {
        self.values_by_global_id.get(&alloc_id).cloned()
    }

    #[inline]
    pub fn type_of(&self, value_id: ValueId) -> Option<TypeId> {
        self.values
            .get(value_id)
            .and_then(|value| value.type_id.clone())
    }

    #[inline]
    pub fn set_type_of(&mut self, value_id: ValueId, type_id: Option<TypeId>) {
        self.values.get_mut(value_id).map(|value| value.type_id = type_id);
    }

}
