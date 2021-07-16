use std::rc::Rc;

use ahash::AHashMap;
use montyc_core::{utils::SSAMap, ModuleRef, TypeId};
use montyc_hlir::{ObjAllocId, ObjectGraph, ObjectGraphIndex, Value};

use crate::ribs::RibData;

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
    value_index: ObjectGraphIndex,
    graph_index: usize,
    type_id: Option<TypeId>,
    rib: Option<RibData>,
}

/// A structure used to track and cache the results of `montyc_hlir::Value` typecheck/inference/whatever.
#[derive(Default, Debug)]
pub struct GlobalValueStore {
    values: SSAMap<ValueId, WrappedValue>,

    alloc_id_to_value_id: AHashMap<ObjAllocId, ValueId>,
    type_ids_to_class_values: AHashMap<TypeId, ValueId>,
    module_ref_to_value_id: AHashMap<ModuleRef, ValueId>,

    object_graphs: Vec<Rc<ObjectGraph>>,
}

impl GlobalValueStore {
    #[inline]
    pub fn insert(
        &mut self,
        value_index: ObjectGraphIndex,
        alloc_id: ObjAllocId,
        graph_index: usize,
    ) -> ValueId {
        if let Some(value_id) = self.alloc_id_to_value_id.get(&alloc_id) {
            return *value_id;
        }

        let value_id = self.values.insert(WrappedValue {
            alloc_id,
            value_index,
            graph_index,
            type_id: Default::default(),
            rib: None,
        });

        self.alloc_id_to_value_id.insert(alloc_id, value_id);

        value_id
    }

    #[inline]
    pub fn insert_module(
        &mut self,
        value_index: ObjectGraphIndex,
        alloc_id: ObjAllocId,
        graph_index: usize,
    ) -> ValueId {
        let mref = if let Value::Module { mref, .. } = self.object_graphs[graph_index]
            .node_weight(value_index)
            .unwrap()
        {
            *mref
        } else {
            panic!("Value is not a module");
        };

        let value_id = self.insert(value_index, alloc_id, graph_index);
        self.module_ref_to_value_id.insert(mref, value_id);
        value_id
    }

    #[inline]
    pub fn insert_object_graph(&mut self, graph: ObjectGraph) -> (Rc<ObjectGraph>, usize) {
        self.object_graphs.push(Rc::new(graph));

        let index = self.object_graphs.len().saturating_sub(1);
        let graph = self.object_graphs.get(index).unwrap().clone();

        (graph, index)
    }

    #[inline]
    pub fn get_value_from_alloc(&self, alloc_id: ObjAllocId) -> Option<ValueId> {
        self.alloc_id_to_value_id.get(&alloc_id).cloned()
    }

    #[inline]
    pub fn class_of<'a>(
        &'a self,
        type_id: TypeId,
    ) -> Option<(ValueId, &'a Value, Rc<ObjectGraph>)> {
        let (value_id, value) = self
            .type_ids_to_class_values
            .get(&type_id)
            .and_then(|v| self.values.get(*v).map(|k| (*v, k)))?;

        let (graph, value) = self
            .object_graphs
            .get(value.graph_index)
            .and_then(|graph| {
                graph
                    .node_weight(value.value_index)
                    .map(|value| (Rc::clone(graph), value))
            })?;

        Some((value_id, value, graph))
    }

    #[inline]
    pub fn set_class_of(&mut self, type_id: TypeId, value_id: ValueId) {
        self.type_ids_to_class_values.insert(type_id, value_id);
    }

    #[inline]
    pub fn type_of(&self, value_id: ValueId) -> Option<TypeId> {
        self.values
            .get(value_id)
            .and_then(|value| value.type_id.clone())
    }

    #[inline]
    pub fn set_type_of(&mut self, value_id: ValueId, type_id: Option<TypeId>) {
        self.values
            .get_mut(value_id)
            .map(|value| value.type_id = type_id);
    }

    #[inline]
    pub fn set_rib_data_of(&mut self, module_ref: ModuleRef, rib: Option<RibData>) {
        let value_id = self
            .module_ref_to_value_id
            .get(&module_ref)
            .cloned()
            .unwrap();
        self.values.get_mut(value_id).map(|value| value.rib = rib);
    }

    #[inline]
    pub fn get_rib_data_of(&self, module_ref: ModuleRef) -> Option<RibData> {
        let value_id = self
            .module_ref_to_value_id
            .get(&module_ref)
            .cloned()
            .unwrap();

        self.values
            .get(value_id)
            .and_then(|value| value.rib.clone())
    }
}
