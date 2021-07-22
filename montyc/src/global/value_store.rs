use std::rc::Rc;

use ahash::AHashMap;
use montyc_core::{utils::SSAMap, ModuleRef, TypeId, ValueId};
use montyc_hlir::{typing::TypingContext, ObjAllocId, ObjectGraph, ObjectGraphIndex, Value};

use crate::def_stack::{DefScope, DefStack};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct UniqueValueIndex {
    // Which ObjectGraph does this value belong to?
    graph_id: usize,

    /// Where in the ObjectGraph is this value found.
    value_index: ObjectGraphIndex,
}

#[derive(Debug)]
enum StoredValue {
    Function {
        /// The module it belongs to.
        mref: ModuleRef,

        /// Module DefScope.
        def_stack: Option<DefStack>,

        /// Used to index the Object graph to access the raw value.
        index: UniqueValueIndex,

        /// The Alloc ID of the function.
        alloc_id: ObjAllocId,

        /// A slot used to intern the inferred TypeId of the value.
        type_id: Option<TypeId>,
    },

    Module {
        /// The module reference.
        mref: ModuleRef,

        /// Module DefScope.
        rib: DefScope,

        /// Used to index the Object graph to access the raw value.
        index: UniqueValueIndex,
    },

    AnyValue {
        /// The Alloc ID of the object.
        value_alloc_id: ObjAllocId,

        /// A slot used to intern the inferred TypeId of the value.
        value_type_id: Option<TypeId>,

        /// Used to index the Object graph to access the raw value.
        value_index: UniqueValueIndex,

        /// An optional slot used to associate DefScope with a value.
        value_rib: Option<DefScope>,
    },
}

/// A structure used to track and cache the results of `montyc_hlir::Value` typecheck/inference/whatever.
#[derive(Default, Debug)]
pub(crate) struct GlobalValueStore {
    values: SSAMap<ValueId, StoredValue>,

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
        value_alloc_id: ObjAllocId,
        graph_id: usize,
    ) -> ValueId {
        if let Some(value_id) = self.alloc_id_to_value_id.get(&value_alloc_id) {
            return *value_id;
        }

        let stored_value = StoredValue::AnyValue {
            value_alloc_id,
            value_type_id: None,
            value_index: UniqueValueIndex {
                graph_id,
                value_index,
            },
            value_rib: None,
        };

        let value_id = self.values.insert(stored_value);

        self.alloc_id_to_value_id.insert(value_alloc_id, value_id);

        value_id
    }

    #[inline]
    pub fn insert_function(
        &mut self,
        value_index: ObjectGraphIndex,
        graph_id: usize,
        mref: ModuleRef,
        def_stack: Option<DefStack>,
        type_id: Option<TypeId>,
    ) -> ValueId {
        let alloc_id = self.object_graphs[graph_id]
            .alloc_id_of(value_index)
            .unwrap();

        let value_id = self.values.insert(StoredValue::Function {
            mref,
            def_stack,
            alloc_id,
            type_id,
            index: UniqueValueIndex {
                graph_id,
                value_index,
            },
        });

        self.alloc_id_to_value_id.insert(alloc_id, value_id);
        value_id
    }

    #[inline]
    pub fn insert_module(&mut self, value_index: ObjectGraphIndex, graph_id: usize) -> ValueId {
        let object_graph = self.object_graphs[graph_id].clone();
        let alloc_id = object_graph.alloc_id_of(value_index).unwrap();

        if let Some(value_id) = self.alloc_id_to_value_id.get(&alloc_id) {
            return *value_id;
        }

        let (_value, mref) = object_graph
            .node_weight(value_index)
            .and_then(|value| match value {
                Value::Module { mref, .. } => Some((value, mref.clone())),
                _ => None,
            })
            .expect("not a module value.");

        let stored_value = StoredValue::Module {
            mref,
            rib: Default::default(),
            index: UniqueValueIndex {
                graph_id,
                value_index,
            },
        };

        let value_id = self.values.insert(stored_value);

        self.module_ref_to_value_id.insert(mref, value_id);
        value_id
    }

    #[inline]
    pub fn with<T>(&self, value_id: ValueId, mut f: impl FnMut(&Value) -> T) -> T {
        let value = self.values.get(value_id).unwrap();

        match value {
            StoredValue::Module {
                index: value_index, ..
            }
            | StoredValue::Function {
                index: value_index, ..
            }
            | StoredValue::AnyValue { value_index, .. } => {
                let graph = self.object_graphs.get(value_index.graph_id).unwrap();
                let value = graph.node_weight(value_index.value_index).unwrap();

                f(value)
            }
        }
    }

    #[inline]
    pub fn get_module_value(&self, mref: ModuleRef) -> ValueId {
        self.module_ref_to_value_id.get(&mref).unwrap().clone()
    }

    #[inline]
    pub fn insert_object_graph(&mut self, graph: ObjectGraph) -> (Rc<ObjectGraph>, usize) {
        self.object_graphs.push(Rc::new(graph));

        let index = self.object_graphs.len().saturating_sub(1);
        let graph = self.object_graphs.get(index).unwrap().clone();

        (graph, dbg!(index))
    }

    #[inline]
    pub fn get_graph_of(&self, value_id: ValueId) -> (Rc<ObjectGraph>, usize) {
        let value = self.values.get(value_id).unwrap();

        match value {
            StoredValue::Module {
                index: value_index, ..
            }
            | StoredValue::Function {
                index: value_index, ..
            }
            | StoredValue::AnyValue { value_index, .. } => (
                self.object_graphs
                    .get(value_index.graph_id)
                    .cloned()
                    .unwrap(),
                value_index.graph_id,
            ),
        }
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

        let (graph_id, value_index) = match value {
            StoredValue::Module { .. } => return self.class_of(TypingContext::Module),
            StoredValue::Function { .. } => return None,
            StoredValue::AnyValue { value_index, .. } => {
                (value_index.graph_id, value_index.value_index)
            }
        };

        let (graph, value) = self.object_graphs.get(graph_id).and_then(|graph| {
            graph
                .node_weight(value_index)
                .map(|value| (Rc::clone(graph), value))
        })?;

        Some((value_id, value, graph))
    }

    #[inline]
    pub fn set_class_of(&mut self, type_id: TypeId, value_id: ValueId) {
        log::trace!(
            "[GlobalValueStore::set_class_of] {:?} is now the class of {:?}",
            value_id,
            type_id
        );

        self.type_ids_to_class_values.insert(type_id, value_id);
    }

    #[inline]
    pub fn type_of(&self, value_id: ValueId) -> Option<TypeId> {
        self.values.get(value_id).and_then(|value| match value {
            StoredValue::Module { .. } => Some(TypingContext::Module),
            StoredValue::Function { type_id, .. } => type_id.clone(),
            StoredValue::AnyValue { value_type_id, .. } => value_type_id.clone(),
        })
    }

    #[inline]
    pub fn set_type_of(&mut self, value_id: ValueId, mut type_id: Option<TypeId>) {
        log::trace!(
            "[GlobalValueStore::set_type_of] {:?} :- {:?}",
            value_id,
            type_id
        );

        self.values.get_mut(value_id).map(|value| match value {
            StoredValue::Function {
                type_id: value_type_id,
                ..
            }
            | StoredValue::AnyValue { value_type_id, .. } => {
                std::mem::swap(value_type_id, &mut type_id)
            }
            StoredValue::Module { .. } => unimplemented!(),
        });
    }

    #[inline]
    pub fn function_rib_stack(
        &mut self,
        value_id: ValueId,
    ) -> Result<DefStack, &mut Option<DefStack>> {
        let value = self.values.get_mut(value_id).unwrap();

        if let StoredValue::Function { def_stack, .. } = value {
            match def_stack {
                Some(def_stack) => Ok(def_stack.clone()),
                None => Err(def_stack),
            }
        } else {
            unimplemented!("not a funciton.");
        }
    }

    #[inline]
    pub fn set_rib_data_of(&mut self, value_id: ValueId, mut rib: Option<DefScope>) {
        log::trace!(
            "[GlobalValueStore::set_rib_data_of] {:?} rib={:?}",
            value_id,
            rib
        );

        let value = self.values.get_mut(value_id).unwrap();

        match value {
            StoredValue::Function { .. } => unimplemented!(),
            StoredValue::AnyValue { value_rib, .. } => std::mem::swap(value_rib, &mut rib),
            StoredValue::Module { rib: value_rib, .. } => {
                std::mem::swap(value_rib, rib.as_mut().unwrap())
            }
        }
    }

    #[inline]
    pub fn get_rib_data_of(&self, value_id: ValueId) -> Option<DefScope> {
        let value = self.values.get(value_id)?;

        match value {
            StoredValue::Function { .. } => unimplemented!(
                "attempted to access rib data of function, use `function_rib_stack` instead."
            ),

            StoredValue::AnyValue { value_rib, .. } => value_rib.clone(),
            StoredValue::Module { rib, .. } => Some(rib.clone()),
        }
    }
}
