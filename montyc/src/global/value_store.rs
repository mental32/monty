use ahash::AHashMap;
use montyc_core::{utils::SSAMap, ModuleRef, TypeId, ValueId};
use montyc_hlir::{typing::TypingContext, ObjAllocId, ObjectGraph, ObjectGraphIndex, Value};

use crate::def_stack::{DefScope, DefStack};

#[derive(Debug)]
enum StoredValue {
    Function {
        /// The module it belongs to.
        mref: ModuleRef,

        /// Module DefScope.
        def_stack: Option<DefStack>,

        /// Used to index the Object graph to access the raw value.
        index: ObjectGraphIndex,

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
        index: ObjectGraphIndex,
    },

    AnyValue {
        /// The Alloc ID of the object.
        value_alloc_id: ObjAllocId,

        /// A slot used to intern the inferred TypeId of the value.
        value_type_id: Option<TypeId>,

        /// Used to index the Object graph to access the raw value.
        value_index: ObjectGraphIndex,

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
}

impl GlobalValueStore {
    #[inline]
    pub fn insert(&mut self, value_index: ObjectGraphIndex, value_alloc_id: ObjAllocId) -> ValueId {
        if let Some(value_id) = self.alloc_id_to_value_id.get(&value_alloc_id) {
            return *value_id;
        }

        let stored_value = StoredValue::AnyValue {
            value_alloc_id,
            value_type_id: None,
            value_index,
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
        mref: ModuleRef,
        def_stack: Option<DefStack>,
        type_id: Option<TypeId>,
        object_graph: &ObjectGraph,
    ) -> ValueId {
        let alloc_id = object_graph.alloc_id_of(value_index).unwrap();

        let value_id = self.values.insert(StoredValue::Function {
            mref,
            def_stack,
            alloc_id,
            type_id,
            index: value_index,
        });

        self.alloc_id_to_value_id.insert(alloc_id, value_id);
        value_id
    }

    #[inline]
    pub fn insert_module(
        &mut self,
        value_index: ObjectGraphIndex,
        object_graph: &ObjectGraph,
    ) -> ValueId {
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
            index: value_index,
        };

        let value_id = self.values.insert(stored_value);

        self.module_ref_to_value_id.insert(mref, value_id);
        value_id
    }

    #[inline]
    pub fn with<T>(
        &self,
        value_id: ValueId,
        object_graph: &ObjectGraph,
        mut f: impl FnMut(&Value) -> T,
    ) -> T {
        let value = self.values.get(value_id).unwrap();

        match value {
            StoredValue::Module {
                index: value_index, ..
            }
            | StoredValue::Function {
                index: value_index, ..
            }
            | StoredValue::AnyValue { value_index, .. } => {
                let value = object_graph.node_weight(*value_index).unwrap();

                f(value)
            }
        }
    }

    #[inline]
    pub fn get_module_value(&self, mref: ModuleRef) -> ValueId {
        self.module_ref_to_value_id.get(&mref).unwrap().clone()
    }

    #[inline]
    pub fn get_value_from_alloc(&self, alloc_id: ObjAllocId) -> Option<ValueId> {
        self.alloc_id_to_value_id.get(&alloc_id).cloned()
    }

    #[inline]
    pub fn class_of<'a>(
        &'a self,
        type_id: TypeId,
        object_graph: &'a ObjectGraph,
    ) -> Option<(ValueId, &'a Value)> {
        let (value_id, value) = self
            .type_ids_to_class_values
            .get(&type_id)
            .and_then(|v| self.values.get(*v).map(|k| (*v, k)))?;

        let value_index = match value {
            StoredValue::Module { .. } => {
                return self.class_of(TypingContext::Module, object_graph)
            }
            StoredValue::Function { .. } => return None,
            StoredValue::AnyValue { value_index, .. } => value_index,
        };

        let value = object_graph.node_weight(*value_index)?;

        Some((value_id, value))
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
