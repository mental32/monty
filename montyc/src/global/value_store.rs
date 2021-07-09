use cranelift_codegen::ir::Type;
use montyc_core::{utils::SSAMap, TypeId};
use montyc_hlir::ObjectGraphIndex;

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
    inner: ObjectGraphIndex,
    type_id: Option<TypeId>,
}

#[derive(Debug, derive_more::From)]
pub enum TypeDeducible {
    Value(ValueId),
    Index(ObjectGraphIndex),
}

/// A structure used to track and cache the results of `montyc_hlir::Value` typecheck/inference/whatever.
#[derive(Default, Debug)]
pub struct GlobalValueStore {
    values: SSAMap<ValueId, WrappedValue>,
    values_by_index: ahash::AHashMap<ObjectGraphIndex, ValueId>,
}

impl GlobalValueStore {
    #[inline]
    pub fn insert(&mut self, value_idx: ObjectGraphIndex) -> ValueId {
        let value_id = self.values.insert(WrappedValue {
            inner: value_idx,
            type_id: Default::default(),
        });

        self.values_by_index.insert(value_idx, value_id);

        value_id
    }

    pub fn contains(&self, value: ObjectGraphIndex) -> bool {
        self.values_by_index.contains_key(&value)
    }

    #[inline]
    pub fn type_of(&self, value: impl Into<TypeDeducible>) -> Option<TypeId> {
        let value = value.into();
        let value_id = match value {
            TypeDeducible::Index(idx) => self.values_by_index.get(&idx).cloned()?,
            TypeDeducible::Value(value) => value,
        };

        self.values
            .get(value_id)
            .and_then(|value| value.type_id.clone())
    }

    #[inline]
    pub fn set_type_of(&mut self, value: impl Into<TypeDeducible>, type_id: Option<TypeId>) {
        let value = value.into();
        let value_id = match value {
            TypeDeducible::Index(idx) => self.values_by_index.get(&idx).cloned().unwrap(),
            TypeDeducible::Value(value) => value,
        };

        self.values.get_mut(value_id).map(|value| value.type_id = type_id);
    }

}
