use crate::{value::ValueId, MapT};
use std::ops::{Deref, DerefMut};

pub type HashKeyT = u64;

/// A Python dict-like object that stores its values by a pre-computed hash.
#[derive(Debug, Clone, derive_more::From)]
pub struct PyDictRaw<V>(pub MapT<HashKeyT, V>);

impl<V> DerefMut for PyDictRaw<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<V> Deref for PyDictRaw<V> {
    type Target = MapT<HashKeyT, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V> Default for PyDictRaw<V> {
    fn default() -> Self {
        Self(MapT::new())
    }
}

impl<V> PyDictRaw<V>
where
    V: Clone,
{
    /// Get a value from the dictionary.
    pub fn get(&self, key: HashKeyT) -> Option<V> {
        self.0.get(&key).cloned()
    }

    /// Insert a value into the dictionary.
    pub fn insert(&mut self, key: HashKeyT, value: V) -> Option<V> {
        self.0.insert(key, value)
    }
}

type KeyValue = (ValueId, ValueId);

impl PyDictRaw<KeyValue> {
    /// Return an iterator of object indecies sorted by old to young.
    pub fn iter_by_alloc_asc<F, I>(&self, filter: F) -> impl Iterator<Item = KeyValue>
    where
        F: Fn(MapT<ValueId, ValueId>) -> I,
        I: Iterator<Item = KeyValue>,
    {
        let values = {
            let mut values = MapT::new();

            for (key, value) in self.0.values() {
                values.insert(*value, *key);
            }

            values
        };

        filter(values)
    }
}
