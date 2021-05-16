#![allow(dead_code)]

use std::{cell::Cell, marker::PhantomData};

type SSAKey = usize;

/// A helper utility for SSA-like map semantics.
#[derive(Debug)]
pub struct SSAMap<K, V>
where
    K: From<SSAKey>,
{
    inner: Vec<V>,
    next_free: Cell<SSAKey>,
    _k: PhantomData<K>,
}

impl<K, V> SSAMap<K, V>
where
    K: From<SSAKey>,
{
    pub fn new() -> Self {
        Self {
            inner: vec![],
            _k: PhantomData,
            next_free: Cell::new(0),
        }
    }

    pub fn insert(&mut self, value: impl Into<V>) -> K {
        let value = value.into();
        let key = self.next_free.replace(self.next_free.get() + 1);

        assert_eq!(key, self.inner.len());

        self.inner.push(value);

        K::from(key)
    }

    pub fn entry(&mut self, value: impl Into<V>) -> K
    where
        V: PartialEq,
    {
        let value = value.into();

        self.inner
            .iter()
            .enumerate()
            .find_map(|(k, v)| (*v == value).then_some(K::from(k)))
            .unwrap_or_else(|| self.insert(value))
    }

    pub fn get(&self, key: K) -> Option<&V>
    where
        K: Into<SSAKey>,
    {
        self.inner.get(key.into())
    }

    pub fn get_raw(&self, key: SSAKey) -> Option<&V> {
        self.inner.get(key)
    }
}
