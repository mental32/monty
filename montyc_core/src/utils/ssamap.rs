use std::{
    cell::Cell,
    convert::{TryFrom, TryInto},
    fmt::Debug,
    marker::PhantomData,
};

type SSAKey = usize;

/// A helper utility for SSA-like map semantics.
#[derive(Debug)]
pub struct SSAMap<K, V>
where
    K: TryFrom<SSAKey> + TryInto<SSAKey>,
{
    inner: Vec<Option<V>>,
    next_free: Cell<SSAKey>,
    _k: PhantomData<K>,
}

impl<K, V> Default for SSAMap<K, V>
where
    K: TryFrom<SSAKey> + TryInto<SSAKey>,
    <K as TryFrom<SSAKey>>::Error: Debug,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> SSAMap<K, V>
where
    K: TryFrom<SSAKey> + TryInto<SSAKey>,
    <K as TryFrom<SSAKey>>::Error: Debug,
{
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: vec![],
            _k: PhantomData,
            next_free: Cell::new(0),
        }
    }

    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (K, &'a V)> {
        self.inner.iter().enumerate().filter_map(|(idx, value)| {
            value
                .as_ref()
                .and_then(|v| K::try_from(idx).ok().map(|k| (k, v)))
        })
    }

    #[inline]
    pub fn skip(&mut self, n: usize) {
        self.next_free.replace(self.next_free.get() + n);

        for _ in 0..n {
            self.inner.push(None);
        }
    }

    #[inline]
    pub fn skip_to_nth(&mut self, pos: usize) -> Result<(), usize> {
        let current = self.next_free.get();

        if pos < current {
            Err(current)
        } else {
            self.next_free.replace(pos);

            for _ in 0..(pos - current) {
                self.inner.push(None);
            }

            Ok(())
        }
    }

    #[inline]
    pub fn reserve(&mut self) -> K {
        let key = self.next_free.replace(self.next_free.get() + 1);

        self.inner.push(None);

        K::from(key.try_into().unwrap())
    }

    #[inline]
    pub fn try_set_value(&mut self, k: K, v: impl Into<V>) -> Result<(), ()>
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        let k: usize = k.try_into().unwrap();

        match self.inner.get_mut(k) {
            None | Some(Some(_)) => return Err(()),

            Some(cell @ None) => {
                cell.replace(v.into());
                Ok(())
            }
        }
    }

    #[inline]
    pub fn insert(&mut self, value: impl Into<V>) -> K {
        let value = value.try_into().unwrap();
        let key = self.next_free.replace(self.next_free.get() + 1);

        assert_eq!(key, self.inner.len());

        self.inner.push(Some(value));

        K::from(key.try_into().unwrap())
    }

    #[inline]
    pub fn get_or_insert(&mut self, key: K, f: impl Fn() -> V) -> &mut V
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        let key = key.try_into().unwrap();

        let key = if self.get_raw(key).is_none() {
            let key = self.insert(f());
            key.try_into().unwrap()
        } else {
            key
        };

        self.get_raw_mut(key).unwrap()
    }

    #[inline]
    pub fn entry(&mut self, value: impl Into<V>) -> K
    where
        V: PartialEq,
    {
        let value = value.into();

        self.inner
            .iter()
            .enumerate()
            .find_map(|(k, v)| {
                v.as_ref().and_then(|v| {
                    if *v == value {
                        Some(K::try_from(k).unwrap())
                    } else {
                        None
                    }
                })
            })
            .unwrap_or_else(|| self.insert(value))
    }

    #[inline]
    pub fn get(&self, key: K) -> Option<&V>
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        self.inner.get(key.try_into().unwrap())?.as_ref()
    }

    /// # Safety
    ///
    /// The key index is not bounds checked, calling this with an out of bounds index
    /// is undefined behavior.
    ///
    #[inline]
    pub unsafe fn get_unchecked(&self, key: K) -> Option<&V>
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        self.inner.get_unchecked(key.try_into().ok()?).as_ref()
    }

    #[inline]
    pub fn get_mut(&mut self, key: K) -> Option<&mut V>
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        self.inner.get_mut(key.try_into().unwrap())?.as_mut()
    }

    #[inline]
    pub fn get_raw(&self, key: SSAKey) -> Option<&V> {
        self.inner.get(key)?.as_ref()
    }

    #[inline]
    pub fn get_raw_mut(&mut self, key: SSAKey) -> Option<&mut V> {
        self.inner.get_mut(key)?.as_mut()
    }
}
