use std::{
    cell::Cell,
    convert::{TryFrom, TryInto},
    fmt::Debug,
};

type SSAKey = usize;

/// A helper utility for SSA-like map semantics.
#[derive(Debug)]
pub struct SSAMap<V> {
    inner: Vec<Option<V>>,
    next_free: Cell<SSAKey>,
}

impl<V> Default for SSAMap<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> SSAMap<V> {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: vec![],
            next_free: Cell::new(0),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.iter().next().is_none()
    }

    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (usize, &'a V)> {
        self.inner
            .iter()
            .enumerate()
            .filter_map(|(idx, value)| value.as_ref().map(|v| (idx, v)))
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
    pub fn reserve(&mut self) -> usize {
        let key = self.next_free.replace(self.next_free.get() + 1);

        self.inner.push(None);

        key
    }

    #[inline]
    pub fn cancel_reserve<K>(&mut self, k: K) -> Result<(), ()>
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        let k = k.try_into().unwrap();

        if (self.next_free.get() - k) == 1 {
            let slot = self.inner.get(k).unwrap();

            assert!(
                slot.is_none(),
                "can not cancel a reserved slot that has been filled."
            );

            self.inner.pop();
            self.next_free.replace(k);

            Ok(())
        } else {
            Err(())
        }
    }

    #[inline]
    pub fn try_set_value<K>(&mut self, k: K, v: impl Into<V>) -> Result<(), ()>
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
    pub fn insert<K, T>(&mut self, value: T) -> K
    where
        K: TryFrom<SSAKey>,
        <K as TryFrom<SSAKey>>::Error: Debug,
        T: Into<V>,
    {
        let value = value.try_into().unwrap();
        let key = self.next_free.replace(self.next_free.get() + 1);

        assert_eq!(key, self.inner.len());

        self.inner.push(Some(value));

        K::from(key.try_into().unwrap())
    }

    #[inline]
    pub fn get_or_insert<K>(&mut self, key: K, f: impl Fn() -> V) -> &mut V
    where
        K: TryInto<SSAKey> + TryFrom<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
        <K as TryFrom<SSAKey>>::Error: Debug,
    {
        let key = key.try_into().unwrap();

        let key = if self.get_raw(key).is_none() {
            let key: K = self.insert(f());
            key.try_into().unwrap()
        } else {
            key
        };

        self.get_raw_mut(key).unwrap()
    }

    #[inline]
    pub fn entry<K>(&mut self, value: impl Into<V>) -> K
    where
        V: PartialEq,
        K: TryInto<SSAKey> + TryFrom<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
        <K as TryFrom<SSAKey>>::Error: Debug,
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
    pub fn get<K>(&self, key: K) -> Option<&V>
    where
        K: TryInto<SSAKey>,
        <K as TryInto<SSAKey>>::Error: Debug,
    {
        self.inner.get(key.try_into().unwrap())?.as_ref()
    }

    #[inline]
    pub fn get_mut<K>(&mut self, key: K) -> Option<&mut V>
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
