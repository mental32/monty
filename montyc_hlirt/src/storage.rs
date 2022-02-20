use std::sync::atomic::{AtomicU64, Ordering};

use ahash::RandomState;
use dashmap::DashMap;

use crate::{object::PyValue, ObjectId};

#[derive(Debug)]
pub struct DefaultObjectSpace {
    last_object_id: AtomicU64,
    inner: DashMap<ObjectId, PyValue, RandomState>,
}

impl DefaultObjectSpace {
    pub fn new() -> Self {
        Self {
            last_object_id: 1.into(),
            inner: Default::default(),
        }
    }
}

pub trait ObjectSpace {
    /// Generate a new, unique, unassociated ObjectId.
    fn new_object_id(&self) -> ObjectId;

    /// Insert a PyValue and get back its ObjectId.
    fn insert(&self, value: PyValue) -> ObjectId;

    /// Insert a PyValue from the constructor function.
    fn insert_with(&self, f: impl FnOnce(ObjectId) -> PyValue) -> ObjectId;

    /// Invoke the provided FnMut with a reference to the PyValue associated with the given ObjectId.
    fn with_object<T>(&self, object: ObjectId, f: impl FnMut(&PyValue) -> T) -> T;

    /// Invoke the provided FnMut with a **mutable** reference to the PyValue associated with the given ObjectId.
    fn with_object_mut<T>(&self, object: ObjectId, f: impl FnOnce(&mut PyValue) -> T) -> T;

    /// The amount of objects in this space.
    fn size_hint(&self) -> Option<usize>;

    /// Invoke the provided filter `f` with every object this space has until it returns `Some(T)`.
    fn filter_map<T>(&self, f: impl FnMut(ObjectId, &PyValue) -> Option<T>) -> Option<T>;

    /// Invoke the provided function `f` with every object this space has until.
    fn for_each(&self, f: impl FnMut(ObjectId, &PyValue));
}

impl ObjectSpace for DefaultObjectSpace {
    fn new_object_id(&self) -> ObjectId {
        let id = self.last_object_id.fetch_add(1, Ordering::SeqCst);

        ObjectId::from(id)
    }

    fn insert(&self, value: PyValue) -> ObjectId {
        let alloc = self.new_object_id();
        self.inner.insert(alloc, value);
        alloc
    }

    fn insert_with(&self, f: impl FnOnce(ObjectId) -> PyValue) -> ObjectId {
        let alloc = self.new_object_id();
        let val = f(alloc);

        self.inner.insert(alloc, val);

        alloc
    }

    #[track_caller]
    fn with_object<T>(&self, object: ObjectId, mut f: impl FnMut(&PyValue) -> T) -> T {
        let entry = self.inner.get(&object).expect("object does not exist.");
        let value = entry.value();

        f(value)
    }

    fn with_object_mut<T>(&self, object: ObjectId, f: impl FnOnce(&mut PyValue) -> T) -> T {
        let mut entry = self.inner.get_mut(&object).unwrap();
        let value = entry.value_mut();
        f(value)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.inner.len())
    }

    fn filter_map<T>(&self, mut f: impl FnMut(ObjectId, &PyValue) -> Option<T>) -> Option<T> {
        for kv in self.inner.iter() {
            if let Some(t) = f(kv.key().clone(), kv.value()) {
                return Some(t);
            }
        }

        None
    }

    fn for_each(&self, mut f: impl FnMut(ObjectId, &PyValue)) {
        for kv in self.inner.iter() {
            f(kv.key().clone(), kv.value());
        }
    }
}
