use std::alloc::Layout;
use std::any::TypeId;
use std::hash::Hash;

struct StorageCell {
    data: *const (),
    layout: Layout,
    type_id: TypeId,
}

impl StorageCell {
    pub fn new<T>(t: T) -> Self
    where
        T: 'static,
    {
        let t = Box::new(t);

        Self {
            data: Box::into_raw(t) as *const (),
            layout: Layout::array::<T>(1).unwrap(),
            type_id: TypeId::of::<T>(),
        }
    }
}

pub type QueryKey = TypeId;
pub type QueryInputHash = u128;

trait DynQueryInput {
    fn vtable_as_any(&self) -> &dyn std::any::Any;
    fn vtable_partial_eq(&self, other: &dyn std::any::Any) -> bool;
    fn vtable_hash(&self, state: &mut dyn std::hash::Hasher);
}

impl<T> DynQueryInput for T
where
    T: Hash + PartialEq<T> + 'static,
{
    #[inline]
    fn vtable_as_any(&self) -> &dyn std::any::Any {
        self
    }

    #[inline]
    fn vtable_partial_eq(&self, other: &dyn std::any::Any) -> bool {
        match other.downcast_ref::<T>() {
            Some(other) => self.eq(other),
            None => false,
        }
    }

    #[inline]
    fn vtable_hash(&self, state: &mut dyn std::hash::Hasher) {
        struct H<'a>(&'a mut dyn std::hash::Hasher);

        impl std::hash::Hasher for H<'_> {
            fn finish(&self) -> u64 {
                self.0.finish()
            }

            fn write(&mut self, bytes: &[u8]) {
                self.0.write(bytes)
            }
        }

        self.hash(&mut H(state))
    }
}

impl Hash for dyn DynQueryInput {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.vtable_hash(state)
    }
}

impl PartialEq for dyn DynQueryInput {
    fn eq(&self, other: &Self) -> bool {
        self.vtable_partial_eq(other.vtable_as_any())
    }
}

impl Eq for dyn DynQueryInput {}

type AHasher = std::hash::BuildHasherDefault<ahash::AHasher>;
type MapKey = (QueryKey, Box<dyn DynQueryInput>);
type MapKind = dashmap::DashMap<MapKey, StorageCell, AHasher>;

enum StorageImpl {
    Phantom,
    Real(MapKind),
}

impl StorageImpl {
    fn insert(&self, key: MapKey, value: StorageCell) {
        if let Self::Real(map) = self {
            let _ = map.insert(key, value);
        }
    }
}

pub struct Storage(StorageImpl);

impl Storage {
    pub const EMPTY: Self = Self(StorageImpl::Phantom);

    pub fn insert<Q, I, V>(&self, input: I, value: V)
    where
        Q: ?Sized + 'static,
        I: Hash + PartialEq<I> + 'static,
        V: 'static,
    {
        if let StorageImpl::Phantom = self.0 {
            return;
        }

        let query_input = Box::new(input) as Box<dyn DynQueryInput>;
        let key = (TypeId::of::<Q>(), query_input);

        self.0.insert(key, StorageCell::new(value));
    }
}
