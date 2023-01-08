use std::cell::{RefCell, RefMut};
use std::hash::{BuildHasher, Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use ahash::{AHashMap, RandomState};
use montyc_core::{utils::SSAMap, ModuleRef, SpanData, SpanRef};

// -- struct RawSpanInterner;

#[derive(Debug)]
struct RawSpanInterner {
    map: SSAMap<SpanData>,
    groups: AHashMap<u64, u32>,
    ahash_rstate: RandomState,
}

impl Default for RawSpanInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl RawSpanInterner {
    fn new() -> Self {
        Self {
            map: SSAMap::new(),
            groups: AHashMap::new(),
            ahash_rstate: RandomState::default(),
        }
    }
}

// -- struct SpanInterner;

/// A strong reference to a SpanInterner instance.
#[derive(Debug, Clone, Default)]
pub struct SpanInterner(Rc<RefCell<RawSpanInterner>>);

impl SpanInterner {
    /// Create a new SpanInterner instance.
    #[inline]
    pub fn new() -> Self {
        Self(Default::default())
    }

    #[inline]
    pub fn span_data(&self, sref: SpanRef) -> Option<SpanData> {
        self.0.borrow().map.get(sref.distinct()).cloned()
    }

    #[inline]
    pub fn spangroup_of_str(&self, st: &str) -> Option<u32> {
        let hash = {
            let mut hasher = self.0.borrow().ahash_rstate.build_hasher();
            st.hash(&mut hasher);
            hasher.finish()
        };

        self.spangroup_of_hash(hash)
    }

    #[inline]
    pub fn spangroup_of_hash(&self, hash: u64) -> Option<u32> {
        self.0.borrow().groups.get(&hash).cloned()
    }

    #[inline]
    pub fn spanrefs_of_group<'a>(&self, group: u32) -> Option<impl Iterator<Item = SpanRef>> {
        let inner = self.0.borrow();
        let (hash, _) = inner.groups.iter().find(|(_, g)| **g == group)?;

        let it = inner
            .map
            .iter()
            .filter_map(|(idx, data)| (data.hash == *hash).then(|| (idx)))
            .collect::<Vec<_>>()
            .into_iter();

        let it = it.map(move |distinct| SpanRef::from((group, distinct as u32)));

        Some(it)
    }

    /// Return the string slice of the span's string.
    #[inline]
    pub fn spanref_to_str<'a, T>(
        &self,
        sref: SpanRef,
        resolver: impl Fn(ModuleRef, Range<usize>) -> Option<T>,
    ) -> Option<T> {
        let data = self.0.borrow().map.get(sref.distinct())?.clone();
        resolver(data.module, data.range)
    }

    /// Return a new span reference to the given string.
    #[inline]
    pub fn str_to_spanref<const N: u32>(&self, name: &str) -> Result<SpanRef, ()> {
        let mut bound = self.get(name, ModuleRef(N))?;
        Ok(bound.insert(0..name.len()))
    }

    /// Create a `BoundMutInterner` from contextual information and a mutable borrow of the interner.
    #[inline]
    pub fn get<'a, 'b>(
        &'b self,
        source: &'a str,
        module: ModuleRef,
    ) -> Result<BoundMutInterner<'a, 'b>, ()> {
        let inner = self.0.try_borrow_mut().map_err(|_| ())?;

        Ok(BoundMutInterner {
            source,
            inner,
            module,
        })
    }
}

// -- struct BoundMutInterner<'source, 'data>

#[derive(Debug)]
pub struct BoundMutInterner<'source, 'data> {
    source: &'source str,
    module: ModuleRef,
    inner: RefMut<'data, RawSpanInterner>,
}

impl<'source, 'data> BoundMutInterner<'source, 'data> {
    #[inline]
    pub fn insert(&mut self, range: Range<usize>) -> SpanRef {
        let data = self
            .source
            .get(range.clone())
            .expect("provided range does not map into bound source.");

        let hash = {
            let mut hasher = self.inner.ahash_rstate.build_hasher();
            data.hash(&mut hasher);
            hasher.finish()
        };

        let distinct = self.inner.map.insert(SpanData {
            range,
            hash,
            module: self.module,
        });

        let group = self.inner.groups.entry(hash).or_insert(distinct).clone();

        (group, distinct).into()
    }
}
