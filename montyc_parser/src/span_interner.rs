use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::hash::{BuildHasher, Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use ahash::{AHashMap, RandomState};

use montyc_core::utils::SSAMap;
use montyc_lexer::SpanRef;

// -- struct SpanData

#[derive(Debug, Clone)]
pub struct SpanData<M> {
    pub range: Range<usize>,
    pub module: M,
    pub hash: u64,
}

// -- struct RawSpanInterner;

#[derive(Debug)]
struct RawSpanInterner<M> {
    map: SSAMap<SpanData<M>>,
    groups: AHashMap<u64, u32>,
    ahash_rstate: RandomState,
}

impl<M> Default for RawSpanInterner<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M> RawSpanInterner<M> {
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
#[derive(Debug, Default)]
pub struct SpanInterner<M>(Rc<RefCell<RawSpanInterner<M>>>);

impl<M> Clone for SpanInterner<M> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl<M> SpanInterner<M>
where
    M: Clone,
{
    /// Create a new SpanInterner instance.
    #[inline]
    pub fn new() -> Self {
        Self(Default::default())
    }

    #[inline]
    pub fn span_data(&self, sref: SpanRef) -> Option<SpanData<M>>
    where
        M: Clone,
    {
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
    pub fn spanref_to_str<'a>(
        &self,
        sref: SpanRef,
        resolver: impl Fn(M, Range<usize>) -> Option<Cow<'a, str>>,
    ) -> Option<Cow<'a, str>> {
        let data = self.0.borrow().map.get(sref.distinct())?.clone();
        resolver(data.module, data.range)
    }

    /// Return a new span reference to the given string.
    #[inline]
    pub fn str_to_spanref<const N: u32>(&self, name: &str) -> Result<SpanRef, ()>
    where
        M: From<u32>,
    {
        let mut bound = self.get(name, N.into())?;
        Ok(bound.insert(0..name.len()))
    }

    /// Create a `BoundMutInterner` from contextual information and a mutable borrow of the interner.
    #[inline]
    pub fn get<'a, 'b>(
        &'b self,
        source: &'a str,
        module: M,
    ) -> Result<BoundMutInterner<'a, 'b, M>, ()> {
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
pub struct BoundMutInterner<'source, 'data, M> {
    source: &'source str,
    module: M,
    inner: RefMut<'data, RawSpanInterner<M>>,
}

impl<'source, 'data, M> BoundMutInterner<'source, 'data, M>
where
    SpanRef: From<(u32, u32)>,
    M: Clone,
{
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
            module: self.module.clone(),
        });

        let group = self.inner.groups.entry(hash).or_insert(distinct).clone();

        (group, distinct).into()
    }
}
