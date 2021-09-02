use std::cell::{RefCell, RefMut};
use std::hash::{BuildHasher, Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use ahash::{AHashMap, RandomState};
use montyc_core::{utils::SSAMap, ModuleRef, SpanData, SpanRef};

// -- struct RawSpanInterner;

#[derive(Debug)]
struct RawSpanInterner {
    map: SSAMap<u32, SpanData>,
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
#[derive(Debug, Clone)]
pub struct SpanInterner(Rc<RefCell<RawSpanInterner>>);

impl SpanInterner {
    /// Create a new SpanInterner instance.
    #[inline]
    pub fn new() -> Self {
        Self(Default::default())
    }

    /// Return the string slice of the span's string.
    #[inline]
    pub fn spanref_to_str<'a>(
        &self,
        sref: SpanRef,
        resolver: impl Fn(ModuleRef, Range<usize>) -> Option<&'a str>,
    ) -> Option<&'a str> {
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
