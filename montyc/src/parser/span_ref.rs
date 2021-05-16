use std::{
    collections::{hash_map::DefaultHasher, BTreeMap},
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    ops::Range,
};

use crate::{context::ModuleRef, ssamap::SSAMap};

use super::Span;

pub type SpanRef = NonZeroUsize;

pub(self) type SpanHash = u64;

// -- SpanRef

#[derive(Debug)]
pub struct SpanInterner {
    inner: SSAMap<usize, Range<usize>>,
    clobber_map: BTreeMap<SpanHash, (SpanRef, Vec<(SpanRef, ModuleRef)>)>,
    span_trace_map: BTreeMap<SpanRef, (ModuleRef, SpanHash)>,
}

impl Default for SpanInterner {
    fn default() -> Self {
        let mut inner = SSAMap::new();

        assert_eq!(inner.insert(usize::MAX..usize::MIN), 0);

        Self {
            inner,
            clobber_map: BTreeMap::new(),
            span_trace_map: BTreeMap::new(),
        }
    }
}

impl SpanInterner {
    #[inline]
    pub fn push_grouped(
        &mut self,
        value: Range<usize>,
        source: &str,
        mref: ModuleRef,
    ) -> (SpanRef, SpanRef) {
        let expected = source.get(value.clone()).unwrap();

        let hash = {
            let mut s = DefaultHasher::new();
            expected.hash(&mut s);
            s.finish()
        };

        if let Some((group_span, mut srefs)) = self.clobber_map.get_mut(&hash).cloned() {
            let sref = match srefs.iter().find(|(_, m)| *m == mref) {
                Some((sref, _)) => *sref,

                None => {
                    let sref = self.push(value);
                    srefs.push((sref, mref.clone()));
                    self.span_trace_map.insert(sref, (mref.clone(), hash));
                    sref
                }
            };

            (group_span, sref)
        } else {
            // Never before seen group, the spanref of the first entry is
            // the deciding spanref for the entire group.

            let group_span = self.push(value);

            self.clobber_map
                .insert(hash, (group_span, vec![((group_span, mref.clone()))]));

            self.span_trace_map.insert(group_span, (mref, hash));

            (group_span, group_span)
        }
    }

    #[inline]
    pub fn get_group(&self, group: SpanRef) -> Option<&[(NonZeroUsize, ModuleRef)]> {
        let (_, hash) = self.span_trace_map.get(&group)?;
        self.clobber_map.get(hash).as_ref().map(|(_, v)| v.as_slice())
    }

    #[inline]
    pub fn crosspan_eq(&self, a: SpanRef, b: SpanRef) -> bool {
        if let Some((a_t, b_t)) = self
            .span_trace_map
            .get(&a)
            .and_then(|a_t| self.span_trace_map.get(&b).map(|b_t| (a_t, b_t)))
        {
            // spans come from potentially separate modules (compare the hashes)
            a_t.1 == b_t.1
        } else {
            // not all spans are traced so just compare them directly
            a == b
        }
    }

    #[inline]
    pub(super) fn push(&mut self, value: Range<usize>) -> SpanRef {
        let key = self.inner.insert(value);
        assert_ne!(key, 0);
        unsafe { NonZeroUsize::new_unchecked(key) }
    }

    #[inline]
    pub fn get_traced<'a>(&self, reference: SpanRef) -> Option<(Span, ModuleRef)> {
        let (mref, _) = self.span_trace_map.get(&reference)?;
        let span = self.get(reference)?;

        Some((span, mref.clone()))
    }

    #[inline]
    pub fn get<'a>(&self, reference: SpanRef) -> Option<Span> {
        self.inner
            .get(usize::from(reference))
            .cloned()
    }

    #[inline]
    pub fn find(&self, st: impl AsRef<str>) -> Option<SpanRef> {
        let st = st.as_ref();
        let hash = {
            let mut s = DefaultHasher::new();
            st.hash(&mut s);
            s.finish()
        };

        self.clobber_map.get(&hash).map(|(gv, _)| *gv)
    }
}
