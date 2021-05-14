use std::{
    collections::{hash_map::DefaultHasher, BTreeMap},
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    ops::Range,
};

use crate::context::ModuleRef;

use super::Span;

pub type SpanRef = NonZeroUsize;

pub(self) type SpanHash = u64;

// -- SpanRef

#[derive(Debug)]
pub struct SpanInterner {
    ptr: usize,
    seq: Vec<Range<usize>>,

    clobber_map: BTreeMap<SpanHash, (SpanRef, Vec<(SpanRef, ModuleRef)>)>,
    span_trace_map: BTreeMap<SpanRef, (ModuleRef, SpanHash)>,
}

impl Default for SpanInterner {
    fn default() -> Self {
        Self {
            ptr: 1,
            seq: vec![usize::MAX..usize::MIN],
            clobber_map: BTreeMap::new(),
            span_trace_map: BTreeMap::new(),
        }
    }
}

impl SpanInterner {
    #[inline]
    pub fn push_noclobber(
        &mut self,
        value: Range<usize>,
        source: &str,
        mref: ModuleRef,
    ) -> SpanRef {
        let expected = source.get(value.clone()).unwrap();

        let hash = {
            let mut s = DefaultHasher::new();
            expected.hash(&mut s);
            s.finish()
        };

        if let Some((clobber_span, mut srefs)) = self.clobber_map.get_mut(&hash).cloned() {
            if srefs.iter().find(|(_, m)| *m == mref).is_none() {
                let sref = self.push(value);
                srefs.push((sref, mref.clone()));
                self.span_trace_map.insert(sref, (mref.clone(), hash));
            }

            clobber_span
        } else {
            let clobber_span = self.push(value);

            self.clobber_map
                .insert(hash, (clobber_span, vec![((clobber_span, mref.clone()))]));
            self.span_trace_map.insert(clobber_span, (mref, hash));

            clobber_span
        }
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
        self.ptr += 1;
        let key = self.ptr;
        self.seq.push(value);

        // SAFETY: `Self` gets constructed with `self.ptr = 1`
        //         and `key` is `self.ptr + 1` so even if `self.ptr == 0`
        //         `key` would not be zero.
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
        self.seq
            .get(usize::from(reference).saturating_sub(1))
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
