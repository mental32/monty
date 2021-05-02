use std::{num::NonZeroUsize, ops::Range};

pub type SpanEntry = Option<NonZeroUsize>;

// -- SpanRef

#[derive(Debug)]
pub struct SpanRef {
    ptr: usize,
    pub(super) seq: Vec<Range<usize>>,
}

impl Default for SpanRef {
    fn default() -> Self {
        Self {
            ptr: 1,
            seq: vec![usize::MAX..usize::MIN],
        }
    }
}

impl SpanRef {
    #[inline]
    pub fn push_noclobber(&mut self, value: Range<usize>, source: &str) -> SpanEntry {
        let expected = source.get(value.clone())?;

        if let Some(n) = self.find(expected, source) {
            return Some(n);
        }

        self.ptr += 1;
        let key = self.ptr;

        self.seq.push(value);

        NonZeroUsize::new(key)
    }

    #[inline]
    pub fn push(&mut self, value: Range<usize>) -> SpanEntry {
        self.ptr += 1;
        let key = self.ptr;
        self.seq.push(value);

        NonZeroUsize::new(key)
    }

    #[inline]
    pub fn resolve_ref<'a>(
        &self,
        reference: Option<NonZeroUsize>,
        source: &'a str,
    ) -> Option<&'a str> {
        let range = self
            .seq
            .get(usize::from(reference?).saturating_sub(1))?
            .clone();

        source.get(range)
    }

    #[inline]
    pub fn find(&self, needle: &str, haystack: &str) -> SpanEntry {
        let it = (1..self.ptr).zip(self.seq[1..].iter().cloned());

        for (idx, range) in it {
            if haystack.get(range).map(|st| st == needle).unwrap_or(false) {
                return NonZeroUsize::new(idx + 1);
            }
        }

        None
    }
}
