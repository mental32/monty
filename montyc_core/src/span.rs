use std::ops::Range;

#[derive(Debug, Clone)]
pub struct SpanData {
    pub range: Range<usize>,
    pub module: crate::module::ModuleRef,
    pub hash: u64,
}

pub use montyc_lexer::SpanRef;
