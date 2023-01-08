use ahash::AHashMap;
use montyc_core::ModuleRef;

use crate::object::ObjectId;

// -- FrameState

type SpanGroup = u32;

type FrameValues = AHashMap<usize, ObjectId>;
type FrameLocals = AHashMap<SpanGroup /* group of SpanRef */, ObjectId>;

#[derive(Debug, Default)]
pub struct FrameState {
    /// The underlying namespace object.
    pub(super) frame_object: Option<ObjectId>,

    /// A value which is read-only when inside of an `InstExec` handler but gets written to in the main driver.
    pub(super) current_inst_ix: usize,

    // Result values produced by the flatcode.
    pub(super) values: FrameValues,

    // Frame locals.
    pub(super) locals: FrameLocals,

    /// The semantic module of the current frame, used to calculate imports.
    pub(super) mref: Option<ModuleRef>,
}

impl FrameState {
    pub fn new(frame_object: Option<ObjectId>, mref: Option<ModuleRef>) -> Self {
        Self {
            frame_object,
            mref,
            ..Default::default()
        }
    }

    #[inline]
    pub fn next_inst(&self) -> usize {
        self.current_inst_ix.saturating_add(1)
    }
}
