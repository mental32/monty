use ahash::AHashMap;
use montyc_core::utils::SSAMap;

use super::{PyObject, RawObject};
use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjAllocId,
};

pub type FrameLocals = AHashMap<u32, (HashKeyT, ObjAllocId, ObjAllocId)>;

object! {
    struct FrameObject {
        locals: FrameLocals
    }
}
