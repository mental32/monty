use ahash::AHashMap;
use montyc_core::utils::SSAMap;

use super::{PyObject, RawObject};
use crate::{
    interpreter::{runtime::SharedMutAnyObject, HashKeyT, Runtime},
    ObjAllocId, ObjectGraph,
};

pub type FrameLocals = AHashMap<u32, (HashKeyT, ObjAllocId, ObjAllocId)>;

object! {
    struct FrameObject {
        locals: FrameLocals
    }

    fn into_value(
        &self,
        rt: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> crate::ObjectGraphIndex {
        unimplemented!()
    }
}
