use montyc_core::dict::PyDictRaw;

use super::*;

/// Basic polymorphic object representation.
#[repr(C)]
#[derive(Debug, Default, Clone)]
pub struct RawObject {
    /// Every object is assigned a unique object allocation ID (`ObjAllocId`)
    pub alloc_id: ObjectId,

    /// This **the** `__dict__` slot, almost everything Python-centric gets stored here.
    pub __dict__: PyDictRaw<(ObjectId, ObjectId)>,

    /// The class of the object.
    pub __class__: ObjectId,
}

impl PyObject for RawObject {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<Self>()
    }
}
