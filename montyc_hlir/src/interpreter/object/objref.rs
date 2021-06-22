use std::{cell::RefCell};

use crate::interpreter::{runtime::eval::ModuleExecutor};

use super::{ObjAllocId, PyObject, RawObject, dict::PyDictRaw};



//// An immutable reference to a `RefCell<RawObject>`
#[derive(Debug, Clone, Copy)]
pub(in crate::interpreter) struct PyObjectRef<'rt>(pub(in crate::interpreter) &'rt RefCell<RawObject>);

impl<'rt> PyObject<'rt> for PyObjectRef<'rt> {
    fn alloc_id(&self) -> ObjAllocId {
        self.0.borrow().alloc_id
    }

    fn class_id(&self, _: &'rt ModuleExecutor) -> ObjAllocId {
        self.0.borrow().__class__
    }

    fn with_dict<T>(&self, _: &'rt ModuleExecutor, f: impl Fn(&PyDictRaw) -> T) -> T {
        let dict = self.0.borrow();
        let dict = &dict.__dict__;
        f(dict)
    }
}
