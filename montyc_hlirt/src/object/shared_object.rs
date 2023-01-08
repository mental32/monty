use std::rc::Rc;

use crate::{
    eval::ctx::EvalGlue,
    exception::{PyResult, PyResultExt},
    ObjectId,
};

use super::PyObject;

#[derive(Debug, Clone)]
pub struct SharedObject {
    object: Rc<dyn PyObject>,
}

impl SharedObject {
    pub fn new<T>(object: T) -> Self
    where
        T: PyObject + 'static,
    {
        Self {
            object: Rc::new(object),
        }
    }

    pub fn try_downcast_ref<'a, T: 'static>(&'a self) -> Option<&'a T> {
        // SAFETY: its not.
        let object_type_id = unsafe { self.object.std_type_id() };

        if std::any::TypeId::of::<T>() == object_type_id {
            let inner = Rc::as_ptr(&self.object);

            unsafe { Some(&*(inner as *const T)) }
        } else {
            None
        }
    }

    fn with<T>(&self, f: impl FnOnce(&dyn PyObject) -> T) -> T {
        f(self.object.as_ref())
    }
}

impl PyObject for SharedObject {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        self.with(|o| o.std_type_id())
    }

    fn repr(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
        self.with(|o| o.repr(ecx))
    }

    fn call(&self, cx: crate::eval::ctx::CallCx) -> PyResult<ObjectId> {
        self.with(|o| o.call(cx))
    }

    fn set_attribute(
        &self,
        ecx: &mut dyn EvalGlue,
        attr: ObjectId,
        value: ObjectId,
    ) -> PyResult<ObjectId> {
        self.with(|o| o.set_attribute(ecx, attr, value))
    }

    fn get_attribute(&self, ecx: &mut dyn EvalGlue, attr: ObjectId) -> PyResult<ObjectId> {
        self.with(|o| o.get_attribute(ecx, attr))
    }

    fn call_method(
        &self,
        ecx: &mut dyn EvalGlue,
        name: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let __repr__ = ecx.new_string("__repr__")?;

        if name == __repr__ {
            return self.repr(ecx).trace();
        }

        self.with(|this| this.call_method(ecx, name, args)).trace()
    }
}
