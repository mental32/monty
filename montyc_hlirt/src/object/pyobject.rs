use crate::{
    eval::ctx::EvalGlue,
    exception::{PyException, PyResult, PyResultExt},
    ObjectId,
};

#[allow(unused_variables)]
pub trait PyObject: std::fmt::Debug {
    /// the `std::any::TypeId` of the implementing type.
    ///
    /// Sadly I can not provide a default impl because I need to use this
    /// from `dyn PyObject`s so its up to the implementor to fill in the blank.
    ///
    /// # Safety
    ///
    /// The body of this function should **NEVER** be anything other than:
    ///
    /// ```rs
    /// std::any::TypeId::of::<Self>() // or self.type_id()
    /// ```
    ///
    unsafe fn std_type_id(&self) -> std::any::TypeId;

    fn call_method(
        &self,
        ecx: &mut dyn EvalGlue,
        name: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let meth = self.get_attribute(ecx, name).trace()?;

        ecx.call_object(meth, args).trace()
    }

    fn set_attribute(
        &self,
        ecx: &mut dyn EvalGlue,
        attr: ObjectId,
        value: ObjectId,
    ) -> PyResult<ObjectId> {
        PyException::not_implemented_error()
            .set_message("NotImplementedError: set_attribute is not implemented for this object.")
            .into()
    }

    fn get_attribute(&self, ecx: &mut dyn EvalGlue, attr: ObjectId) -> PyResult<ObjectId> {
        PyException::attribute_error(attr, 0).into()
    }

    fn call(&self, cx: crate::eval::ctx::CallCx) -> PyResult<ObjectId> {
        PyException::type_error()
            .set_message("TypeError: object is not callable.")
            .into()
    }

    fn hash(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
        PyException::not_implemented_error().into()
    }

    fn repr(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
        let repr = format!("<object @ '{:x?}'>", self as *const _);

        ecx.new_string(&repr)
    }
}
