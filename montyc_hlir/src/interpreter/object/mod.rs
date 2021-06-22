pub mod alloc;
pub mod dict;
pub mod func;
pub mod objref;
pub mod raw {
    use std::any::Any;

    use super::{dict::PyDictRaw, ObjAllocId};

    /// Fundemental object representation.
    #[derive(Debug)]
    pub(in crate::interpreter) struct RawObject {
        /// Every object is assigned a unique object allocation ID (`ObjAllocId`)
        pub alloc_id: ObjAllocId,

        /// This **the** `__dict__` slot, almost everything Python-centric gets stored here.
        pub __dict__: PyDictRaw,

        /// The class of the object.
        pub __class__: ObjAllocId,

        /// A slot to hold some native backing type i.e. Vec<PyObject<'rt>> for lists or isize for integers.
        pub __impl__: Option<Box<dyn Any>>,
    }
}

pub(in crate::interpreter) use {objref::PyObjectRef, raw::RawObject};

use self::dict::PyDictRaw;

use super::{exception::PyException, runtime::eval::ModuleExecutor, ObjAllocId, PyResult, Runtime};

pub(in crate::interpreter) trait PyObject<'rt> {
    fn alloc_id(&self) -> ObjAllocId;

    fn class_id(&self, ex: &'rt ModuleExecutor<'_, '_>) -> ObjAllocId {
        ex.runtime.get_object(self.alloc_id()).unwrap().class_id(ex)
    }

    fn with_dict<T>(&self, ex: &'rt ModuleExecutor, f: impl Fn(&PyDictRaw) -> T) -> T {
        ex.runtime
            .get_object(self.alloc_id())
            .unwrap()
            .with_dict(ex, f)
    }

    #[inline]
    fn contains(&self, ex: &'rt ModuleExecutor, obj: impl PyObject<'rt>) -> bool {
        false
    }

    #[inline]
    fn call(&self, ex: &'rt ModuleExecutor, args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
        todo!();
    }

    #[inline]
    fn attr(&self, ex: &'rt ModuleExecutor, attr: impl Into<u64>) -> PyResult<PyObjectRef<'rt>> {
        let attr = attr.into();

        fn rec<'rt, T>(this: &T, ex: &'rt ModuleExecutor, attr: u64) -> Option<ObjAllocId>
        where
            T: PyObject<'rt> + ?Sized,
        {
            let objref = this.with_dict(ex, move |dict| dict.get(attr));

            match objref {
                Some(alloc) => return Some(alloc),
                None => {
                    let klass = ex.runtime.get_object(this.class_id(ex))?;

                    if klass.alloc_id() == this.alloc_id() {
                        return None;
                    }

                    return rec(&klass, ex, attr);
                }
            };
        }

        match rec(self, ex, attr) {
            Some(obj) => {
                let attr = ex.runtime.get_object(obj).unwrap();
                Ok(attr)
            }

            None => Err(PyException),
        }
    }

    #[inline]
    fn isinstance(&self, ex: &'rt ModuleExecutor, other_klass: ObjAllocId) -> PyResult<bool> {
        let klass = match ex.runtime.objects.get(self.class_id(ex)) {
            Some(objref) => PyObjectRef(objref),
            None => todo!("RuntimError"),
        };

        // TODO: use MRO instead of __bases__
        let __bases__ = klass
            .attr(ex, ex.name_to_spanref_hashed("__bases__"))?
            .alloc_id();

        return Ok(__bases__.contains(ex, other_klass));
    }

    #[inline]
    fn hash(&self, ex: &'rt ModuleExecutor<'_, '_>) -> PyResult<super::HashKeyT> {
        if let Ok(__hash__) = self.attr(ex, ex.name_to_spanref_hashed("__hash__")) {
            let obj = __hash__.call(ex, &[self.alloc_id()])?;
            let number = ex.try_as_int_value(obj)?;

            Ok(number)
        } else {
            let addr = ex.runtime.objects.get(self.alloc_id()).unwrap() as *const _ as u64;
            Ok(addr)
        }
    }

    #[inline]
    fn setattr_static(&self, rt: &Runtime, attr: impl std::hash::Hash, value: ObjAllocId) {
        let obj = rt.objects.get(self.alloc_id()).unwrap();
        obj.borrow_mut().__dict__.insert(rt.hash(attr), value);
    }

    #[inline]
    fn getattr_static(&self, rt: &Runtime, key: impl std::hash::Hash) -> Option<ObjAllocId> {
        let obj = rt.objects.get(self.alloc_id()).unwrap();
        obj.borrow().__dict__.get(rt.hash(key))
    }

    #[inline]
    fn setattr(
        &self,
        ex: &'rt ModuleExecutor<'_, '_>,
        attrib: ObjAllocId,
        value: ObjAllocId,
    ) -> PyResult<()> {
        if let Ok(attr) = self.attr(ex, ex.name_to_spanref_hashed("__setattr__")) {
            attr.call(ex, &[attrib, value])?;

            return Ok(());
        }

        let attr_hash = PyObjectRef(ex.runtime.objects.get(attrib).unwrap()).hash(ex)?;
        let object = ex.runtime.objects.get(self.alloc_id()).unwrap();

        let _ = object.borrow_mut().__dict__.insert(attr_hash, value);

        Ok(())
    }
}
