pub mod alloc;
pub mod dict;
pub mod func;

pub mod string {
    use super::{alloc::ObjAllocId, PyObject, RawObject};

    #[derive(Debug)]
    pub(in crate::interpreter) struct StrObj {
        pub(in crate::interpreter) header: RawObject,
        pub(in crate::interpreter) value: String,
    }

    impl PyObject for StrObj {
        fn alloc_id(&self) -> super::alloc::ObjAllocId {
            self.header.alloc_id
        }

        fn set_attribute_direct(
            &mut self,
            rt: &crate::interpreter::Runtime,
            hash: crate::interpreter::HashKeyT,
            key: super::alloc::ObjAllocId,
            value: super::alloc::ObjAllocId,
        ) {
            self.header.set_attribute_direct(rt, hash, key, value)
        }

        fn get_attribute_direct(
            &self,
            rt: &crate::interpreter::Runtime,
            hash: crate::interpreter::HashKeyT,
            key: super::alloc::ObjAllocId,
        ) -> Option<super::alloc::ObjAllocId> {
            self.header.get_attribute_direct(rt, hash, key)
        }
    }
}

pub mod int {
    use super::{alloc::ObjAllocId, PyObject, RawObject};

    #[derive(Debug)]
    pub(in crate::interpreter) struct IntObj {
        pub(in crate::interpreter) header: RawObject,
        pub(in crate::interpreter) value: i64,
    }

    impl PyObject for IntObj {
        fn alloc_id(&self) -> super::alloc::ObjAllocId {
            self.header.alloc_id
        }

        fn set_attribute_direct(
            &mut self,
            rt: &crate::interpreter::Runtime,
            hash: crate::interpreter::HashKeyT,
            key: super::alloc::ObjAllocId,
            value: super::alloc::ObjAllocId,
        ) {
            self.header.set_attribute_direct(rt, hash, key, value)
        }

        fn get_attribute_direct(
            &self,
            rt: &crate::interpreter::Runtime,
            hash: crate::interpreter::HashKeyT,
            key: super::alloc::ObjAllocId,
        ) -> Option<super::alloc::ObjAllocId> {
            self.header.get_attribute_direct(rt, hash, key)
        }
    }
}

pub mod raw {
    use std::cell::RefCell;

    use super::{dict::PyDictRaw, ObjAllocId, PyObject};

    /// Fundemental object representation.
    #[derive(Debug)]
    pub(in crate::interpreter) struct RawObject {
        /// Every object is assigned a unique object allocation ID (`ObjAllocId`)
        pub alloc_id: ObjAllocId,

        /// This **the** `__dict__` slot, almost everything Python-centric gets stored here.
        pub __dict__: PyDictRaw<ObjAllocId>,

        /// The class of the object.
        pub __class__: ObjAllocId,
    }

    impl From<RawObject> for RefCell<Box<dyn PyObject>> {
        fn from(raw: RawObject) -> Self {
            RefCell::new(Box::new(raw) as _)
        }
    }

    impl PyObject for RawObject {
        fn alloc_id(&self) -> ObjAllocId {
            self.alloc_id
        }

        fn set_attribute_direct(
            &mut self,
            rt: &crate::interpreter::Runtime,
            hash: crate::interpreter::HashKeyT,
            key: ObjAllocId,
            value: ObjAllocId,
        ) {
            self.__dict__.insert(hash, value);
        }

        fn get_attribute_direct(
            &self,
            rt: &crate::interpreter::Runtime,
            hash: crate::interpreter::HashKeyT,
            key: ObjAllocId,
        ) -> Option<ObjAllocId> {
            self.__dict__.get(hash)
        }
    }
}

pub(in crate::interpreter) use raw::RawObject;

pub use dict::PyDictRaw;

use super::{exception::PyException, runtime::eval::ModuleExecutor, ObjAllocId, PyResult, Runtime};

/// An object safe base trait for all Python "objects".
pub(in crate::interpreter) trait PyObject: core::fmt::Debug + core::any::Any + 'static {
    /// The allocation ID (`ObjAllocId`) for this particular object.
    fn alloc_id(&self) -> ObjAllocId;

    /// The allocation ID for `self.__class__`.
    fn class_alloc_id(&self, rt: &Runtime) -> ObjAllocId {
        rt.get_object(self.alloc_id())
            .expect("non-existent object for alloc id.")
            .borrow()
            .class_alloc_id(rt)
    }

    /// `self.{key} = {value}` with `hash` being a separately-computed result of `key.__hash__()`.
    ///
    /// This should only operate on the underlying object directly and should not call other python methods.
    ///
    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: super::HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    );

    /// `getattr(self, key, None)` with `hash` being a separately-computed result of `key.__hash__()`.
    ///
    /// This should only operate on the underlying object directly and should not call other python methods.
    ///
    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: super::HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId>;
}

pub(in crate::interpreter) trait PyObjectEx: PyObject {
    // fn with_dict<T>(&self, ex: &'rt ModuleExecutor, f: impl Fn(&PyDictRaw<ObjAllocId>) -> T) -> T
    // where
    //     Self: Sized,
    // {
    //     ex.runtime
    //         .get_object(self.alloc_id())
    //         .unwrap()
    //         .with_dict(ex, f)
    // }

    // #[inline]
    // fn contains(&self, ex: &'rt ModuleExecutor, obj: &dyn PyObject) -> bool {
    //     false
    // }

    // #[inline]
    // fn call(&self, ex: &'rt ModuleExecutor, args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
    //     todo!();
    // }

    // #[inline]
    // fn attr(&self, ex: &'rt ModuleExecutor, attr: u64) -> PyResult<PyObjectRef<'rt>> {
    //     fn rec<'rt, T>(this: &T, ex: &'rt ModuleExecutor, attr: u64) -> Option<ObjAllocId>
    //     where
    //         T: PyObject + ?Sized,
    //     {
    //         let objref = this.with_dict(ex, move |dict| dict.get(attr));

    //         match objref {
    //             Some(alloc) => return Some(alloc),
    //             None => {
    //                 let klass = ex.runtime.get_object(this.class_id(ex))?;

    //                 if klass.alloc_id() == this.alloc_id() {
    //                     return None;
    //                 }

    //                 return rec(&klass, ex, attr);
    //             }
    //         };
    //     }

    //     match rec(self, ex, attr) {
    //         Some(obj) => {
    //             let attr = ex.runtime.get_object(obj).unwrap();
    //             Ok(attr)
    //         }

    //         None => Err(PyException),
    //     }
    // }

    // #[inline]
    // fn isinstance(&self, ex: &'rt ModuleExecutor, other_klass: ObjAllocId) -> PyResult<bool> {
    //     let klass = match ex.runtime.objects.get(self.class_id(ex)) {
    //         Some(objref) => PyObjectRef(objref),
    //         None => todo!("RuntimError"),
    //     };

    //     // TODO: use MRO instead of __bases__
    //     let __bases__ = klass
    //         .attr(ex, ex.name_to_spanref_hashed("__bases__"))?
    //         .alloc_id();

    //     return Ok(__bases__.contains(ex, other_klass));
    // }

    // #[inline]
    // fn hash(&self, ex: &'rt ModuleExecutor<'_, '_>) -> PyResult<super::HashKeyT> {
    //     if let Ok(__hash__) = self.attr(ex, ex.name_to_spanref_hashed("__hash__")) {
    //         let obj = __hash__.call(ex, &[self.alloc_id()])?;
    //         let number = ex.try_as_int_value(obj)?;

    //         Ok(number)
    //     } else {
    //         let addr = ex.runtime.objects.get(self.alloc_id()).unwrap() as *const _ as u64;
    //         Ok(addr)
    //     }
    // }

    // #[inline]
    // fn setattr_static(&self, rt: &Runtime, attr: impl std::hash::Hash, value: ObjAllocId)
    // where
    //     Self: Sized,
    // {
    //     let obj = rt.objects.get(self.alloc_id()).unwrap();
    //     obj.borrow_mut().__dict__.insert(rt.hash(attr), value);
    // }

    // #[inline]
    // fn getattr_static(&self, rt: &Runtime, key: impl std::hash::Hash) -> Option<ObjAllocId>
    // where
    //     Self: Sized,
    // {
    //     let obj = rt.objects.get(self.alloc_id()).unwrap();
    //     obj.borrow().__dict__.get(rt.hash(key))
    // }

    // #[inline]
    // fn setattr(
    //     &self,
    //     ex: &'rt ModuleExecutor<'_, '_>,
    //     attrib: ObjAllocId,
    //     value: ObjAllocId,
    // ) -> PyResult<()> {
    //     if let Ok(attr) = self.attr(ex, ex.name_to_spanref_hashed("__setattr__")) {
    //         attr.call(ex, &[attrib, value])?;

    //         return Ok(());
    //     }

    //     let attr_hash = PyObjectRef(ex.runtime.objects.get(attrib).unwrap()).hash(ex)?;
    //     let object = ex.runtime.objects.get(self.alloc_id()).unwrap();

    //     let _ = object.borrow_mut().__dict__.insert(attr_hash, value);

    //     Ok(())
    // }
}
