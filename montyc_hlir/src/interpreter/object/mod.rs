pub mod alloc;
pub mod class;
pub mod dict;
pub mod frame;
pub mod func;
pub mod int;
pub mod raw;
pub mod string;

use montyc_core::utils::SSAMap;
pub(in crate::interpreter) use raw::RawObject;

pub use dict::PyDictRaw;

use crate::{ObjectGraph, ObjectGraphIndex};

use super::{
    runtime::{ceval::ConstEvalContext, SharedMutAnyObject},
    HashKeyT, PyResult, Runtime,
};

pub use alloc::ObjAllocId;

/// An object safe base trait for all Python "objects".
pub(in crate::interpreter) trait PyObject:
    core::fmt::Debug + core::any::Any + 'static
{
    /// The allocation ID (`ObjAllocId`) for this particular object.
    fn alloc_id(&self) -> ObjAllocId;

    /// The allocation ID for `self.__class__`.
    fn class_alloc_id(&self, rt: &Runtime) -> ObjAllocId {
        rt.get_object(self.alloc_id())
            .expect("non-existent object for alloc id.")
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

    /// The objects native `__hash__` dunder, returns `None` if `__hash__ = NotImplemented`
    ///
    /// `hash(obj) == obj.__hash__() = type(obj).__hash__(obj)`
    ///
    #[inline]
    fn hash(&self, rt: &Runtime) -> Option<HashKeyT> {
        unimplemented!("hash on {:?}", rt.get_object(self.alloc_id()));

        // rt.get_object(self.alloc_id())
        //     .map(|obj| obj as *const _ as HashKeyT)
    }

    /// Iterate through this objects `__dict__` and call `f` with the hash, key, and value of every entry.
    fn for_each(
        &self,
        graph: &mut ObjectGraph,
        f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
    );

    /// Produce a `crate::Value` from this interpreter object.
    fn into_value(
        &self,
        graph: &mut ObjectGraph,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) -> ObjectGraphIndex;

    #[inline]
    fn properties_into_values(
        &self,
        graph: &mut ObjectGraph,
        properties: &mut PyDictRaw<(ObjectGraphIndex, ObjectGraphIndex)>,
        objects: &SSAMap<ObjAllocId, SharedMutAnyObject>,
    ) {
        self.for_each(graph, &mut |graph, hash, key, value| {
            let key = key.into_value(graph, objects);
            let value = value.into_value(graph, objects);

            properties.insert(hash, (key, value));
        });
    }

    /// Support for `obj[x] = y` or `obj.__setitem__(x, y)`
    fn set_item(
        &mut self,
        _rt: &Runtime,
        _key: ObjAllocId,
        _value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        None
    }

    /// Support for `obj[x]` or `obj.__getitem__(x)`
    fn get_item(&mut self, _rt: &Runtime, _key: ObjAllocId) -> Option<(ObjAllocId, ObjAllocId)> {
        None
    }

    fn call(&self, _ex: &mut ConstEvalContext, _args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
        todo!();
    }

    fn is_truthy(&self) -> PyResult<bool> {
        Ok(false)
    }

    fn as_any(&self) -> &dyn std::any::Any;
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
    //         .attr(ex, ex.str_to_spanref_hashed("__bases__"))?
    //         .alloc_id();

    //     return Ok(__bases__.contains(ex, other_klass));
    // }

    // #[inline]
    // fn hash(&self, ex: &'rt ModuleExecutor<'_, '_>) -> PyResult<super::HashKeyT> {
    //     if let Ok(__hash__) = self.attr(ex, ex.str_to_spanref_hashed("__hash__")) {
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
    //     if let Ok(attr) = self.attr(ex, ex.str_to_spanref_hashed("__setattr__")) {
    //         attr.call(ex, &[attrib, value])?;

    //         return Ok(());
    //     }

    //     let attr_hash = PyObjectRef(ex.runtime.objects.get(attrib).unwrap()).hash(ex)?;
    //     let object = ex.runtime.objects.get(self.alloc_id()).unwrap();

    //     let _ = object.borrow_mut().__dict__.insert(attr_hash, value);

    //     Ok(())
    // }
}
