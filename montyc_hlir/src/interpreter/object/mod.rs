#[macro_use]
pub(in crate::interpreter) mod macros {
    macro_rules! object {
        {struct $name:ident { $( $field:ident : $kind:ty ),*  } $($ps:tt)*} => {
            #[derive(Debug)]
            pub(in crate::interpreter) struct $name {
                pub(in crate::interpreter) header: RawObject,
                $(
                    pub(in crate::interpreter) $field : $kind,
                )*
            }

            impl PyObject for $name {
                fn alloc_id(&self) -> super::alloc::ObjAllocId {
                    self.header.alloc_id
                }

                fn set_attribute_direct(
                    &mut self,
                    rt: &Runtime,
                    hash: HashKeyT,
                    key: super::alloc::ObjAllocId,
                    value: super::alloc::ObjAllocId,
                ) {
                    self.header.set_attribute_direct(rt, hash, key, value)
                }

                fn get_attribute_direct(
                    &self,
                    rt: &Runtime,
                    hash: HashKeyT,
                    key: super::alloc::ObjAllocId,
                ) -> Option<super::alloc::ObjAllocId> {
                    self.header.get_attribute_direct(rt, hash, key)
                }

                fn for_each(
                    &self,
                    rt: &mut ObjectGraph,
                    f: &mut dyn FnMut(&mut ObjectGraph, HashKeyT, ObjAllocId, ObjAllocId),
                ) {
                    self.header.for_each(rt, f)
                }

                fn as_any(&self) -> &dyn std::any::Any {
                    self
                }

                $($ps)*
            }
        };
    }
}

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
        None
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
        &self,
        _rt: &Runtime,
        _key: ObjAllocId,
        _value: ObjAllocId,
    ) -> Option<(ObjAllocId, ObjAllocId)> {
        None
    }

    /// Support for `obj[x]` or `obj.__getitem__(x)`
    fn get_item(&self, _rt: &Runtime, _key: ObjAllocId) -> Option<(ObjAllocId, ObjAllocId)> {
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

pub(in crate::interpreter) trait PyObjectEx: PyObject {}
