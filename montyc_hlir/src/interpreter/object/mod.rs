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

use crate::value_store::{GlobalValueStore, ValueGraphIx};

use super::{
    runtime::{ceval::ConstEvalContext, SharedMutAnyObject},
    HashKeyT, PyResult, Runtime,
};

pub use alloc::ObjAllocId;

/// Used to handle conversions of PyObject's to Values
pub(crate) trait ToValue {
    fn contains(&self, store: &GlobalValueStore) -> Option<ValueGraphIx>;

    fn into_raw_value(&self, store: &GlobalValueStore) -> crate::Value;

    fn refine_value(
        &self,
        value: &mut crate::Value,
        store: &mut GlobalValueStore,
        value_ix: ValueGraphIx,
    );

    fn set_cache(&self, store: &mut GlobalValueStore, ix: ValueGraphIx);
}

/// An object safe base trait for all Python "objects".
pub(in crate::interpreter) trait PyObject:
    core::fmt::Debug + core::any::Any + 'static
{
    /// The allocation ID (`ObjAllocId`) for this particular object.
    fn alloc_id(&self) -> ObjAllocId;

    /// The allocation ID for `self.__class__`.
    fn class_alloc_id(&self, rt: &Runtime) -> ObjAllocId {
        if self.as_any().type_id() == core::any::TypeId::of::<ObjAllocId>() {
            rt.get_object(self.alloc_id())
                .expect("non-existent object for alloc id.")
                .class_alloc_id(rt)
        } else {
            unimplemented!("class_alloc_id: {:?}", self);
        }
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
