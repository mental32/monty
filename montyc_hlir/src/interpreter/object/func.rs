use petgraph::graph::NodeIndex;

use crate::interpreter::runtime::eval::ModuleExecutor;

use super::{ObjAllocId, PyObject, PyResult, RawObject, dict::PyDictRaw};

pub(in crate::interpreter) type NativeFn =
    for<'rt> fn(&'rt ModuleExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>;

pub(in crate::interpreter) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&ModuleExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef(NodeIndex<u32>),
    Object(ObjAllocId),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct Function {
    pub(in crate::interpreter) header: RawObject,
    pub(in crate::interpreter) inner: Callable,
}

impl PyObject for Function {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(&mut self, rt: &crate::interpreter::Runtime, hash: crate::interpreter::HashKeyT, key: ObjAllocId, value: ObjAllocId) {
        self.header.set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(&self, rt: &crate::interpreter::Runtime, hash: crate::interpreter::HashKeyT, key: ObjAllocId) -> Option<ObjAllocId> {
        self.header.get_attribute_direct(rt, hash, key)
    }

    // fn call(&self, ex: &'rt ModuleExecutor, args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
    //     match self.__call__ {
    //         Callable::Native(f) => f(ex, args),
    //         Callable::BoxedDyn(ref f) => f(ex, args),
    //         Callable::SourceDef(_) | Callable::Object(_) => todo!(),
    //     }
    // }
}
