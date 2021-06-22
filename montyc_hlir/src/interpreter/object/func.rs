use petgraph::graph::NodeIndex;

use crate::interpreter::runtime::eval::ModuleExecutor;

use super::{dict::PyDictRaw, ObjAllocId, PyObject, PyResult};

pub(in crate::interpreter) type NativeFn =
    for<'rt> fn(&'rt ModuleExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>;

enum Callable {
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
struct Function {
    alloc_id: ObjAllocId,
    class_id: ObjAllocId,
    __call__: Callable,
}

impl Function {
    pub fn native(alloc_id: ObjAllocId, class_id: ObjAllocId, f: NativeFn) -> Self {
        Self {
            alloc_id,
            class_id,
            __call__: Callable::Native(f),
        }
    }
}

impl<'rt> PyObject<'rt> for Function {
    fn alloc_id(&self) -> ObjAllocId {
        self.alloc_id
    }

    fn call(&self, ex: &'rt ModuleExecutor, args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
        match self.__call__ {
            Callable::Native(f) => f(ex, args),
            Callable::BoxedDyn(ref f) => f(ex, args),
            Callable::SourceDef(_) | Callable::Object(_) => todo!(),
        }
    }
}
