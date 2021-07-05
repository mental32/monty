

use crate::interpreter::{HashKeyT, Runtime};

use super::{PyObject, RawObject, alloc::ObjAllocId};


#[derive(Debug)]
pub(in crate::interpreter) struct ClassObj {
    pub header: RawObject,
}

impl PyObject for ClassObj {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.header.set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.header.get_attribute_direct(rt, hash, key)
    }

    fn for_each(&self, rt: &Runtime, f: &mut dyn FnMut(&Runtime, HashKeyT, ObjAllocId, ObjAllocId)) {
        self.header.for_each(rt, f)
    }

    fn into_value(&self, rt: &Runtime, object_graph: &mut crate::ObjectGraph) -> crate::Value {
        let properties = self.header.into_value_dict(rt, object_graph);

        crate::Value::Class {
            name: self.get_attribute_direct(rt, rt.hash("__name__"), self.alloc_id())
                .map(|obj| rt.try_as_str_value(obj))
                .unwrap()
                .unwrap(),

            properties,
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
