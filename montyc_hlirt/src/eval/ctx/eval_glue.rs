use crate::exception::{PyException, PyResult, PyResultExt};
use crate::object::{PyIter, PyObject, PyValue};
use crate::rt::{Runtime, RuntimeHost};
use crate::{storage::ObjectSpace, ObjectId};

mod sealed {
    use crate::{exception::PyResult, ObjectId};

    use super::EvalGlue;

    pub trait AnyAttr {
        fn to_object(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId>;

        fn to_hash(&self, ecx: &mut dyn EvalGlue) -> u64;
    }

    impl AnyAttr for String {
        fn to_object(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
            ecx.new_string(self)
        }

        fn to_hash(&self, ecx: &mut dyn EvalGlue) -> u64 {
            ecx.runtime_mut().hash(self)
        }
    }

    impl AnyAttr for ObjectId {
        fn to_object(&self, _ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
            Ok(*self)
        }

        fn to_hash(&self, ecx: &mut dyn EvalGlue) -> u64 {
            ecx.hash_object(*self).unwrap()
        }
    }

    pub trait Attrs {
        fn to_attrs<'a>(&self, ecx: &mut dyn EvalGlue) -> Vec<Box<dyn AnyAttr + 'a>>;
    }

    impl Attrs for &str {
        fn to_attrs<'a>(&self, _ecx: &mut dyn EvalGlue) -> Vec<Box<dyn AnyAttr + 'a>> {
            vec![Box::new(self.to_string())]
        }
    }

    impl<T, const N: usize> Attrs for [T; N]
    where
        T: AnyAttr + Clone + 'static,
    {
        fn to_attrs<'a>(&self, _ecx: &mut dyn EvalGlue) -> Vec<Box<dyn AnyAttr + 'a>> {
            self.iter()
                .cloned()
                .map(|p| Box::new(p) as Box<dyn AnyAttr>)
                .collect()
        }
    }
}

pub trait EvalGlue {
    fn self_as_dyn<'a>(&'a mut self) -> &'a mut dyn EvalGlue;

    fn runtime_mut(&mut self) -> &mut Runtime;
    fn runtime(&self) -> &Runtime;

    fn runtime_host_mut(&mut self) -> &mut dyn RuntimeHost;
    fn runtime_host(&self) -> &dyn RuntimeHost;

    #[track_caller]
    fn getattr_direct_hash(
        &mut self,
        object: ObjectId,
        hash: u64,
    ) -> PyResult<(ObjectId, ObjectId)> {
        self.runtime_mut().objects.with_object(object, |this| {
            let result = match this {
                PyValue::Module { inner, .. }
                | PyValue::Any(inner)
                | PyValue::Function { inner, .. }
                | PyValue::Class { inner, .. } => inner.__dict__.get(hash),

                PyValue::Dict(inner) => inner.get(hash),

                PyValue::Dynamic(_obj) => todo!(),
                PyValue::Int(_) => todo!(),
                PyValue::Float(_) => todo!(),
                PyValue::Bool(_) => todo!(),
                PyValue::Ellipsis => todo!(),
                PyValue::Bytes(_) => todo!(),
                PyValue::Str(_) => todo!(),
                PyValue::List(_) => todo!("list getattr"),
                PyValue::Callable(_) => todo!(),

                PyValue::None => None,
            };

            result.ok_or_else(|| PyException::attribute_error(object, hash))
        })
    }

    fn setattr(&mut self, object: ObjectId, key: ObjectId, value: ObjectId) -> PyResult<()> {
        let hash = self.hash_object(key)?;

        self.runtime_mut()
            .objects
            .with_object_mut(object, move |this| match this {
                PyValue::Module { inner, .. }
                | PyValue::Any(inner)
                | PyValue::Function { inner, .. }
                | PyValue::Class { inner, .. } => inner.__dict__.insert(hash, (key, value)),

                PyValue::Dynamic(_) => todo!(),
                PyValue::Int(_) => todo!(),
                PyValue::Float(_) => todo!(),
                PyValue::Bool(_) => todo!(),
                PyValue::None => todo!(),
                PyValue::Ellipsis => todo!(),
                PyValue::Bytes(_) => todo!(),
                PyValue::Str(_) => todo!(),

                PyValue::List(_) => todo!("list getattr"),

                PyValue::Dict(_) => todo!(),
                PyValue::Callable(_) => todo!(),
            });

        Ok(())
    }

    fn getattr(&mut self, base: ObjectId, attrs: &dyn sealed::Attrs) -> PyResult<ObjectId> {
        let mut object = base;

        // enum AttrAccessKind {
        //     HashLookup(ObjectId),
        //     DynamicCall(SharedObject),
        // }

        for attr in attrs.to_attrs(self.self_as_dyn()) {
            let as_shared = self
                .runtime_mut()
                .objects
                .with_object(object, |this| match this {
                    PyValue::Dynamic(n) => Some(n.clone()),
                    _ => None,
                });

            object = match as_shared {
                Some(obj) => {
                    let ecx = self.self_as_dyn();
                    let attr = attr.to_object(ecx).trace()?;

                    obj.get_attribute(ecx, attr).trace()?
                }

                None => {
                    let attr = attr.to_hash(self.self_as_dyn());

                    self.getattr_direct_hash(object, attr).trace()?.1
                }
            };
        }

        Ok(object)
    }

    fn new_string(&mut self, string: &str) -> PyResult<ObjectId> {
        Ok(self.runtime_mut().new_string(string))
    }

    fn new_int(&mut self, n: i64) -> PyResult<ObjectId> {
        Ok(self.runtime_mut().new_int(n))
    }

    fn call_method_object(
        &mut self,
        object: ObjectId,
        method: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId>;

    fn getattr_object(&mut self, object: ObjectId, attr: ObjectId) -> PyResult<ObjectId>;

    fn repr_object(&mut self, object: ObjectId) -> PyResult<ObjectId>;

    fn iter_object(&mut self, object: ObjectId) -> PyIter;

    fn hash_object(&mut self, object: ObjectId) -> PyResult<u64>;

    fn call_object(&mut self, callable: ObjectId, arguments: &[ObjectId]) -> PyResult<ObjectId>;
}
