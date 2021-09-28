use montyc_core::{dict::PyDictRaw, MapT, CLASS, MODULE, OBJECT};

use crate::{
    exception::PyResult,
    object::RawObject,
    rt::{ModuleKey, Runtime},
    storage::ObjectSpace,
    ObjectId,
};

use super::{sealed::IntoAnyFunc, AnyFunc, IntoPyValue, PyValue};

#[derive(Debug)]
pub enum ObjectType {
    Module,
    Class(Option<ObjectId>),
    Any,
}

impl Default for ObjectType {
    fn default() -> Self {
        Self::Any
    }
}

#[derive(Debug, Default)]
pub struct PyDataModel {
    __repr__: Option<()>,
    __call__: Option<()>,
}

#[derive(Debug, Default)]
pub struct ObjectBuilder<const OB_TYPE: usize> {
    pub(crate) properties: MapT<Box<str>, PyValue>,
}

impl ObjectBuilder<{ MODULE }> {
    pub fn module() -> Self {
        Self::default()
    }
}

impl ObjectBuilder<{ OBJECT }> {
    pub fn object() -> Self {
        Self::default()
    }
}

impl ObjectBuilder<{ CLASS }> {
    pub fn class() -> Self {
        Self::default()
    }
}

impl<const OB_TYPE: usize> ObjectBuilder<{ OB_TYPE }> {
    pub fn new() -> ObjectBuilder<{ OB_TYPE }> {
        ObjectBuilder::<{ OB_TYPE }>::default()
    }

    pub fn hasattr<S>(&self, name: S) -> bool
    where
        S: AsRef<str>,
    {
        self.properties.contains_key(name.as_ref())
    }

    pub fn setattr<O, S>(mut self, name: S, object: O) -> Self
    where
        O: IntoPyValue,
        S: ToString,
    {
        let name = name.to_string().into_boxed_str();
        let object = object.into_py_val();

        self.properties.insert(name, object);

        self
    }

    fn properties_into_dict(self, rt: &mut Runtime) -> PyDictRaw<(ObjectId, ObjectId)> {
        let Self { mut properties } = self;

        let mut __dict__: PyDictRaw<(ObjectId, ObjectId)> =
            MapT::with_capacity(properties.len()).into();

        for (key, value) in properties.drain() {
            let hash = rt.hash(&key);

            let key_as_str = rt.objects.insert(key.clone().into_py_val());
            let value_as_obj = rt.objects.insert(value);

            log::trace!(
                "[ObjectBuilder::properties_into_dict] __dict__[{:?} ({:?})] = {:?}",
                key,
                key_as_str,
                value_as_obj
            );

            let _ = __dict__.insert(hash, (key_as_str, value_as_obj));
        }

        __dict__
    }
}

impl ObjectBuilder<{ OBJECT }> {
    pub fn synthesise_within(self, rt: &mut Runtime) -> PyResult<ObjectId> {
        log::trace!("[ObjectBuilder::synthesise_within] synthesizing an `any` object");

        let __dict__ = self.properties_into_dict(rt);
        let __class__ = rt.singletons.object_class;
        let object = rt.objects.insert_with(|alloc_id| {
            PyValue::Any(RawObject {
                alloc_id,
                __dict__,
                __class__,
            })
        });

        Ok(object)
    }
}

impl ObjectBuilder<{ CLASS }> {
    pub fn synthesise_within(self, rt: &mut Runtime) -> PyResult<ObjectId> {
        log::trace!("[ObjectBuilder::synthesise_within] synthesizing a class object");

        let __dict__ = self.properties_into_dict(rt);
        let __class__ = rt.singletons.type_class;
        let class = rt.objects.insert_with(|alloc_id| PyValue::Class {
            name: None,
            parent: None,
            inner: RawObject {
                alloc_id,
                __class__,
                __dict__,
            },
        });

        Ok(class)
    }
}

impl ObjectBuilder<{ MODULE }> {
    pub fn synthesise_within(self, rt: &mut Runtime) -> PyResult<ObjectId> {
        log::trace!("[ObjectBuilder::synthesise_within] synthesizing a module object");

        let __dict__ = self.properties_into_dict(rt);
        let __class__ = rt.singletons.module_class;
        let module = rt.objects.insert_with(|alloc_id| PyValue::Module {
            mkey: ModuleKey::Object(alloc_id),
            inner: RawObject {
                alloc_id,
                __dict__,
                __class__,
            },
        });

        Ok(module)
    }
}

pub const READY: usize = 0;
pub type ReadyCallable = CallableBuilder<{ crate::object::READY }>;

impl<T> From<T> for ReadyCallable
where
    T: IntoAnyFunc,
{
    fn from(t: T) -> Self {
        Self::Ready(t.into_any_func())
    }
}

#[derive(Debug)]
pub enum CallableBuilder<const STATE: usize> {
    Ready(AnyFunc),
    Building { parameters: Vec<String> },
}

#[cfg(test)]
mod test {
    use montyc_core::ModuleRef;

    use crate::{eval::ctx::CallCx, object::NativeFn, storage::ObjectSpace, test::setup};

    use super::*;

    #[test]
    fn native_identity_callable() {
        let (mut rt, mut host) = setup();

        let identity =
            ReadyCallable::from(|cx: CallCx| Ok(cx.ecx.runtime_mut().new_list(cx.args.to_owned())));

        let module = ObjectBuilder::<{ MODULE }>::new().setattr("f", identity);
        let _module = rt.synthesise_module(ModuleRef(42), module).unwrap();

        let result_module = rt
            .eval(&mut host, "a = f(1, 2, 3)")
            .unwrap()
            .from_module_import(ModuleRef(42), ["f"])
            .run_until_complete()
            .unwrap();

        let a_hash = rt.hash("a");
        let (_a_str, a_val) = rt
            .objects
            .with_object(result_module, move |this| match this {
                PyValue::Module { inner, .. } => inner.__dict__.get(a_hash),
                _ => unreachable!(),
            })
            .unwrap();

        let one_two_three = [rt.new_int(1), rt.new_int(2), rt.new_int(3)];

        rt.objects.with_object(a_val, |this| match this {
            PyValue::List(elems) => assert_eq!(elems.as_slice(), &one_two_three),
            _ => unreachable!(),
        });
    }

    #[test]
    fn build_simple_module() {
        let (mut rt, mut _host) = setup();

        let module = ObjectBuilder::<{ MODULE }>::new()
            .setattr("foo", "bar".to_string())
            .setattr("pie", 3.14_f64)
            .setattr("fuzz", { |_| Ok(()) } as NativeFn<()>);

        assert!(module.hasattr("foo"));
        assert!(module.hasattr("pie"));
        assert!(module.hasattr("fuzz"));

        rt.synthesise_module(ModuleRef(0), module).unwrap();
    }
}
