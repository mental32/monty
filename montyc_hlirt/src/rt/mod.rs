use std::collections::hash_map::Entry;
use std::fs::DirEntry;
use std::hash::{BuildHasher, Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use montyc_core::{dict::PyDictRaw, ModuleRef, SpanRef};
use montyc_core::{MapT, MODULE};
use montyc_flatcode::FlatCode;

use crate::eval::ctx::{EvaluationContext, UnboundEvaluationContext};
use crate::exception::{PyException, PyResult, PyResultExt};
use crate::object::{AnyFunc, ObjectBuilder, ObjectId, PyValue};
use crate::object::{CallableBuilder, ReadyCallable};
use crate::rt::singletons::{DynamicSingleton, Singletons};
use crate::storage::{DefaultObjectSpace, ObjectSpace};

#[cfg(test)]
mod tests;

pub trait AcceptInput<I, O>
where
    Self: RuntimeHostExt,
{
    fn accept_input(&mut self, input: I) -> Result<O, Self::Error>;
}

pub trait RuntimeHost {
    /// Given a hash tell the runtime what the span group is, if none exists use the given string to create one.
    fn spangroup_of_hash(&mut self, hash: u64, st: &str) -> u32;

    /// Resolve a spanref to a string.
    fn spanref_to_str(&self, sref: SpanRef) -> &str;

    /// Resolve a span **group** to a string.
    fn spangroup_to_str(&self, group: u32) -> &str;

    /// Ask the host to read the contents of the file at the given path.
    fn try_read_file(&self, path: &Path) -> io::Result<String>;

    /// Ask the host to read the directory contents at the given path.
    fn try_read_directory(&self, path: &Path) -> io::Result<Box<[io::Result<DirEntry>]>>;

    /// Ask the host to get the current working directory.
    fn try_get_cwd(&self) -> io::Result<PathBuf>;
}

pub trait RuntimeHostExt: RuntimeHost {
    type Error: core::fmt::Debug;

    fn import_module_spec(
        &mut self,
        mref: ModuleRef,
        path: &[SpanRef],
        relative: usize,
    ) -> Box<dyn FnOnce(&mut EvaluationContext<'_, '_, Self>) -> PyResult<ObjectId>>
    where
        Self: Sized;

    fn as_accept_input<'a, 'b, 'c>(
        &'a mut self,
    ) -> &'b mut dyn AcceptInput<&'c str, FlatCode, Error = Self::Error>
    where
        'a: 'b,
        'b: 'c;
}

#[derive(Debug)]
pub enum RuntimeError<H>
where
    H: RuntimeHostExt,
{
    Host(H::Error),
    Py(PyException),
    ModuleAlreadyLoaded,
}

impl<H: RuntimeHostExt> From<PyException> for RuntimeError<H> {
    fn from(exc: PyException) -> Self {
        Self::Py(exc)
    }
}

#[derive(Debug)]
pub struct ModuleMetadata {
    pub alloc: ObjectId,
    pub mref: ModuleRef,
    pub code: Option<Rc<FlatCode>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, derive_more::From)]
pub enum ModuleKey {
    Static(&'static str),
    User(ModuleRef),
    Object(ObjectId),
}

pub mod singletons;

/// An interpreter runtime capable of executing Python.
#[derive(Debug)]
pub struct Runtime<Space: ObjectSpace = DefaultObjectSpace> {
    /// All of the objects get stored here.
    pub objects: Space,

    /// The hash state used within the runtime.
    pub(crate) hash_state: ahash::RandomState,

    /// Global singelton objects.
    pub singletons: Singletons,

    /// A map of module refs to their module objects.
    pub module_objects: MapT<ModuleRef, ModuleMetadata>,
}

impl<Space: ObjectSpace> Runtime<Space> {
    pub fn hash(&self, h: impl std::hash::Hash) -> u64 {
        let mut hasher = self.hash_state.build_hasher();

        h.hash(&mut hasher);

        hasher.finish()
    }

    pub fn make_kv_pair(
        &mut self,
        key: &str,
        ctor: impl FnOnce(&mut Self) -> ObjectId,
    ) -> (u64, ObjectId, ObjectId) {
        let hash = self.hash(key);
        let key = self.new_string(key);
        let value = ctor(self);

        (hash, key, value)
    }

    #[track_caller]
    pub fn class_of(&self, obj: ObjectId) -> ObjectId {
        log::trace!("[Runtime::class_of] {:?}", obj);

        let Singletons {
            function_class,
            module_class,
            string_class,
            bool_class,
            int_class,
            type_class,
            object_class,
            float_class,
            none_class,
            ellipsis_class,
            bytes_class,
            list_class,
            dict_class,
            ..
        } = &self.singletons;

        self.objects.with_object(obj, |this| match this {
            PyValue::Int(_) => *int_class,
            PyValue::Bool(_) => *bool_class,
            PyValue::None => *none_class,
            PyValue::Ellipsis => *ellipsis_class,
            PyValue::Str(_) => *string_class,
            PyValue::Float(_) => *float_class,
            PyValue::Function { .. } => *function_class,
            PyValue::Class { .. } => *type_class,
            PyValue::Module { .. } => *module_class,
            PyValue::Dynamic(_) => *object_class,
            PyValue::Any(raw) => raw.__class__,
            PyValue::Bytes(_) => *bytes_class,
            PyValue::List(_) => *list_class,
            PyValue::Dict(_) => *dict_class,
            PyValue::Callable(func) => match func {
                AnyFunc::Native { .. } | AnyFunc::Boxed { .. } => *function_class,
                AnyFunc::Code { .. } => *function_class,
            },
        })
    }
}

impl<Space: ObjectSpace> Runtime<Space> {
    pub fn new_string(&mut self, string: &str) -> ObjectId {
        let hash = self.hash(string);
        let key = DynamicSingleton::Str(hash);

        let Self { singletons, .. } = self;

        let key = if let Some(alloc) = singletons.dynamic.get(&key) {
            *alloc
        } else {
            let alloc = self
                .objects
                .insert(PyValue::Str(string.to_owned().into_boxed_str()));

            singletons.dynamic.insert(key, alloc);

            alloc
        };

        key
    }

    pub fn new_int(&mut self, n: i64) -> ObjectId {
        let Self {
            objects,
            singletons,
            ..
        } = self;

        let alloc = match singletons.dynamic.entry(DynamicSingleton::Int(n)) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(v) => {
                let alloc = objects.insert(PyValue::Int(n));

                v.insert(alloc);

                alloc
            }
        };

        alloc
    }

    pub fn new_callable<C>(&self, callable: C) -> ObjectId
    where
        C: Into<ReadyCallable>,
    {
        let val = match callable.into() {
            CallableBuilder::Ready(func) => PyValue::from(func),
            CallableBuilder::Building { .. } => unreachable!(),
        };

        self.objects.insert(val)
    }

    pub fn new_list<I>(&self, elems: I) -> ObjectId
    where
        I: IntoIterator<Item = ObjectId>,
    {
        let elems = elems.into_iter().collect::<Vec<_>>();
        let lst = self.objects.insert(PyValue::List(elems));

        lst
    }

    pub fn setattrs<A>(&self, object: ObjectId, attrs: A) -> PyResult<usize>
    where
        A: Iterator<Item = (u64, ObjectId, ObjectId)>,
    {
        let f = |this: &PyValue| -> PyResult<PyDictRaw<(ObjectId, ObjectId)>> {
            match this {
                PyValue::Any(inner)
                | PyValue::Module { inner, .. }
                | PyValue::Function { inner, .. } => Ok(inner.__dict__.clone()),

                _ => todo!("AttributeError"),
            }
        };

        let mut __dict__ = self.objects.with_object(object, |v| f(v))?;

        let mut count = 0;

        for (h, k, v) in attrs {
            __dict__.insert(h, (k, v));
            count += 1;
        }

        self.objects.with_object_mut(object, move |val| match val {
            PyValue::Any(inner)
            | PyValue::Module { inner, .. }
            | PyValue::Function { inner, .. } => std::mem::swap(&mut inner.__dict__, &mut __dict__),

            _ => unreachable!(),
        });

        Ok(count)
    }

    pub fn new_function(
        &self,
        body: AnyFunc,
        params: Box<[(SpanRef, Option<ObjectId>)]>,
        returns: Option<ObjectId>,
    ) -> ObjectId {
        let func = PyValue::Function {
            body,
            params,
            parent: None,
            returns,
            inner: Default::default(),
        };

        self.objects.insert(func)
    }
}

impl Runtime {
    pub fn new_uninit() -> Self {
        Self {
            hash_state: ahash::RandomState::new(),
            singletons: Default::default(),
            objects: DefaultObjectSpace::new(),
            module_objects: Default::default(),
        }
    }

    /// Partially initializes the interpreter runtime by constructing values for `True`, `False`, `None`, `Ellipsis` and integer values -126 to +126.
    pub fn try_init<H>(&mut self, _host: &mut H)
    where
        H: RuntimeHost,
    {
        macro_rules! init {
            ($name:ident, class) => {
                if self.singletons.$name.is_uninit() {
                    let alloc = self.objects.new_object_id();

                    self.objects.insert(PyValue::Class {
                        name: None,
                        parent: None,
                        inner: Default::default(),
                    });

                    self.singletons.$name = alloc;
                }
            };

            ($name:ident, $init:expr) => {
                if self.singletons.$name.is_uninit() {
                    let alloc = self.objects.insert($init);
                    self.singletons.$name = alloc;
                }
            };
        }

        init!(true_v, PyValue::Bool(true));
        init!(false_v, PyValue::Bool(false));
        init!(none_v, PyValue::None);
        init!(ellipsis_v, PyValue::Ellipsis);

        for n in -126_i64..=126_i64 {
            self.new_int(n);
        }

        // initialize core modules automatically for tests...
        #[cfg(test)]
        {
            init!(builtins, {
                PyValue::Module {
                    mkey: ModuleKey::Static("builtins"),
                    inner: Default::default(),
                }
            });

            init!(sys, {
                PyValue::Module {
                    mkey: ModuleKey::Static("sys"),
                    inner: Default::default(),
                }
            });

            let sys_path_hooks = self.make_kv_pair("path_hooks", |this| this.new_list(None));
            let sys_path = self.make_kv_pair("path", move |this| this.new_list(None));

            let sys_modules = self.make_kv_pair("modules", |this| {
                this.objects.insert(PyValue::Dict(Default::default()))
            });

            let sys_meta_path = self.make_kv_pair("meta_path", |this| this.new_list(None));

            let sys_attrs = [sys_path, sys_path_hooks, sys_modules, sys_meta_path];

            crate::import::bootstrap::setup(self, sys_meta_path.2, sys_path_hooks.2)
                .trace()
                .unwrap();

            self.setattrs(self.singletons.sys, sys_attrs.into_iter())
                .unwrap();
        }
    }

    /// Create and initialize a module from a builder and associate it with the provided module ref.
    pub fn synthesise_module(
        &mut self,
        mref: ModuleRef,
        module: ObjectBuilder<{ MODULE }>,
    ) -> PyResult<ObjectId> {
        let object_id = module.synthesise_within(self).trace()?;

        self.objects.with_object_mut(object_id, |val| match val {
            PyValue::Module { mkey, .. } => *mkey = ModuleKey::User(mref),
            _ => unreachable!(),
        });

        let mod_obj = ModuleMetadata {
            alloc: object_id,
            code: None,
            mref,
        };

        self.module_objects.insert(mref, mod_obj);

        Ok(object_id)
    }

    /// Evaluate some input with the given host.
    pub fn eval<'a, 'b, H, I, O>(
        &'a mut self,
        host: &'b mut H,
        input: I,
    ) -> Result<EvaluationContext<'a, 'b, H>, RuntimeError<H>>
    where
        H: RuntimeHost + AcceptInput<I, O>,
        O: Into<FlatCode>,
    {
        let code = host
            .accept_input(input)
            .map(Into::into)
            .map(Rc::new)
            .map_err(RuntimeError::Host)?;

        let mref = code.mref;

        let module = ObjectBuilder::<{ MODULE }>::new();
        let module_alloc_id = self
            .synthesise_module(mref, module)
            .trace()
            .map_err(|exc| RuntimeError::Py(exc))?;

        let meta = ModuleMetadata {
            mref,
            alloc: module_alloc_id,
            code: Some(Rc::clone(&code)),
        };

        self.module_objects.insert(mref, meta);

        let ecx = UnboundEvaluationContext { code, mref };

        Ok(ecx.bind(self, host))
    }
}
