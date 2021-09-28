use std::cell::{RefCell, RefMut};
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{fmt, io, panic, rc::Rc};

use ahash::AHashSet;
use dashmap::DashMap;
use parking_lot::Mutex;

use montyc_core::dict::PyDictRaw;
use montyc_core::utils::SSAMap;
use montyc_core::{
    BuiltinType, LocalTypeId, MapT, ModuleData, ModuleRef, MontyError, MontyResult, PythonType,
    SpanRef, TaggedValueId, Type, TypeId, TypingConstants, TypingContext, Value, ValueId, FUNCTION,
    MODULE,
};

use montyc_flatcode::FlatCode;

use montyc_hlirt::ctx::{CallCx, EvalGlue};
use montyc_hlirt::object::{FuncLike, ObjectBuilder, PyValue, ReadyCallable};
use montyc_hlirt::rt::{AcceptInput, Runtime, RuntimeHost, RuntimeHostExt};
use montyc_hlirt::{ObjectId, ObjectSpace, PyException, PyResult, PyResultExt};

use montyc_parser::{AstObject, SpanInterner};

use montyc_query::Queries;

use crate::prelude::*;
use crate::value_store::{GVKey, GlobalValueStore};

pub mod host;
pub mod query;

/// Statically known identifiers used to seed the span interner.
const MAGICAL_NAMES: &[&'static str] = &[
    // monty
    "__monty",
    // names of builtin types or functions
    "int",
    "float",
    "tuple",
    "object",
    "dict",
    "set",
    "type",
    "id",
    "isinstance",
    "list",
    "str",
    "bool",
    // operator-like dunders
    "__lshift__",
    "__rlshift__",
    "__getattr__",
    "__rgetattr__",
    "__eq__",
    "__req__",
    "__pow__",
    "__rpow__",
    "__or__",
    "__ror__",
    "__and__",
    "__rand__",
    "__div__",
    "__rdiv__",
    "__ne__",
    "__rne__",
    "__getitem__",
    "__rgetitem__",
    "__call__",
    "__rcall__",
    "__mul__",
    "__rmul__",
    "__add__",
    "__radd__",
    "__sub__",
    "__rsub__",
    "__getattribute__",
    "__rgetattribute__",
    "__rshift__",
    "__rrshift__",
    // Special dunders
    "__new__",
    "__annotations__",
    "__delitem__",
    "__reversed__",
    "__module__",
    "__invert__",
    "__repr__",
    "__name__",
    "__eq__",
    "__pos__",
    "__len__",
    "__getitem__",
    "__call__",
    "__iter__",
    "__contains__",
    "__neg__",
    "__setitem__",
    "__next__",
    "__missing__",
    "__init__",
    // C types
    "c_ulong",
    "c_ulong_p",
    "c_double",
    "c_double_p",
    "c_uint",
    "c_uint_p",
    "c_bool",
    "c_bool_p",
    "c_uint32",
    "c_uint32_p",
    "c_long",
    "c_long_p",
    "c_ulonglong",
    "c_ulonglong_p",
    "c_void",
    "c_void_p",
    "c_longlong",
    "c_longlong_p",
    "c_uint64",
    "c_uint64_p",
    "c_wchar",
    "c_wchar_p",
    "c_ssize_t",
    "c_ssize_t_p",
    "c_size_t",
    "c_size_t_p",
    "c_ushort",
    "c_ushort_p",
    "c_int8",
    "c_int8_p",
    "c_uint8",
    "c_uint8_p",
    "c_uint16",
    "c_uint16_p",
    "c_int",
    "c_int_p",
    "c_longdouble",
    "c_longdouble_p",
    "c_char",
    "c_char_p",
    "c_float",
    "c_float_p",
    "c_byte",
    "c_byte_p",
    "c_int64",
    "c_int64_p",
    "c_int32",
    "c_int32_p",
    "c_ubyte",
    "c_ubyte_p",
    "__value",
] as &[_];

/// A mapping of all staticly known identifiers to their respective spans.
type StaticNames = MapT<u32, &'static str>;

// -- TypingData

/// A global registar for types.
#[derive(Debug, Default)]
pub struct TypingData {
    inner: RefCell<SSAMap<Type>>,
    clobber: DashMap<Type, TypeId>,
}

impl TypingContext for TypingData {
    fn initialized() -> Self
    where
        Self: Sized,
    {
        let mut inner = SSAMap::default();

        montyc_core::BuiltinType::write_ssa(&mut inner);

        let clobber = DashMap::with_capacity(inner.iter().count());

        for (tid, ty) in inner.iter() {
            clobber.insert(ty.clone(), TypeId::from(tid));
        }

        let inner = RefCell::new(inner);

        Self { inner, clobber }
    }

    fn as_dyn_tcx(&self) -> &dyn TypingContext {
        self
    }

    fn localize<'tcx>(&'tcx self, type_id: TypeId) -> Option<montyc_core::LocalTypeId> {
        let ty = self.inner.borrow().get(type_id)?.clone();

        Some(LocalTypeId { type_id, ty })
    }

    fn insert(&self, ty: Type) -> TypeId {
        if let Some(tid) = self.clobber.get(&ty) {
            return *tid;
        }

        let tid = self.inner.borrow_mut().insert(ty.clone());
        self.clobber.insert(ty, tid);

        tid
    }

    fn insert_property(
        &self,
        base: TypeId,
        name: String,
        property: montyc_core::Property,
    ) -> Option<montyc_core::Property> {
        log::trace!(
            "[<TypingData as TypingContext>::insert_property] setting type property: {} {} {}",
            self.display_type(base, &|_| None)
                .unwrap_or_else(|| format!("{:?}", base)),
            name,
            self.display_type(property.type_id, &|_| None)
                .unwrap_or_else(|| format!("{:?}", property.type_id))
        );

        let mut borrow_mut = self.inner.borrow_mut();
        let type_repr = borrow_mut.get_mut(base)?;

        let previous_property = type_repr.properties.insert(name, property);

        self.clobber.insert(type_repr.clone(), base);

        previous_property
    }

    fn get_type_repr<'tcx>(&'tcx self, type_id: TypeId) -> Option<Type> {
        Some(self.inner.borrow().get(type_id)?.clone())
    }

    fn get_type_repr_mut<'tcx>(&'tcx self, type_id: TypeId) -> Option<RefMut<'tcx, Type>> {
        Some(RefMut::map(self.inner.borrow_mut(), move |inner| {
            inner.get_mut(type_id).unwrap()
        }))
    }

    fn get_python_type_of(&self, type_id: TypeId) -> Option<PythonType> {
        Some(self.inner.borrow().get(type_id)?.kind.clone())
    }

    fn display_type(
        &self,
        type_id: TypeId,
        type_id_of_val: &dyn Fn(ValueId) -> Option<TypeId>,
    ) -> Option<String> {
        struct TypeFormatter<'b, 'c> {
            root: PythonType,
            tcx: &'b TypingData,
            type_id_of_val: &'c dyn Fn(ValueId) -> Option<TypeId>,
        }

        impl<'b, 'c> TypeFormatter<'b, 'c> {
            fn display(
                ty: TypeId,
                tcx: &'b TypingData,
                type_id_of_val: &'c dyn Fn(ValueId) -> Option<TypeId>,
            ) -> String {
                let root = tcx.get_python_type_of(ty).unwrap();
                let this = Self {
                    root,
                    tcx,
                    type_id_of_val,
                };

                format!("{}", this)
            }

            fn list(
                types: &[TypeId],
                tcx: &'b TypingData,
                type_id_of_val: &'c dyn Fn(ValueId) -> Option<TypeId>,
            ) -> String {
                let mut fmt = vec![];

                for ty in types {
                    fmt.push(Self::display(*ty, tcx, type_id_of_val));
                }

                fmt.join(", ")
            }
        }

        impl<'b, 'c> fmt::Display for TypeFormatter<'b, 'c> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let TypeFormatter {
                    root,
                    tcx,
                    type_id_of_val,
                } = self;

                match root {
                    PythonType::Instance { of } => write!(
                        f,
                        "Instance of {}",
                        TypeFormatter::display(
                            type_id_of_val(*of).ok_or(fmt::Error)?,
                            *tcx,
                            *type_id_of_val
                        )
                    ),

                    PythonType::NoReturn => todo!(),
                    PythonType::Any => todo!(),
                    PythonType::Tuple { .. } => todo!(),
                    PythonType::List { .. } => todo!(),

                    PythonType::Union { members } => write!(
                        f,
                        "Union[{}]",
                        TypeFormatter::list(
                            &*members.clone().unwrap_or_default(),
                            *tcx,
                            *type_id_of_val
                        )
                    ),

                    PythonType::Type { of } => write!(
                        f,
                        "Type[{}]",
                        TypeFormatter::display(*of, *tcx, *type_id_of_val)
                    ),

                    PythonType::TypeVar { .. } => todo!(),

                    PythonType::Callable { params: args, ret } => match (args, ret) {
                        (Some(args), ret) => {
                            write!(
                                f,
                                "({}) -> {}",
                                TypeFormatter::list(&*args, *tcx, *type_id_of_val),
                                TypeFormatter::display(*ret, *tcx, *type_id_of_val),
                            )
                        }

                        (None, ret) => {
                            write!(
                                f,
                                "() -> {}",
                                TypeFormatter::display(*ret, *tcx, *type_id_of_val)
                            )
                        }
                    },

                    PythonType::Generic { .. } => todo!(),
                    PythonType::Builtin { inner } => match inner {
                        BuiltinType::Bytes => write!(f, "<bytes>"),
                        BuiltinType::Int => write!(f, "<int>"),
                        BuiltinType::Float => write!(f, "<float>"),
                        BuiltinType::Str => write!(f, "<str>"),
                        BuiltinType::Bool => write!(f, "<bool>"),
                        BuiltinType::None => write!(f, "<none>"),
                        BuiltinType::Ellipsis => write!(f, "<...>"),
                        BuiltinType::Module => write!(f, "<module>"),
                        BuiltinType::Unknown => write!(f, "<unknown>"),
                        BuiltinType::Type => write!(f, "<type>"),
                        BuiltinType::Object => write!(f, "<object>"),
                        BuiltinType::TSelf => write!(f, "<self>"),
                        BuiltinType::UntypedFunc => write!(f, "<callable(unknown) -> unknown>"),
                        BuiltinType::UntypedTuple => write!(f, "<tuple[unknown, ...]>"),
                        BuiltinType::AnyType => write!(f, "<any>"),
                        BuiltinType::U8 => write!(f, "<u8>"),
                        BuiltinType::U16 => write!(f, "<u16>"),
                        BuiltinType::U32 => write!(f, "<u32>"),
                        BuiltinType::U64 => write!(f, "<u64>"),
                        BuiltinType::I8 => write!(f, "<i8>"),
                        BuiltinType::I16 => write!(f, "<i16>"),
                        BuiltinType::I32 => write!(f, "<i32>"),
                        BuiltinType::I64 => write!(f, "<i64>"),
                        BuiltinType::Never => write!(f, "<never>"),
                    },
                }
            }
        }

        let kind = self.inner.borrow().get(type_id)?.kind.clone();
        let formatter = TypeFormatter {
            root: kind,
            tcx: self,
            type_id_of_val,
        };

        Some(format!("{}", formatter))
    }
}

// -- SessionContext

#[derive(Debug, Default)]
pub struct Pot {
    dirty: Mutex<bool>,
    pub(crate) n_objects: AtomicUsize,
}

/// Global state for the current compilation.
#[derive(Debug)]
pub struct SessionContext {
    /// The options this context was created with.
    pub(crate) opts: CompilerOptions,

    /// A Span interner shared between all parsing sessions.
    pub(crate) spanner: SpanInterner,

    /// Static names that get loaded at startup.
    static_names: StaticNames,

    /// A registry of the current modules that have been included.
    pub(crate) modules: Mutex<SSAMap<Rc<ModuleData>>>,

    /// A map of all the source code for every module imported.
    pub(crate) module_sources: DashMap<ModuleRef, Box<str>>,

    /// A map of module -> ast.
    pub(crate) module_asts: DashMap<ModuleRef, Rc<montyc_parser::ast::Module>>,

    /// An interpreter runtime for consteval.
    pub(crate) const_runtime: Rc<RefCell<montyc_hlirt::rt::Runtime>>,

    /// Used to keep track of type information.
    pub(crate) typing_context: TypingData,

    /// A global caching store for all processed values.
    pub(crate) value_store: Box<GlobalValueStore>,

    pot: Pot,
}

impl SessionContext {
    fn new(opts: CompilerOptions) -> Self {
        let value_store = Box::new(GlobalValueStore::default());
        let const_runtime = Rc::new(RefCell::new(Runtime::new_uninit()));

        Self {
            opts,
            spanner: SpanInterner::new(),
            static_names: Default::default(),
            modules: Default::default(),
            module_sources: Default::default(),
            module_asts: Default::default(),
            const_runtime: Rc::clone(&const_runtime),
            typing_context: TypingData::initialized(),
            value_store,
            pot: Pot::default(),
        }
    }

    /// Initiate a new context with the given, verified, options.
    pub fn initialize(
        VerifiedCompilerOptions(opts): &VerifiedCompilerOptions,
    ) -> Result<Self, (Self, MontyError)> {
        let mut gcx = Self::new(opts.clone());

        log::debug!("[global_context:initialize] {:?}", opts);
        log::debug!("[global_context:initialize] stdlib := {:?}", opts.libstd());

        let monty_mdata = Rc::new(ModuleData {
            mref: ModuleRef(0),
            path: PathBuf::default(),
            name: String::from("__monty"),
            qualname: vec![String::from("__monty")],
        });

        gcx.modules
            .lock()
            .insert::<ModuleRef, _>(monty_mdata.clone());

        // There are identifiers we want to refer to statically e.g. __new__, __del__, or __rrshift__
        // but `SpanRef`s are generated at runtime through our `SpanInterner` so we just initialize a map
        // of `Dict[SpanRef, str]` at startup.
        //
        // This enables us to soundly refer to static identifiers as spans and correctly get the original
        // stringy name back from a `SpanRef` for codespan/reflection purposes.
        for raw_name in MAGICAL_NAMES.iter().cloned() {
            let name_ref = gcx
                .spanner
                .str_to_spanref::<0>(raw_name) // INVARIANT: ModuleRef(0) is reserved.
                .expect("Span interner was already mutably borrowed!");

            gcx.static_names.insert(name_ref.group(), raw_name);
        }

        log::debug!(
            "[global_context:initialize] initialized {:?} static names",
            gcx.static_names.len()
        );

        match gcx.initialize_hlirt(monty_mdata.as_ref()) {
            Ok(_) => Ok(gcx),
            Err(err) => match err {
                montyc_hlirt::rt::RuntimeError::Host(e) => Err((gcx, e)),
                _ => todo!("{:#?}", err),
            },
        }
    }

    /// Initialize the interpreter runtime, setting .
    fn initialize_hlirt(
        &mut self,
        _monty: &ModuleData,
    ) -> Result<(), montyc_hlirt::rt::RuntimeError<&Self>> {
        use montyc_hlirt::rt::RuntimeError;

        let rt = Rc::clone(&self.const_runtime);

        {
            {
                let mut rt = rt.borrow_mut();

                rt.try_init(&mut &*self); // initialize interpreter builtins and classes.

                fn nop(_cx: CallCx) -> PyResult<ObjectId> {
                    todo!()
                }

                rt.singletons.monty = ObjectBuilder::<{ MODULE }>::new()
                    .setattr("extern", ReadyCallable::from(nop))
                    .synthesise_within(&mut *rt)?;
            }

            let (_, builtins) = self
                .include_module(self.opts.libstd().join("builtins.py"), "builtins")
                .map_err(|e| montyc_hlirt::rt::RuntimeError::Host(e))?;

            let mut rt = rt.borrow_mut();

            rt.singletons.builtins = builtins;

            let default_builtin_classes: &[(TypeId, fn(&mut Runtime) -> &mut ObjectId)] = &[
                (TypingConstants::Object, |rt| {
                    &mut rt.singletons.object_class
                }),
                (TypingConstants::Type, |rt| &mut rt.singletons.type_class),
                (TypingConstants::Bool, |rt| &mut rt.singletons.bool_class),
                (TypingConstants::Int, |rt| &mut rt.singletons.int_class),
                (TypingConstants::Float, |rt| &mut rt.singletons.float_class),
                (TypingConstants::Str, |rt| &mut rt.singletons.string_class),
                (TypingConstants::UntypedFunc, |rt| {
                    &mut rt.singletons.function_class
                }),
            ];

            log::trace!("[SessionContext::initialize_hlirt] proceeding to overwrite builtin singletons with classes from builtins.py");

            for (ty, as_mut_ref) in default_builtin_classes {
                let class_name = match self.typing_context.get_python_type_of(*ty).unwrap() {
                    PythonType::Builtin { inner } => String::from(inner.name()),
                    _ => unreachable!(),
                };

                let name_hash = rt.hash(class_name);
                let (_, real_class) = rt
                    .objects
                    .with_object(builtins, move |val| match val {
                        PyValue::Module { inner, .. } => inner.__dict__.get(name_hash),
                        _ => todo!(),
                    })
                    .unwrap();

                (*as_mut_ref(&mut *rt)) = real_class;
            }

            for (ty, as_mut_ref) in default_builtin_classes {
                let klass = *as_mut_ref(&mut *rt);
                let klass_dict = rt.objects.with_object(klass, |val| match val {
                    PyValue::Class { inner, .. } => inner.__dict__.clone(),
                    _ => todo!(),
                });

                for (_, (k, v)) in klass_dict.iter() {
                    let name = rt.objects.with_object(*k, |v| match v {
                        PyValue::Str(s) => s.to_string(),
                        _ => todo!(),
                    });

                    let val = self.value_store.get_by_assoc(*v).unwrap();

                    let property = montyc_core::Property::new(
                        self.compute_type_of(&*rt, val)
                            .map_err(|e| montyc_hlirt::rt::RuntimeError::Host(e))?,
                        montyc_core::PropertyValue::Id(val),
                    );

                    let base = self
                        .typing_context
                        .insert(PythonType::Type { of: *ty }.into());

                    self.typing_context.insert_property(base, name, property);
                }
            }

            log::trace!("[SessionContext::initialize_hlirt] done");

            // initialize the sys module with stuff necessary for importing.
            rt.singletons.sys = {
                let sys = ObjectBuilder::<{ MODULE }>::new()
                    .setattr("path", Vec::<ObjectId>::new())
                    .setattr("path_hooks", Vec::<ObjectId>::new())
                    .setattr("meta_path", Vec::<ObjectId>::new())
                    .setattr("modules", PyDictRaw::from(MapT::new()));

                sys.synthesise_within(&mut *rt)?
            };
        }

        // short version: "take the current bootstrap file and eval it and simply natively define stuff its missing."
        //
        // the long version:
        //
        // first include and eval `libstd/importlib/_bootstrap.py`
        // then take its module object and register that in `sys.modules` under the qualname `importlib._bootstrap`
        // once that's done we "patch" the module by associating some native procedures defined here with the module.
        //
        // why do we do this? well because currently there are two major deficiencies at play here:
        //
        //  * the current montyc_parser can NOT handle try/except blocks, (and I think the raise statement isn't a thing in the ast.)
        //  * montyc_hlirt is seriously lacking in defining methods for builtin types so there's a bunch of stuff we cant do.
        //
        // until this gets fixed we gotta do this.

        let bootstrap = {
            let bootstrap_path = self.opts.libstd().join("importlib/_bootstrap.py");

            self.include_module(bootstrap_path, "importlib._bootstrap")
                .map_err(RuntimeError::Host)?
        };

        crate::import::setup(rt, bootstrap)?;

        Ok(())
    }

    #[inline]
    pub(crate) fn resolve_sref_as_str(&self, sref: SpanRef) -> Option<&str> {
        self.spanner.spanref_to_str(sref, |mref, span| {
            if mref == ModuleRef(0) {
                self.static_names.get(&sref.group()).cloned()
            } else {
                self.module_sources.get(&mref)?.value().get(span)
            }
        })
    }

    #[inline]
    fn parse_module(&self, source: &str, path: &Path, module_name: &str) -> ModuleRef {
        let mut modules = self.modules.lock();
        let mref = (modules.reserve() as u32).into();

        let module_ast = montyc_parser::parse(
            &source,
            montyc_parser::comb::module,
            Some(self.spanner.clone()),
            mref,
        );

        // if module_ast.is_err() {
        //     self.modules.cancel_reserve(mref).unwrap();
        // }

        let module = ModuleData {
            path: path.to_path_buf(),
            mref,
            name: module_name.to_string(),
            qualname: vec![path.file_stem().unwrap().to_string_lossy().to_string()],
        };

        let _ = self
            .module_sources
            .insert(mref, source.to_string().into_boxed_str());

        let _ = self.module_asts.insert(mref, Rc::new(module_ast));
        let _ = modules.try_set_value(mref, module).unwrap();

        mref
    }

    #[inline]
    fn load_module_with<T>(
        &self,
        path: impl AsRef<Path>,
        module_name: impl AsRef<str>,
        f: impl Fn(&Self, ModuleRef) -> T,
    ) -> io::Result<T> {
        let path = path.as_ref();

        if let Some(_) = self
            .modules
            .lock()
            .iter()
            .filter(|(_, module)| module.path == path)
            .next()
        {
            log::error!("[global_context:load_module_with] Found a module with the same path as one we're trying to load! path={:?}", path);

            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                "Attempted to load a module with a path that is already loaded.",
            ));
        }

        log::debug!(
            "[global_context:load_module_with] Loading module {:?}",
            path
        );

        let source = match std::fs::read_to_string(path) {
            Ok(st) => st.into_boxed_str(),
            Err(why) => {
                log::error!(
                    "[global_context:load_module_with] Failed to read path contents! {:?}",
                    why
                );
                return Err(why);
            }
        };

        let mref = self.parse_module(&source, path, module_name.as_ref());

        Ok(f(self, mref))
    }

    /// Subject a module to evaluation.
    fn eval_module(&self, mref: ModuleRef) -> MontyResult<ObjectId> {
        use montyc_hlirt::rt::RuntimeError;

        let rt = Rc::clone(&self.const_runtime);
        let mut rt = rt.borrow_mut();

        let mut this = self;
        let ecx = match rt.eval(&mut this, mref) {
            Ok(cx) => cx,
            Err(err) => match err {
                RuntimeError::Host(_) => todo!(),
                RuntimeError::Py(_) => todo!(),
                RuntimeError::ModuleAlreadyLoaded => todo!(),
            },
        };

        *ecx.host.pot.dirty.lock() = true;

        let module = match ecx
            .set_upper_tick_bound(0x1000.try_into().ok())
            .run_until_complete()
        {
            Ok(module) => module,
            Err(exc) => todo!("runtime exception! {:#?}", exc),
        };

        Ok(module)
    }

    /// Add a new module, by its path, to the current context.
    ///
    /// This will load the module, parse it, and consteval it before returning.
    ///
    #[inline]
    pub fn include_module(
        &self,
        path: impl AsRef<Path>,
        name: impl AsRef<str>,
    ) -> MontyResult<(ModuleRef, ObjectId)> {
        self.load_module_with(path, name, |gcx, mref| {
            let module = gcx.eval_module(mref)?;
            let () = gcx.stir_the_pot()?;

            let module_value = gcx
                .value_store
                .with_metadata(module, |meta| meta.value_ix)
                .unwrap();

            gcx.value_store.assoc(mref, module_value);

            Ok((mref, module))
        })
        .map_err(|err| MontyError::IO(err))?
    }

    /// A "fancy path" is any colon-deliminated string describing a path to a function through it's modules.
    ///
    /// i.e. `__main__:main` would talk about a function called `main` in the module `__main__.py`
    ///
    #[inline]
    pub(crate) fn resolve_fancy_path_to_modules(&self, path: impl AsRef<str>) -> Vec<ModuleRef> {
        let path = path.as_ref();
        let modules = self.modules.lock();

        let mut candidates: Vec<ModuleRef> = modules
            .iter()
            .map(|(mref, _)| (mref as u32).into())
            .collect();

        let mut drain_bucket = Vec::with_capacity(candidates.len()); // Hack: drain_filter is still feature gated.

        let segments: Vec<_> = path.split(":").collect();
        let mut prefix = {
            let mut prefix = String::with_capacity(path.len());

            prefix.push_str(segments.first().unwrap());

            for mref in candidates.drain(..) {
                if let Some(module) = modules.get(mref.clone()) {
                    if module.name.starts_with(&prefix) {
                        drain_bucket.push(mref);
                    }
                }
            }

            std::mem::swap(&mut candidates, &mut drain_bucket);

            prefix
        };

        // All segments except the last one are module names.
        for segment in segments
            .as_slice()
            .get(1..segments.len().saturating_sub(1))
            .unwrap()
        {
            for mref in candidates.drain(..) {
                if let Some(module) = modules.get(mref.clone()) {
                    if module.name.starts_with(&prefix) {
                        drain_bucket.push(mref);
                    }
                }
            }

            std::mem::swap(&mut candidates, &mut drain_bucket);

            prefix.push_str(*segment);
        }

        candidates
    }

    #[inline]
    pub fn get_func_from_path(&self, path: &str) -> MontyResult<TaggedValueId<{ FUNCTION }>> {
        if !path.contains(":") {
            panic!("entry path must speciful at least one module.");
        }

        let store = &self.value_store;

        let mref = match self.resolve_fancy_path_to_modules(path).as_slice() {
            [] => match &self.opts {
                CompilerOptions::Check { input, .. } | CompilerOptions::Build { input, .. } => {
                    if !self
                        .modules
                        .lock()
                        .iter()
                        .any(|(_, data)| data.path == *input)
                    {
                        self.include_module(input, "__main__")?;
                        return self.get_func_from_path(path);
                    } else {
                        todo!("could not find a module matching the path {:?}", path);
                    }
                }
            },

            [mref] => mref.clone(),
            [_, ..] => todo!("ambiguity in resolving module."),
        };

        let fname = path.split(":").last().unwrap();

        let rt = Rc::clone(&self.const_runtime);
        let fname_hash = rt.borrow().hash(fname);

        let func = {
            let module = store.alloc_id_of(mref.resolve(store).unwrap()).unwrap();
            let module_dict = rt.borrow().objects.with_object(module, |val| match val {
                PyValue::Module { inner, .. } => Some(inner.__dict__.clone()),
                _ => None,
            });

            let func = match module_dict.map(|dict| dict.get(fname_hash)) {
                None => todo!("specified module is not a module value?"),
                Some(None) => todo!("no such function."),
                Some(Some((_, fobj))) => fobj,
            };

            let is_func = rt.borrow().objects.with_object(func, |val| match val {
                PyValue::Function { .. } => true,
                _ => false,
            });

            if !is_func {
                todo!("not a func.");
            }

            store.get_by_assoc(func).ok_or(MontyError::None)?
        };

        Ok(TaggedValueId::func(func))
    }

    /// Serialize the runtime object graph into a consumable format for type checking and analysis.
    fn stir_the_pot(&self) -> MontyResult<()> {
        let mut dirty_guard = match self.pot.dirty.try_lock() {
            Some(guard) if *guard => guard,
            _ => return Ok(()),
        };

        let rt = Rc::clone(&self.const_runtime);

        let n_prev_objects = self.pot.n_objects.load(Ordering::SeqCst);
        let n_cur_objects = rt.borrow().objects.size_hint().unwrap_or(n_prev_objects);

        let n_obj_diff = n_cur_objects
            .checked_sub(n_prev_objects)
            .unwrap_or(n_cur_objects);

        let mut new_objects = MapT::with_capacity(n_obj_diff);
        let mut update_objects = MapT::new();

        let mut ref_bucket = Vec::with_capacity(64);

        let store = &self.value_store;

        rt.borrow().objects.for_each(|id, val| {
            assert!(ref_bucket.is_empty());

            val.refs_with(&mut ref_bucket);

            let mut h_refs = ahash::AHashSet::<ObjectId>::with_capacity(ref_bucket.len());
            h_refs.extend(ref_bucket.iter());
            h_refs.remove(&ObjectId::default());
            h_refs.shrink_to_fit();

            if !store.contains(id) {
                new_objects.insert(id, h_refs);
            } else {
                let internal_refs = store
                    .with_metadata(id, |m| m.internal_refs.clone())
                    .expect("object id is in alloc_data so there must be a value.");

                let h_refs = if h_refs.is_empty() {
                    None
                } else {
                    Some(h_refs)
                };

                match (internal_refs, h_refs) {
                    (None, None) => (),

                    (Some(a), Some(b)) => {
                        if a.iter()
                            .cloned()
                            .map(|v| self.value_store.alloc_id_of(v).unwrap())
                            .collect::<AHashSet<_>>()
                            != b
                        {
                            update_objects.insert(id, Some(b));
                        }
                    }

                    (Some(_), h_refs @ None) | (None, h_refs @ Some(_)) => {
                        update_objects.insert(id, h_refs);
                    }
                }
            }

            ref_bucket.clear();
        });

        for id in new_objects.keys() {
            let value_id = store.insert(Value::default());

            store.assoc(*id, value_id);
        }

        for (id, refs) in new_objects.drain() {
            let value_id = store.get_by_assoc(id).unwrap();

            store.with_metadata_mut(value_id, |meta| {
                meta.object_id.replace(id);
                meta.internal_refs.replace(
                    refs.into_iter()
                        .map(|id| id.resolve(&self.value_store).unwrap())
                        .collect(),
                );
            });
        }

        for (id, refs) in update_objects.drain() {
            store.with_metadata_mut(id, |m| {
                m.internal_refs.replace(
                    refs.unwrap_or_default()
                        .into_iter()
                        .map(|id| id.resolve(&self.value_store).unwrap())
                        .collect(),
                )
            });
        }

        *dirty_guard = false;

        Ok(())
    }

    fn resolve_type_annotation(&self, rt: &Runtime, ann: ObjectId) -> MontyResult<TypeId> {
        log::trace!("[SessionContext::resolve_type_annotation] ann={:?}", ann);

        let (_name, _parent) = rt.objects.with_object(ann, |val| match val {
            PyValue::Class { name, parent, .. } => (name.clone(), parent.clone()),
            _ => todo!(),
        });

        match self.klass_to_instance_type(&*rt, ann) {
            Ok(t) => Ok(t),

            Err(MontyError::None) => {
                let klass = self.value_store.get_by_assoc(ann).unwrap();
                let instance = self
                    .typing_context
                    .insert(PythonType::Instance { of: klass }.into());

                Ok(instance)
            }

            Err(err) => Err(err),
        }
    }

    fn object_type(&self, rt: &Runtime, val: &PyValue, val_class: ObjectId) -> MontyResult<TypeId> {
        log::trace!("[SessionContext::object_type] class={:?}", val_class);

        if let Some(elems) = val.as_list() {
            log::trace!(
                "[SessionContext::object_type]  value is a list(len={}).",
                elems.len()
            );

            let it = elems
                .iter()
                .map(|o| self.value_store.get_by_assoc(*o).unwrap());

            let mut inner = self
                .typing_context
                .insert(Type::from(PythonType::Union { members: None }));

            for val in it {
                let val_t = self.compute_type_of(rt, val)?;

                inner = self.typing_context.make_union(inner, val_t).unwrap();
            }

            let list = self.typing_context.list(inner);

            log::trace!(
                "[SessionContext::object_type]  produced type {}",
                self.typing_context
                    .display_type(list, &|v| self.value_store.type_id_of(v))
                    .unwrap()
            );

            Ok(list)
        } else if let Some(func) = val.as_func() {
            match func {
                FuncLike::Def {
                    body: _,
                    params,
                    returns,
                } => {
                    log::trace!(
                        "[SessionContext::object_type]  value is a function(args={}, ret={:?}).",
                        params.len(),
                        returns
                    );

                    let args = match params {
                        [] => None,
                        params => {
                            let mut param_t = vec![];
                            let mut params = params.into_iter().peekable();

                            if let Some((var, None)) = params.peek() {
                                if Queries::spanref_to_str(self, *var)? == "self" {
                                    param_t.push(TypingConstants::TSelf);
                                    let _ = params.next();
                                }
                            }

                            log::trace!(
                                "[SessionContext::object_type]  resolving parameter annotations",
                            );

                            for (name, ann) in params {
                                log::trace!(
                                    "[SessionContext::object_type]  resolving {:?} : {:?}",
                                    name,
                                    ann,
                                );

                                let type_id = match ann {
                                    Some(o) => self.resolve_type_annotation(rt, *o)?,
                                    None => TypingConstants::None,
                                };

                                log::trace!(
                                    "[SessionContext::object_type]  argument annotation is: {:?}",
                                    self.typing_context
                                        .display_type(type_id, &|v| self.value_store.type_id_of(v))
                                        .unwrap()
                                );

                                param_t.push(type_id);
                            }

                            Some(param_t)
                        }
                    };

                    let ret = match returns {
                        Some(ret) => self.resolve_type_annotation(rt, ret)?,
                        None => TypingConstants::None,
                    };

                    log::trace!(
                        "[SessionContext::object_type]  return annotation is: {:?}",
                        self.typing_context
                            .display_type(ret, &|v| self.value_store.type_id_of(v))
                            .unwrap()
                    );

                    let py = PythonType::Callable { params: args, ret };
                    let fn_type = self.typing_context.insert(Type::from(py));

                    log::trace!(
                        "[SessionContext::object_type]  produced type: {:?}",
                        self.typing_context
                            .display_type(fn_type, &|v| self.value_store.type_id_of(v))
                            .unwrap()
                    );

                    Ok(fn_type)
                }

                _ => todo!("{:?}", func),
            }
        } else {
            unimplemented!("{:#?}", val)
        }
    }

    fn klass_to_instance_type(&self, rt: &Runtime, klass: ObjectId) -> MontyResult<TypeId> {
        log::trace!("[SessionContext::klass_to_instance_type] klass={:?}", klass);

        macro_rules! switch {
            ($k:ident, $t:ident) => {
                if rt.singletons.$k == klass {
                    log::trace!("[SessionContext::klass_to_instance_type]  klass is a: {:?}", stringify!($k));

                    return Ok(::montyc_core::TypingConstants::$t);
                }
            };

            [$( $k:ident -> $t:ident )|+] => {
                $(
                    switch!($k, $t);
                )+
            };
        }

        switch![
            bool_class -> Bool
            | int_class -> Int
            | float_class -> Float
            | none_class -> None
            | ellipsis_class -> Ellipsis
            | string_class -> Str
            | module_class -> Module
            | type_class -> Type
            | object_class -> Object
            | bytes_class -> Bytes
        ];

        Err(MontyError::None)
    }

    fn compute_type_of(&self, rt: &Runtime, val: ValueId) -> MontyResult<TypeId> {
        log::trace!("[SessionContext::compute_type_of] {:?}", val);

        let obj = self
            .value_store
            .with_metadata(val, |m| m.object_id)
            .ok_or(MontyError::None)?
            .unwrap(); // TODO: handle values that do not have an object repr.

        let object_class = rt.class_of(obj);

        assert!(!object_class.is_uninit());

        log::trace!(
            "[SessionContext::compute_type_of] class of {:?} is {:?}",
            val,
            object_class
        );

        match self.klass_to_instance_type(&*rt, object_class) {
            Ok(t) => Ok(t),

            Err(MontyError::None) => rt
                .objects
                .with_object(obj, |val| self.object_type(&*rt, val, object_class)),

            Err(err) => Err(err),
        }
    }
}
