use std::{
    cell::RefCell,
    io,
    path::{Path, PathBuf},
    rc::Rc,
};

use montyc_core::{utils::SSAMap, ModuleRef, MontyError, MontyResult, SpanRef};
use montyc_hlir::{
    interpreter::{self, HostGlue},
    typing::{PythonType, TypingContext},
    value_store::GlobalValueStore,
    ModuleData, ModuleObject, Value,
};
use montyc_parser::SpanInterner;

use crate::{prelude::*, value_context::ValueContext};

/// Statically known identifiers used to seed the span interner.
const MAGICAL_NAMES: &[&'static str] = &[
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
type StaticNames = ahash::AHashMap<SpanRef, &'static str>;

#[derive(Debug)]
pub struct ImportPath {
    base: PathBuf,
    curdir: PathBuf,
}

impl ImportPath {
    #[inline]
    #[allow(dead_code)]
    pub fn new(base: PathBuf) -> Self {
        Self {
            base: base.clone(),
            curdir: base,
        }
    }

    #[inline]
    #[allow(dead_code)]
    pub fn resolve_import_to_path(
        qualname: Vec<&str>,
        paths: impl Iterator<Item = PathBuf>,
    ) -> Option<PathBuf> {
        'outer: for path in paths {
            let mut root = path.to_owned();

            let mut path = Self::new(root.clone());

            for part in qualname.iter() {
                let final_path = match path.advance(part) {
                    Some(p) => p,
                    None => continue 'outer,
                };

                root = final_path.clone();
            }

            return Some(root);
        }

        None
    }

    #[inline]
    #[allow(dead_code)]
    pub fn advance(&mut self, expected: impl AsRef<str>) -> Option<PathBuf> {
        let expected = expected.as_ref();

        log::trace!("import: {:?} ++ {:?}", self.curdir, expected);

        let next = self.curdir.read_dir().ok()?.find_map(|maybe_entry| {
            let entry = maybe_entry.ok()?;
            let file_type = entry.file_type().ok()?;
            let path = entry.path();

            let stem = path.file_stem()?.to_string_lossy();
            let wellformed_stem = !stem.contains(".");

            let ok = if file_type.is_file() {
                let has_py_ext = path.extension()?.to_string_lossy() == "py";

                has_py_ext && wellformed_stem && (stem == expected)
            } else if file_type.is_dir() {
                stem == expected
            } else {
                unreachable!();
            };

            if ok {
                Some(path)
            } else {
                None
            }
        })?;

        if next.is_dir() {
            self.curdir = next.clone();
        }

        Some(next)
    }
}

/// Global state for the current compilation.
#[derive(Debug)]
pub struct GlobalContext {
    /// The options this context was created with.
    opts: CompilerOptions,

    /// A Span interner shared between all parsing sessions.
    spanner: SpanInterner,

    /// Static names that get loaded at startup.
    static_names: StaticNames,

    /// A registry of the current modules that have been included.
    pub(crate) modules: SSAMap<ModuleRef, RefCell<montyc_hlir::ModuleObject>>,

    /// A map of all the source code for every module imported.
    module_sources: ahash::AHashMap<ModuleRef, Box<str>>,

    /// An interpreter runtime for consteval.
    pub(crate) const_runtime: Rc<RefCell<interpreter::Runtime>>,

    /// Used to keep track of type information.
    pub(crate) typing_context: RefCell<TypingContext>,

    /// A global caching store for all processed values.
    pub(crate) value_store: Rc<RefCell<GlobalValueStore>>,
}

impl GlobalContext {
    /// Initiate a new context with the given, verified, options.
    pub fn initialize(VerifiedCompilerOptions(opts): &VerifiedCompilerOptions) -> Self {
        let value_store = Rc::new(RefCell::new(GlobalValueStore::default()));
        let const_runtime = Rc::new(RefCell::new(interpreter::Runtime::new(
            0x1000,
            Rc::clone(&value_store) as Rc<_>,
        )));

        let mut gcx = Self {
            opts: opts.clone(),
            spanner: SpanInterner::new(),
            static_names: Default::default(),
            modules: SSAMap::new(),
            module_sources: Default::default(),
            const_runtime: Rc::clone(&const_runtime),
            typing_context: RefCell::new(TypingContext::initialized()),
            value_store,
        };

        gcx.modules.insert(RefCell::new(ModuleObject {
            mref: ModuleRef(0),
            data: Rc::new(ModuleData {
                mref: ModuleRef(0),
                ast: Default::default(),
                path: PathBuf::default(),
                name: String::from("__monty"),
            }),
        }));

        const_runtime.borrow_mut().initialize_monty_module(&mut gcx);

        log::debug!("[global_context:initialize] {:?}", opts);
        log::debug!("[global_context:initialize] stdlib := {:?}", opts.libstd());

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

            gcx.static_names.insert(name_ref, raw_name);
        }

        log::debug!(
            "[global_context:initialize] initialized {:?} static names",
            gcx.static_names.len()
        );

        let _ = gcx.include_module(opts.libstd().join("builtins.py"), "builtins");

        gcx
    }

    #[inline]
    fn load_module_with<T>(
        &mut self,
        path: impl AsRef<Path>,
        module_name: impl AsRef<str>,
        f: impl Fn(&mut Self, ModuleRef) -> T,
    ) -> io::Result<T> {
        let path = path.as_ref();

        if let Some(_) = self
            .modules
            .iter()
            .filter(|(_, module)| module.borrow().path() == path)
            .next()
        {
            log::error!("[global_context:load_module_with] Found a module with the same path as one we're trying to load! path={:?}", path);

            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                "Attempted to load a module with a path that is already loaded.",
            ));
        }

        let mref = self.modules.reserve();

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

        let module = montyc_parser::parse(
            &source,
            montyc_parser::comb::module,
            Some(self.spanner.clone()),
            mref,
        );

        let module = montyc_hlir::ModuleObject::new(
            path.to_path_buf(),
            module,
            mref,
            module_name.as_ref().to_string(),
        );

        let _ = self.module_sources.insert(mref, source);
        let _ = self.modules.try_set_value(mref, module).unwrap();

        Ok(f(self, mref))
    }

    /// Add a new module, by its path, to the current context.
    ///
    /// This will load the module, parse it, consteval it, and typecheck it
    /// before returning.
    ///
    #[inline]
    pub fn include_module(
        &mut self,
        path: impl AsRef<Path>,
        name: impl AsRef<str>,
    ) -> MontyResult<ModuleRef> {
        #[allow(unreachable_code)]
        self.load_module_with(path, name, |gcx, mref| -> MontyResult<ModuleRef> {
            let const_rt = gcx.const_runtime.clone();

            let (module_index, module_code) = const_rt.borrow_mut().consteval(mref, gcx).unwrap();
            // let runtime = const_rt.borrow();
            let mut store = gcx.value_store.borrow_mut();

            let _module_alloc_id = store.alloc_id_of(module_index).unwrap();

            let mut rib = {
                let parent = store.value_graph.node_weight(module_index).unwrap();
                let mut names = ahash::AHashMap::new();

                for (_, (key, _)) in parent.iter() {
                    let key = store
                        .value_graph
                        .node_weight(*key)
                        .map(|weight| match weight {
                            Value::String(st) => montyc_hlir::HostGlue::str_to_spanref(gcx, &st),
                            _ => unreachable!(),
                        })
                        .unwrap();

                    names.insert(key.group(), TypingContext::Unknown);
                }

                names
            };

            store.metadata(module_index).rib.replace(rib.clone());

            let (mref, properties) = match store
                .value_graph
                .node_weight(module_index)
                .expect("missing module index.")
            {
                Value::Module { mref, properties } => (*mref, properties),
                _ => unreachable!(),
            };

            for (key_idx, value_idx) in properties.iter_by_alloc_asc(&store) {
                let value = &store.value_graph.node_weight(value_idx).unwrap().clone();

                let value_type = ValueContext {
                    mref,
                    gcx,
                    value_store: &mut *store,
                    code: &module_code,
                    value,
                    value_idx,
                }
                .typecheck()?;

                if let Some(key) = store
                    .value_graph
                    .node_weight(key_idx)
                    .map(|weight| match weight {
                        Value::String(st) => montyc_hlir::HostGlue::str_to_spanref(gcx, &st),
                        _ => unreachable!(),
                    })
                {
                    rib.insert(key.group(), value_type);
                    store.metadata(module_index).rib.replace(rib.clone());
                }
            }

            Ok(mref)
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

        let mut candidates: Vec<_> = self.modules.iter().map(|(mref, _)| mref).collect();
        let mut drain_bucket = Vec::with_capacity(candidates.len()); // Hack: drain_filter is still feature gated.

        let segments: Vec<_> = path.split(":").collect();
        let mut prefix = {
            let mut prefix = String::with_capacity(path.len());

            prefix.push_str(segments.first().unwrap());

            for mref in candidates.drain(..) {
                if let Some(module) = self.modules.get(mref.clone()) {
                    if module.borrow().data.name.starts_with(&prefix) {
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
                if let Some(module) = self.modules.get(mref.clone()) {
                    if module.borrow().data.name.starts_with(&prefix) {
                        drain_bucket.push(mref);
                    }
                }
            }

            std::mem::swap(&mut candidates, &mut drain_bucket);

            prefix.push_str(*segment);
        }

        candidates
    }

    /// From a given `entry` path, recursively lower all used functions into HLIR code.
    #[inline]
    pub fn lower_code_starting_from(
        &mut self,
        entry_path: String,
    ) -> MontyResult<Vec<montyc_hlir::Function>> {
        if !entry_path.contains(":") {
            panic!("entry path must speciful at least one module.");
        }

        let mut store = self.value_store.borrow_mut();
        let (module_value, _mref) = match self
            .resolve_fancy_path_to_modules(entry_path.as_str())
            .as_slice()
        {
            [] => todo!("could not find a module matching the path {:?}", entry_path),
            [mref] => (store.get(mref.clone()), mref.clone()),
            [_, ..] => todo!("ambiguity in resolving module."),
        };

        let entry_function = entry_path.split(":").last().unwrap();

        let entry_function_value_index = {
            let entry_function_hash = self.const_runtime.borrow().hash(entry_function);

            let module = module_value.unwrap();
            let module_globals = module.properties();

            let (_, entry_function_index) = module_globals.get(entry_function_hash).unwrap();

            entry_function_index
        };

        let tcx = self.typing_context.borrow();
        let type_id = store.metadata(entry_function_value_index).type_id.unwrap();

        {
            let local_type_id = tcx.contextualize(type_id).unwrap();

            match local_type_id.as_python_type() {
                PythonType::Callable { args, ret } => match (args, ret.clone()) {
                    (Some(_), _) => todo!("main function can not accept arguments."),
                    (None, TypingContext::Int) | (None, TypingContext::None) => (),
                    (None, _) => todo!("main must return either None or int."),
                },

                _ => unimplemented!(),
            }
        }

        let entry_function = store
            .metadata(entry_function_value_index)
            .function
            .as_mut()
            .unwrap();

        todo!("emit hlir::Function's that wrap FlatSeq's with semantic metadata.");
    }
}

impl HostGlue for GlobalContext {
    fn str_to_spanref(&self, name: &str) -> SpanRef {
        self.spanner.str_to_spanref::<0>(name).unwrap()
    }

    fn spanref_to_str(&self, sref: SpanRef) -> &str {
        self.spanner
            .spanref_to_str(sref, |mref, range| {
                self.module_sources.get(&mref)?.get(range)
            })
            .expect("unable to translate spanref to string")
    }

    fn import_module(
        &mut self,
        path: &[SpanRef],
        base: Option<(usize, &Path)>,
    ) -> Vec<(ModuleRef, SpanRef)> {
        log::trace!(
            "[GlobalContext::import_module] Importing path={:?} with base={:?}",
            path,
            base
        );

        let path_as_str = path
            .iter()
            .cloned()
            .map(|sr| {
                self.spanner.spanref_to_str(sr, |mref, range| {
                    self.module_sources.get(&mref).and_then(|st| st.get(range))
                })
            })
            .map(|st| st.expect("SpanRef's should always be resolvable."))
            .collect::<Vec<_>>();

        match base {
            Some((level, base)) => {
                let mut import_path = ImportPath::new(base.into());

                match *path_as_str.as_slice() {
                    [] => unreachable!(),

                    [head] | [head, ..] if head == "__monty" => {
                        return vec![(ModuleRef(0), path[0])];
                    }

                    ref rest => {
                        let mut root = PathBuf::default();

                        for part in rest.iter() {
                            let final_path = match import_path.advance(part) {
                                Some(p) => p,
                                None => return vec![],
                            };

                            root = final_path.clone();
                        }

                        self.load_module_with(
                            root.clone(),
                            root.file_name().unwrap().to_string_lossy(),
                            |_gcx, mref| mref,
                        )
                        .unwrap();
                    }
                }

                todo!("{:#?}", level);
            }

            None => {
                let import = ImportPath::resolve_import_to_path(
                    path_as_str,
                    Some(self.opts.libstd()).into_iter(),
                );

                todo!("{:?}", import);
            }
        }
    }

    fn with_module_mut(
        &self,
        mref: ModuleRef,
        f: &mut dyn FnMut(&mut montyc_hlir::ModuleObject) -> interpreter::PyResult<()>,
    ) -> interpreter::PyResult<()> {
        let module = self.modules.get(mref).unwrap();
        let mut module = module.borrow_mut();

        f(&mut *module)
    }

    fn module_data(&self, mref: ModuleRef) -> Option<montyc_hlir::ModuleData> {
        Some(montyc_hlir::ModuleData::clone(
            self.modules.get(mref)?.borrow().data.as_ref(),
        ))
    }
}
