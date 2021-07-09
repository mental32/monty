use std::{
    cell::RefCell,
    collections::HashSet,
    io,
    iter::FromIterator,
    path::{Path, PathBuf},
};

use montyc_core::{utils::SSAMap, ModuleRef, MontyError, MontyResult, SpanRef};
use montyc_hlir::{
    interpreter::{self, HostGlue},
    typing::TypingContext,
    Value,
};
use montyc_parser::{ast, AstNode, SpanInterner};

use crate::{global::value_context::ValueContext, prelude::*, typechk::Typecheck};

use super::value_store::GlobalValueStore;

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

type StaticNames = ahash::AHashMap<SpanRef, &'static str>;

#[derive(Debug)]
pub struct ImportPath {
    base: PathBuf,
    curdir: PathBuf,
}

impl ImportPath {
    #[inline]
    pub fn new(base: PathBuf) -> Self {
        Self {
            base: base.clone(),
            curdir: base,
        }
    }

    #[inline]
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

#[derive(Debug)]
pub struct GlobalContext {
    /// The options this context was created with.
    opts: CompilerOptions,

    /// A Span interner shared between all parsing sessions.
    spanner: SpanInterner,

    /// Static names that get loaded at startup.
    static_names: StaticNames,

    /// A registry of the current modules that have been included.
    pub(super) modules: SSAMap<ModuleRef, RefCell<montyc_hlir::ModuleObject>>,

    /// A map of all the source code for every module imported.
    module_sources: ahash::AHashMap<ModuleRef, Box<str>>,

    /// An interpreter runtime for consteval.
    pub(super) const_runtime: RefCell<interpreter::Runtime>,

    /// Used to keep track of type information.
    typing_context: TypingContext,

    /// A global caching store for all processed values.
    pub(super) value_store: RefCell<GlobalValueStore>,
}

impl GlobalContext {
    pub fn initialize(VerifiedCompilerOptions(opts): &VerifiedCompilerOptions) -> Self {
        let mut gcx = Self {
            opts: opts.clone(),
            spanner: SpanInterner::new(),
            static_names: Default::default(),
            modules: SSAMap::new(),
            module_sources: Default::default(),
            const_runtime: RefCell::new(interpreter::Runtime::new(0x1000)),
            typing_context: TypingContext::initialized(),
            value_store: RefCell::new(GlobalValueStore::default()),
        };

        gcx.const_runtime.borrow_mut().initialize_monty_module(&gcx);

        gcx.modules.skip(1);

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
                .name_to_spanref::<0>(raw_name) // INVARIANT: ModuleRef(0) is reserved.
                .expect("Span interner was already mutably borrowed!");

            gcx.static_names.insert(name_ref, raw_name);
        }

        log::debug!(
            "[global_context:initialize] initialized {:?} static names",
            gcx.static_names.len()
        );

        let _ = gcx.include_module(opts.libstd().join("builtins.py"));

        gcx
    }

    #[inline]
    fn load_module_with<T>(
        &mut self,
        path: impl AsRef<Path>,
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

        let module = montyc_hlir::ModuleObject::new(path.to_path_buf(), module, mref);

        let _ = self.module_sources.insert(mref, source);
        let _ = self.modules.try_set_value(mref, module).unwrap();

        Ok(f(self, mref))
    }

    #[inline]
    pub fn include_module(&mut self, path: impl AsRef<Path>) -> MontyResult<ModuleRef> {
        self.load_module_with(path, |gcx, mref| -> MontyResult<ModuleRef> {
            let (module_index, object_graph) =
                gcx.const_runtime.borrow_mut().consteval(mref, gcx).unwrap();

            let (mref, properties) = match object_graph
                .node_weight(module_index)
                .expect("missing module index.")
            {
                Value::Module { mref, properties } => (*mref, properties),
                _ => unreachable!(),
            };

            // let module_object = gcx.modules.get(mref).unwrap();

            for value_idx in properties.iter_by_alloc_asc(&object_graph) {
                let value = object_graph.node_weight(value_idx).unwrap();

                value.typecheck(ValueContext {
                    mref,
                    gcx,
                    value,
                    value_idx,
                    object_graph: &object_graph,
                })?;
            }

            Ok(mref)
        })
        .map_err(|err| MontyError::IO(err))?
    }

    #[inline]
    pub fn import_module(
        &self,
        decl: ast::ImportDecl,
    ) -> Result<Vec<(ModuleRef, SpanRef)>, Vec<String>> {
        let qualnames = match decl.parent {
            ast::Import::Names(_) => vec![decl.name.inner.components()],
            ast::Import::From { module, names, .. } => {
                let base = module.inner.components();
                let mut leaves = Vec::with_capacity(names.len());

                for name in names {
                    leaves.push(base.clone());
                    leaves.last_mut().unwrap().extend(name.inner.components());
                }

                leaves
            }
        };

        let mut modules = HashSet::with_capacity(qualnames.len());

        let __monty = self.spanner.name_to_spanref::<0>("__monty").unwrap();

        for qualname in &qualnames {
            assert!(!qualname.is_empty(), "{:?}", qualname);

            if matches!(qualname.get(0), Some(ast::Atom::Name(name)) if name.group() == __monty.group())
            {
                modules.insert((ModuleRef(0), __monty));
                continue;
            }

            log::trace!(
                "[global_context:import_module] Trying to resolve qualname to filepath {:?}",
                qualname
            );

            unimplemented!();

            // let name = qualname.last().unwrap().as_name().unwrap();
            // let qualname: Vec<&str> = qualname
            //     .into_iter()
            //     .map(|atom| match atom {
            //         ast::Atom::Name((n, _)) => {
            //             mctx.source.get(self.span_ref.borrow().get(*n).unwrap()).unwrap()
            //         }
            //         _ => unreachable!(),
            //     })
            //     .collect();

            // // the magical `__monty` module name is special.
            // if matches!(qualname.as_slice(), ["__monty", ..]) {
            //     modules.push((ModuleRef("__monty".into()), name));
            //     continue;
            // }

            // let path = {
            //     let path = match ImportPath::resolve_import_to_path(qualname.clone(), std::iter::once(module_dir.clone())) {
            //         Some(p) => p,
            //         None => return Err(qualname.iter().map(ToString::to_string).collect()),
            //     };

            //     let module_ref = ModuleRef(path);

            //     if self.modules.contains_key(&module_ref) {
            //         modules.push((module_ref, name));
            //         continue;
            //     } else {
            //         module_ref.0
            //     }
            // };

            // log::trace!("import: Import module ({:?})", shorten(&path));

            // let source = match std::fs::read_to_string(&path) {
            //     Ok(st) => st.into_boxed_str(),
            //     Err(why) => {
            //         log::error!("Failed to read module contents! why={:?}", why);
            //         unreachable!();
            //     }
            // };

            // let module_ref = self.parse_and_register_module(source, path);

            // modules.push((module_ref, name));
        }

        Ok(Vec::from_iter(modules))
    }
}

impl HostGlue for GlobalContext {
    fn name_to_spanref(&self, name: &str) -> SpanRef {
        self.spanner.name_to_spanref::<0>(name).unwrap()
    }

    fn spanref_to_str(&self, sref: SpanRef) -> &str {
        self.spanner
            .spanref_to_name(sref, |mref, range| {
                self.module_sources.get(&mref)?.get(range)
            })
            .expect("unable to translate spanref to string")
    }

    fn import_module(&self, decl: ast::ImportDecl) -> Vec<(ModuleRef, SpanRef)> {
        self.import_module(decl).unwrap()
    }

    fn tcx(&self) -> &TypingContext {
        &self.typing_context
    }

    fn with_module(
        &self,
        mref: ModuleRef,
        f: &mut dyn FnMut(&montyc_hlir::ModuleObject) -> interpreter::PyResult<()>,
    ) -> interpreter::PyResult<()> {
        let module = self.modules.get(mref).unwrap();
        let module = module.borrow();

        f(&*module)
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
}
