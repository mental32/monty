use std::{
    cell::RefCell,
    collections::HashMap,
    hash::Hash,
    path::{Path, PathBuf},
    rc::Rc,
};

use log::*;

use crate::{
    ast::{
        atom::Atom, class, module::Module, primary::Primary, stmt::Statement, AstObject, Spanned,
    },
    func::Function,
    parser::{Parseable, SpanRef},
    scope::{downcast_ref, LocalScope, OpaqueScope, Scope, ScopedObject},
    typing::{LocalTypeId, TypeMap},
    CompilerOptions,
};

fn shorten(path: &Path) -> String {
    let mut c = path
        .components()
        .rev()
        .take(2)
        .map(|c| format!("{}", c.as_os_str().to_string_lossy()))
        .collect::<Vec<_>>();

    c.reverse();
    c.join("/")
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
pub struct ModuleRef(PathBuf);

/// Used to track global compilation state per-compilation.
#[derive(Debug)]
pub struct GlobalContext {
    pub modules: HashMap<ModuleRef, ModuleContext>,
    pub functions: Vec<Function>,
    pub span_ref: Rc<RefCell<SpanRef>>,
    pub type_map: RefCell<TypeMap>,
    pub builtins: HashMap<LocalTypeId, Rc<dyn AstObject>>,
}

impl From<CompilerOptions> for GlobalContext {
    fn from(opts: CompilerOptions) -> Self {
        debug!("Bootstrapping with {:?}", opts);

        let CompilerOptions { libstd, input } = opts;

        let libstd = match libstd.canonicalize() {
            Ok(path) => path,
            Err(why) => {
                error!("Failed to canonicalize stdlib path! why={:?}", why);
                unreachable!();
            }
        };

        info!("libstd path is set to => {:?}", libstd);

        let mut ctx = Self::default();

        // pre-emptively load in core modules i.e. builtins, ctypes, and typing.

        ctx.preload_module(libstd.join("builtins.py"), |ctx, mref| {
            // The "builtins.py" module currently stubs and forward declares the compiler builtin types.
            //
            // This is necessary as most of the types are actually magical and the compiler decides what
            // implementation the user gets for any given usage i.e. a "str" in a C FFI function is a CString
            // but passing around and reading from a "str" will probably use a Copy-On-Write string slice.

            let module_context = ctx
                .modules
                .get(&mref)
                .expect("failed to get pre-loaded module.");

            for item in module_context.scope.iter() {
                let object_original = item.object.clone();
                let object_unspanned = item.object.unspanned();

                if let Some(Statement::Class(class_def)) = downcast_ref(object_unspanned.as_ref()) {
                    let dec_name = match class_def.decorator_list.as_slice() {
                        [] => continue,
                        [dec] => dec.reveal(&module_context.source).unwrap(),
                        _ => panic!("Multiple decorators are not supported."),
                    };

                    assert_eq!(
                        dec_name, "@extern",
                        "only `@extern` (opaque type decorators) are supported."
                    );

                    let klass_name = class_def.name.reveal(&module_context.source).unwrap();

                    match klass_name {
                        "int" => ctx.builtins.insert(TypeMap::INTEGER, object_original),
                        "float" => ctx.builtins.insert(TypeMap::FLOAT, object_original),
                        "str" => ctx.builtins.insert(TypeMap::STRING, object_original),
                        "bool" => ctx.builtins.insert(TypeMap::BOOL, object_original),
                        st => panic!("unknown builtin {:?}", st),
                    };

                    trace!("\tAssociated builtin with class definition! {:?}", klass_name);
                }
            }
        });

        ctx
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            modules: HashMap::new(),
            functions: Vec::new(),
            span_ref: Default::default(),
            type_map: RefCell::new(TypeMap::new()),
            builtins: HashMap::new(),
        }
    }
}

impl GlobalContext {
    fn preload_module(&mut self, path: impl AsRef<Path>, f: impl Fn(&mut Self, ModuleRef)) {
        let path = path.as_ref();

        debug!("Preloading module ({:?})", shorten(path));

        let source = match std::fs::read_to_string(&path) {
            Ok(st) => st,
            Err(why) => {
                error!("Failed to read module contents! why={:?}", why);
                unreachable!();
            }
        };

        let module = self.parse_and_register_module(source, path);

        f(self, module);

        debug!("Finished preloading module ({:?})", shorten(path));
    }

    pub fn parse<T, S>(&self, s: S) -> T
    where
        S: AsRef<str>,
        T: Parseable + Clone,
    {
        let Spanned { inner, .. }: Spanned<T> = (s, self.span_ref.clone()).into();
        inner
    }

    pub fn parse_and_register_module<S, P>(&mut self, source: S, path: P) -> ModuleRef
    where
        S: AsRef<str>,
        P: Into<PathBuf>,
    {
        let module: Module = self.parse(&source);
        self.register_module(module, path.into(), source.as_ref().to_string())
    }

    pub fn walk(
        &self,
        module_ref: ModuleRef,
    ) -> impl Iterator<Item = (Rc<dyn AstObject>, LocalContext)> {
        let module_context = self.modules.get(&module_ref).unwrap();
        let mut it = module_context.scope.iter();

        std::iter::from_fn(move || {
            let scoped = it.next()?;

            let object = scoped.object.unspanned();
            let ctx = scoped.make_local_context(module_ref.clone(), self);

            Some((object, ctx))
        })
    }

    pub fn register_module(&mut self, module: Module, path: PathBuf, source: String) -> ModuleRef {
        let module = Rc::new(module);
        let key = ModuleRef::from(path.clone());

        let scope = OpaqueScope::from(module.clone() as Rc<dyn AstObject>);
        let scope = Box::new(scope) as Box<dyn Scope>;

        if let Some(previous) = self.modules.insert(
            key.clone(),
            ModuleContext {
                module,
                path,
                scope,
                source,
            },
        ) {
            panic!(
                "Overwrote previously registered module {:?} -> {:?}",
                key, previous
            );
        }

        key
    }
}

#[derive(Debug)]
pub struct ModuleContext {
    pub path: PathBuf,
    pub module: Rc<Module>,
    pub scope: Box<dyn Scope>,
    pub source: String,
}

impl ModuleContext {
    pub fn make_local_context<'a>(&'a self, global_context: &'a GlobalContext) -> LocalContext<'a> {
        LocalContext {
            global_context,
            module_ref: ModuleRef::from(self.path.clone()),
            scope: self.scope.as_ref(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LocalContext<'a> {
    pub global_context: &'a GlobalContext,
    pub module_ref: ModuleRef,
    pub scope: &'a dyn Scope,
}
