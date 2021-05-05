use std::{
    cell::RefCell,
    collections::HashMap,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast::{
        atom::Atom,
        class::ClassDef,
        import::{Import, ImportDecl},
        module::Module,
        primary::Primary,
        stmt::Statement,
    },
    class::Class,
    database::AstDatabase,
    func::Function,
    phantom::PhantomObject,
    prelude::*,
    scope::ScopedObject,
    typing::{Generic, LocalTypeId, TypeDescriptor},
    CompilerOptions,
};

use super::{
    local::LocalContext, module::ModuleContext, resolver::InternalResolver, ModuleRef,
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

/// Used to track global compilation state per-compilation.
#[derive(Debug)]
pub struct GlobalContext {
    pub modules: HashMap<ModuleRef, ModuleContext>,
    pub functions: RefCell<Vec<Rc<Function>>>,
    pub span_ref: Rc<RefCell<SpanRef>>,
    pub type_map: Rc<TypeMap>,
    pub builtins: HashMap<LocalTypeId, (Rc<Class>, ModuleRef)>,
    pub libstd: PathBuf,
    pub resolver: Rc<InternalResolver>,
    pub database: AstDatabase,
    pub phantom_objects: Vec<Rc<PhantomObject>>,
}

const MAGICAL_NAMES: &str = include_str!("../magical_names.py");

impl From<CompilerOptions> for GlobalContext {
    fn from(opts: CompilerOptions) -> Self {
        log::debug!("Bootstrapping with {:?}", opts);

        let CompilerOptions { libstd, input: _ } = opts;

        let libstd = match libstd.canonicalize() {
            Ok(path) => path,
            Err(why) => {
                log::error!("Failed to canonicalize stdlib path! why={:?}", why);
                unreachable!();
            }
        };

        log::debug!("libstd path is set to => {:?}", libstd);

        let mut ctx = Self::default();

        ctx.libstd = libstd.clone();

        // pre-emptively load in core modules i.e. builtins, ctypes, and typing.

        // HACK: Synthesize a module so that our `SpanRef` string/ident/comment interner is aware of certain builtin
        //       method names this is necessary when resolving binary expressions using builtin types (since they have)
        //       no module, hence "builtin", the name resolution logic fails.
        ctx.load_module_literal(MAGICAL_NAMES, "__monty:magical_names", |_, _| {});

        let c_char_p = ctx
            .type_map
            .insert(TypeDescriptor::Generic(Generic::Pointer {
                inner: TypeMap::U8,
            }));

        ctx.phantom_objects.push(Rc::new(PhantomObject {
            name: ctx.magical_name_of("c_char_p").unwrap(),
            infer_type: |ctx| {
                Ok(ctx
                    .global_context
                    .type_map
                    .entry(TypeDescriptor::Generic(Generic::Pointer {
                        inner: TypeMap::U8,
                    })))
            },
        }));

        use cranelift_codegen::ir::InstBuilder;

        ctx.type_map
            .add_coercion_rule(c_char_p, TypeMap::NONE_TYPE, |_ctx, _value| todo!());

        ctx.type_map
            .add_coercion_rule(TypeMap::NONE_TYPE, c_char_p, |ctx, _value| {
                ctx.builder
                    .borrow_mut()
                    .ins()
                    .null(cranelift_codegen::ir::types::R64)
            });

        ctx.type_map
            .add_coercion_rule(TypeMap::STRING, c_char_p, |ctx, value| {
                use cranelift_module::Module;

                let data_id = ctx
                    .codegen_backend
                    .object_module
                    .borrow_mut()
                    .declare_data("string", cranelift_module::Linkage::Export, false, false)
                    .unwrap();

                let mut dctx = cranelift_module::DataContext::new();
                let st = ctx.codegen_backend.global_context.get_string_literal(value, ctx.func.scope.module_ref());

                let st = std::ffi::CString::new(st.as_str()).unwrap();
                let st = st.into_bytes_with_nul();
                let st = st.into_boxed_slice();

                dctx.define(st);

                let _ = ctx.codegen_backend.object_module.borrow_mut().define_data(data_id, &dctx).unwrap();

                let gv = ctx.codegen_backend.object_module.borrow_mut().declare_data_in_func(data_id, &mut ctx.builder.borrow_mut().func);

                let ptr = ctx.builder
                    .borrow_mut()
                    .ins()
                    .global_value(cranelift_codegen::ir::types::I64, gv);

                // ctx.builder.borrow_mut().ins().bitcast(cranelift_codegen::ir::types::R64, ptr)
                ptr
            });

        ctx.load_module(libstd.join("builtins.py"), |ctx, mref| {
            // The "builtins.py" module currently stubs and forward declares the compiler builtin types.
            //
            // This is necessary as most of the types are actually magical and the compiler decides what
            // implementation the user gets for any given usage i.e. a "str" in a C FFI function is a CString
            // but passing around and reading from a "str" will probably use a Copy-On-Write string slice.

            let module_context = ctx
                .modules
                .get(&mref)
                .cloned()
                .expect("failed to get pre-loaded module.");

            for item in module_context.scope.iter() {
                let object_unspanned = item.object.unspanned();

                // associate opaque class definitions of builtin types...
                if let Some(Statement::Class(class_def)) = object_unspanned.as_ref().downcast_ref() {
                    let (dec_name, dec_span) = match class_def.decorator_list.as_slice() {
                        [] => continue,
                        [dec] => (dec.reveal(&module_context.source).unwrap(), dec.span.clone()),
                        _ => panic!("Multiple decorators are not supported."),
                    };

                    if dec_name != "extern" {
                        module_context
                            .local_context(ctx)
                            .exit_with_error(
                                MontyError::Unsupported {
                                        span: dec_span,
                                        message: String::from("only `extern` decorators are supported here.")
                                    })
                    }

                    // assume "montyc" linkage

                    let klass_name = class_def.name.reveal(&module_context.source).unwrap();

                    let type_id = match klass_name {
                        "int" => TypeMap::INTEGER,
                        "float" => TypeMap::FLOAT,
                        "str" => TypeMap::STRING,
                        "bool" => TypeMap::BOOL,
                        st => panic!("unknown builtin {:?}", st),
                    };

                    let mut klass: Class =
                        item.with_context(ctx, |local, _| (&local, class_def).into());


                        macro_rules! const_prop {
                            ($prop:ident($reciever:expr) := ($($arg:expr),* $(,)?) -> $ret:expr) => ({
                                let span_ref = ctx.span_ref.borrow();
                                let $prop = span_ref.find(stringify!($prop), MAGICAL_NAMES);

                                let type_map = &ctx.type_map;

                                let func = FunctionType {
                                    reciever: Some($reciever),
                                    name: $prop.clone(),
                                    ret: $ret,
                                    args: vec![$($arg,)*],
                                    module_ref: ModuleRef(PathBuf::from("__monty:magical_names")),
                                };

                                klass.properties.insert($prop.unwrap(), type_map.insert(TypeDescriptor::Function(func)));
                            });
                        }

                    match type_id {
                        TypeMap::INTEGER => {
                            const_prop!(__add__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__sub__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__mul__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__div__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::FLOAT);
                        },

                        TypeMap::STRING => {
                            const_prop!(__add__(TypeMap::STRING) := (TypeMap::STRING) -> TypeMap::STRING);
                            const_prop!(__mul__(TypeMap::STRING) := (TypeMap::INTEGER) -> TypeMap::STRING);
                        }

                        TypeMap::BOOL => {
                            const_prop!(__add__(TypeMap::BOOL) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__sub__(TypeMap::BOOL) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__add__(TypeMap::BOOL) := (TypeMap::BOOL) -> TypeMap::INTEGER);
                            const_prop!(__sub__(TypeMap::BOOL) := (TypeMap::BOOL) -> TypeMap::INTEGER);
                        }

                        TypeMap::FLOAT => {
                            const_prop!(__add__(TypeMap::FLOAT) := (TypeMap::FLOAT) -> TypeMap::FLOAT);
                            const_prop!(__sub__(TypeMap::FLOAT) := (TypeMap::FLOAT) -> TypeMap::FLOAT);
                        }

                        _ => (),
                    };


                    let _ = ctx.builtins.insert(type_id, (Rc::new(klass), mref.clone()));

                    log::trace!(
                        "\tAssociated builtin with class definition! {:?}",
                        klass_name
                    );
                }
                // run regular import machinery...
                else if let Some(Statement::Import(import)) =
                    (object_unspanned.as_ref()).downcast_ref()
                {
                    let this = Rc::new(import.clone());

                    let source = module_context.source.as_ref();

                    for decl in this.decls() {
                        ctx.import_module(decl, source);
                    }
                }

                item.with_context(ctx, |local, object| {
                    object.typecheck(&local).unwrap_or_compiler_error(&local);
                })
            }
        });

        ctx
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        let span_ref: Rc<RefCell<SpanRef>> = Default::default();
        let type_map: Rc<TypeMap> = Rc::new(TypeMap::correctly_initialized());

        let resolver = Rc::new(InternalResolver {
            span_ref: span_ref.clone(),
            sources: Default::default(),
            type_map: type_map.clone(),
        });

        Self {
            modules: HashMap::new(),
            functions: RefCell::new(Vec::new()),
            span_ref,
            type_map,
            builtins: HashMap::new(),
            phantom_objects: vec![],
            libstd: PathBuf::default(),
            resolver,
            database: Default::default(),
        }
    }
}

impl GlobalContext {
    pub fn is_builtin(&self, t: &dyn AstObject, t_mref: &ModuleRef) -> Option<LocalTypeId> {
        log::trace!(
            "context:is_builtin: checking if object is a builtin ({:?})",
            t.name()
        );

        let t_mref = t.renamed_properties().unwrap_or(t_mref.clone());
        let t_mref = &t_mref;

        let t_mctx = self.modules.get(t_mref).unwrap();

        let st = self
            .span_ref
            .borrow()
            .resolve_ref(t.name(), t_mctx.source.as_ref())
            .unwrap();

        for (type_id, (object, mref)) in self.builtins.iter() {
            let object_name = Some(object.scope.root())
                .as_ref()
                .and_then(|root| match root {
                    ScopeRoot::AstObject(obj) => Some(obj),
                    _ => None,
                })
                .and_then(|obj| obj.as_ref().downcast_ref::<ClassDef>())
                .and_then(|class_def| class_def.name());

            if t_mref == mref && t.is_named(object_name) {
                return Some(type_id.clone());
            } else if t_mref != mref {
                let builtin_mctx = self.modules.get(mref).unwrap();
                let builtin_st = self
                    .span_ref
                    .borrow()
                    .resolve_ref(object_name, builtin_mctx.source.as_ref())
                    .unwrap();

                if builtin_st == st {
                    return Some(type_id.clone());
                }
            }
        }

        None
    }

    pub fn get_class_from_module(&self, mref: ModuleRef, name: SpanEntry) -> Option<Rc<Class>> {
        let mctx = self.modules.get(&mref)?;

        for obj in mctx.scope.iter() {
            let o = obj.object.unspanned();

            if let Some(Statement::Class(class_def)) = (o.as_ref()).downcast_ref() {
                if class_def.is_named(name) {
                    let klass =
                        obj.with_context(self, |local, _this| Class::from((&local, class_def)));

                    return Some(Rc::new(klass));
                }
            }
        }

        None
    }

    pub fn get_string_literal(&self, value: Rc<dyn AstObject>, mref: ModuleRef) -> String {
        let span = value.span().unwrap();
        let mctx = self.modules.get(&mref).unwrap();

        mctx.source.get((span.start + 1)..(span.end - 1)).unwrap().to_string()
    }

    pub fn access_from_module(
        &self,
        module: &Rc<Spanned<Primary>>,
        item: &Rc<Spanned<Primary>>,
        source: &str,
        mref: &ModuleRef,
    ) -> Option<Rc<dyn AstObject>> {
        let parts = module.inner.components();

        let qualname: Vec<&str> = parts
            .into_iter()
            .map(|atom| match atom {
                Atom::Name(n) => self.span_ref.borrow().resolve_ref(n, source).unwrap(),
                _ => unreachable!(),
            })
            .collect();

        match qualname.as_slice() {
            [] => unreachable!(),
            ["__monty"] => {
                // its a magical builtin
                let item = item.inner.components();

                let item = if let [atom] = item.as_slice() {
                    match atom {
                        Atom::Name(n) => n.unwrap(),
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!();
                };

                self.phantom_objects
                    .iter()
                    .find(|obj| {
                        self.name_eq((obj.name, &"__monty:magical_names".into()), (item, mref))
                    })
                    .map(|obj| Rc::clone(&obj) as Rc<_>)
            }

            ["__monty", ..] => unreachable!(),

            _ => {
                let path = self.resolve_import_to_path(qualname)?;
                let mref = ModuleRef(path);

                let _mctx = self.modules.get(&mref)?;

                todo!();
            }
        }
    }

    pub fn resolve_module(&self, _module: &Rc<Spanned<Primary>>) -> Option<Rc<dyn AstObject>> {
        todo!()
    }

    pub fn magical_name_of(&self, st: impl AsRef<str>) -> Option<NonZeroUsize> {
        let name_ref = ModuleRef(PathBuf::from(format!("__monty:magical_names")));
        let module = self.resolver.sources.get(&name_ref).unwrap();

        self.span_ref.borrow().find(st.as_ref(), &module)
    }

    pub fn name_eq(
        &self,
        lhs: (NonZeroUsize, &ModuleRef),
        rhs: (NonZeroUsize, &ModuleRef),
    ) -> bool {
        if lhs.1 == rhs.1 && lhs.0 == rhs.0 {
            return true;
        }

        let lmsrc = self.resolver.sources.get(lhs.1).unwrap();
        let rmsrc = self.resolver.sources.get(rhs.1).unwrap();

        self.span_ref
            .borrow()
            .resolve_ref(Some(lhs.0), &lmsrc.value())
            .and_then(|left| {
                self.span_ref
                    .borrow()
                    .resolve_ref(Some(rhs.0), &rmsrc.value())
                    .map(|right| left == right)
            })
            .unwrap_or(false)
    }

    fn load_module_literal(&mut self, source: &str, path: &str, f: impl Fn(&mut Self, ModuleRef)) {
        log::debug!("Loading module ({:?})", path);

        let module = self.parse_and_register_module(source.to_string().into_boxed_str(), path);

        f(self, module);

        log::debug!("Finished loading module ({:?})", path);
    }

    pub fn load_module(&mut self, path: impl AsRef<Path>, f: impl Fn(&mut Self, ModuleRef)) {
        let path = path.as_ref();

        log::debug!("Loading module ({:?})", shorten(path));

        let source = match std::fs::read_to_string(&path) {
            Ok(st) => st.into_boxed_str(),
            Err(why) => {
                log::error!("Failed to read module contents! why={:?}", why);
                unreachable!();
            }
        };

        let module = self.parse_and_register_module(source, path);

        f(self, module);

        log::debug!("Finished loading module ({:?})", shorten(path));
    }

    fn resolve_import_to_path(&self, qualname: Vec<&str>) -> Option<PathBuf> {
        fn search(curdir: &Path, expected: &str) -> Option<PathBuf> {
            curdir
                .read_dir()
                .ok()?
                .filter_map(|maybe_entry| {
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

                    ok.then_some(path)
                })
                .next()
        }

        let paths_to_inspect: &[PathBuf] = &[PathBuf::from("."), self.libstd.clone()] as &[_];

        'outer: for path in paths_to_inspect.iter() {
            let mut root = path.to_owned();

            for (_idx, part) in qualname.iter().enumerate() {
                let final_path = match search(&root, part) {
                    Some(p) => p,
                    None => continue 'outer,
                };

                root = final_path.clone();
            }

            return Some(root);
        }

        None
    }

    fn import_module(&mut self, decl: ImportDecl, source: &str) -> Option<Vec<ModuleRef>> {
        let qualnames = match decl.parent.as_ref() {
            Import::Names(_) => vec![decl.name.inner.components()],
            Import::From { module, names, .. } => {
                let base = module.inner.components();
                let mut leaves = Vec::with_capacity(names.len());

                for name in names {
                    leaves.push(base.clone());
                    leaves.last_mut().unwrap().extend(name.inner.components());
                }

                leaves
            }
        };

        let mut modules = Vec::with_capacity(qualnames.len());

        for qualname in &qualnames {
            let qualname: Vec<&str> = qualname
                .into_iter()
                .map(|atom| match atom {
                    Atom::Name(n) => self.span_ref.borrow().resolve_ref(*n, source).unwrap(),
                    _ => unreachable!(),
                })
                .collect();

            // the magical `__monty` module name is special.
            if matches!(qualname.as_slice(), ["__monty", ..]) {
                modules.push(ModuleRef("__monty".into()));
                continue;
            }

            let path = {
                let module_ref = ModuleRef(self.resolve_import_to_path(qualname)?);

                if self.modules.contains_key(&module_ref) {
                    modules.push(module_ref);
                    continue;
                } else {
                    module_ref.0
                }
            };

            log::trace!("Importing module ({:?})", shorten(&path));

            let source = match std::fs::read_to_string(&path) {
                Ok(st) => st.into_boxed_str(),
                Err(why) => {
                    log::error!("Failed to read module contents! why={:?}", why);
                    unreachable!();
                }
            };

            modules.push(self.parse_and_register_module(source, path));
        }

        Some(modules)
    }

    fn parse<T>(&self, s: Rc<str>) -> T
    where
        T: Parseable + Clone,
    {
        let Spanned { inner, .. }: Spanned<T> = (s, self.span_ref.clone()).into();
        inner
    }

    fn parse_and_register_module<P>(&mut self, input: Box<str>, path: P) -> ModuleRef
    where
        P: Into<PathBuf>,
    {
        let source: Rc<_> = input.into();
        let module: Module = self.parse(Rc::clone(&source));

        let path = path.into();

        self.register_module(module, path, source)
    }

    pub fn walk(
        &self,
        module_ref: ModuleRef,
    ) -> impl Iterator<Item = (Rc<dyn AstObject>, LocalContext)> {
        let module_context = self.modules.get(&module_ref).unwrap();
        let mut it = module_context.scope.iter();

        std::iter::from_fn(move || {
            let ScopedObject { scope, object } = it.next()?;

            let ctx = LocalContext {
                global_context: self,
                module_ref: module_ref.clone(),
                scope,
                this: Some(Rc::clone(&object)),
            };

            Some((object, ctx))
        })
    }

    fn register_module(&mut self, module: Module, path: PathBuf, source: Rc<str>) -> ModuleRef {
        let module = Rc::new(module);
        let key = ModuleRef::from(path.clone());

        let mut scope = OpaqueScope::from(module.clone() as Rc<dyn AstObject>);
        let _ = scope.module_ref.replace(key.clone());

        let scope = Rc::new(scope) as Rc<dyn Scope>;

        self.resolver.sources.insert(key.clone(), source.clone());

        if let Some(previous) = self.modules.insert(
            key.clone(),
            ModuleContext {
                module,
                path,
                scope,
                source,
                flags: ModuleFlags::EMPTY,
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
