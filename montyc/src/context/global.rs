use std::{cell::RefCell, collections::HashMap, convert::TryInto, num::NonZeroUsize, path::{Path, PathBuf}, rc::Rc};

use crate::codegen::CodegenModule;
use crate::import::ImportPath;

use cranelift_codegen::{
    ir::{self, condcodes::IntCC},
    isa::CallConv,
};

use cranelift_module::{DataContext, Linkage};
use dashmap::DashMap;

use crate::{
    ast::{
        atom::{Atom, StringRef},
        expr::Expr,
        import::{Import, ImportDecl},
        module::Module,
        primary::Primary,
        stmt::Statement,
    },
    class::Class,
    codegen::{context::CodegenLowerArg, pointer::Pointer, TypePair, TypedValue},
    database::{DefId, ObjectDatabase},
    func::Function,
    parser::SpanInterner,
    phantom::PhantomObject,
    prelude::*,
    scope::ScopedObject,
    typing::{Generic, LocalTypeId, TypeDescriptor},
    CompilerOptions, VerifiedCompilerOptions,
};

use super::{local::LocalContext, module::ModuleContext, resolver::InternalResolver, ModuleRef};

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

#[derive(Debug)]
pub struct StringData {
    st_ref: StringRef,
    pub mref: ModuleRef,
    resolver: Rc<InternalResolver>,
}

impl ToString for StringData {
    fn to_string(&self) -> String {
        let span = self
            .resolver
            .span_ref
            .borrow()
            .get(self.st_ref.into())
            .unwrap();

        self.resolver
            .sources
            .get(&self.mref)
            .and_then(|st| st.get(span).map(|st| st.to_string()))
            .unwrap()
    }
}

pub type RawBuiltinFn = for<'a> fn(
    CodegenLowerArg<'_, '_, '_>,
    &[crate::codegen::TypedValue],
) -> Option<crate::codegen::TypedValue>;

pub struct BuiltinFunction(pub RawBuiltinFn);

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{{builtin function}}")
    }
}

/// Used to track global compilation state per-compilation.
#[derive(Debug)]
pub struct GlobalContext {
    pub opts: CompilerOptions,
    pub modules: DashMap<ModuleRef, ModuleContext>,
    pub functions: RefCell<Vec<Rc<Function>>>,
    pub span_ref: Rc<RefCell<SpanInterner>>,
    pub type_map: TypeMap,
    pub builtins: HashMap<LocalTypeId, (Rc<Class>, ModuleRef)>,
    pub builtin_functions: HashMap<NonZeroUsize, (Rc<Function>, BuiltinFunction)>,
    pub libstd: PathBuf,
    pub resolver: Rc<InternalResolver>,
    pub database: ObjectDatabase,
    pub phantom_objects: Vec<Rc<PhantomObject>>,
    pub strings: DashMap<StringRef, StringData>,
    pub branches: RefCell<HashMap<DefId, HashMap<DefId, Vec<LocalTypeId>>>>,
}

const MAGICAL_NAMES: &str = include_str!("../magical_names.py");

impl GlobalContext {
    fn new_uninit() -> Self {
        let span_ref = Rc::new(RefCell::new(SpanInterner::default()));
        let type_map = TypeMap::correctly_initialized();

        let resolver = Rc::new(InternalResolver {
            span_ref: Rc::clone(&span_ref),
            sources: Default::default(),
        });

        Self {
            opts: CompilerOptions::default(),
            modules: DashMap::new(),
            functions: RefCell::new(Vec::new()),
            span_ref,
            type_map,
            builtins: HashMap::new(),
            builtin_functions: HashMap::new(),
            phantom_objects: vec![],
            libstd: PathBuf::default(),
            resolver,
            database: Default::default(),
            branches: RefCell::new(HashMap::new()),
            strings: DashMap::new(),
        }
    }

    pub fn initialize(VerifiedCompilerOptions(opts): &VerifiedCompilerOptions) -> Self {
        log::debug!("Bootstrapping with: {:#?}", opts);

        let CompilerOptions {
            libstd, input: _, ..
        } = opts;

        let libstd = match libstd.canonicalize() {
            Ok(path) => path,
            Err(why) => {
                log::error!("Failed to canonicalize stdlib path! why={:?}", why);
                unreachable!();
            }
        };

        log::debug!("libstd path is set to => {:?}", libstd);

        let mut ctx = Self::new_uninit();

        ctx.libstd = libstd.clone();
        ctx.opts = opts.clone();

        // pre-emptively load in core modules i.e. builtins, ctypes, and typing.

        // HACK: Synthesize a module so that our `SpanRef` string/ident/comment interner is aware of certain builtin
        //       method names this is necessary when resolving binary expressions using "builtin" types (since they have)
        //       no module, hence "builtin", the name resolution logic fails.
        let _ = ctx.parse_and_register_module(
            MAGICAL_NAMES.to_string().into_boxed_str(),
            "__monty:magical_names",
        );

        let c_char_p = ctx
            .type_map
            .insert(TypeDescriptor::Generic(Generic::Pointer {
                inner: TypeMap::U8,
            }));

        ctx.phantom_objects.push(Rc::new(PhantomObject::new(
            ctx.magical_name_of("c_char_p").unwrap(),
            |ctx| {
                Ok(ctx
                    .global_context
                    .type_map
                    .entry(TypeDescriptor::Generic(Generic::Pointer {
                        inner: TypeMap::U8,
                    })))
            },
        )));

        use cranelift_codegen::ir::InstBuilder;

        ctx.type_map
            .add_coercion_rule(TypeMap::NONE_TYPE, c_char_p, |(ctx, builder), _value| {
                let zero = builder.ins().iconst(cranelift_codegen::ir::types::I64, 0);

                let c_char_p =
                    ctx.codegen_backend
                        .global_context
                        .type_map
                        .insert(TypeDescriptor::Generic(Generic::Pointer {
                            inner: TypeMap::U8,
                        }));

                TypedValue::by_val(
                    zero,
                    TypePair(c_char_p, Some(cranelift_codegen::ir::types::I64)),
                )
            });

        ctx.type_map.add_coercion_rule(
            TypeMap::UNKNOWN,
            TypeMap::OBJECT,
            |(_, _builder), value| value,
        );

        ctx.type_map
            .add_coercion_rule(TypeMap::STRING, c_char_p, |_, value| value);

        ctx.type_map
            .add_coercion_rule(TypeMap::BOOL, TypeMap::INTEGER, |(ctx, builder), value| {
                let bty = ctx.codegen_backend.scalar_type_of(TypeMap::INTEGER);
                let x = value.into_raw(builder);
                let b = builder.ins().bint(bty, x);

                TypedValue::by_val(b, TypePair(TypeMap::INTEGER, Some(bty)))
            });

        ctx.type_map
            .add_coercion_rule(TypeMap::INTEGER, TypeMap::BOOL, |(ctx, builder), value| {
                let bty = ctx.codegen_backend.scalar_type_of(TypeMap::BOOL);
                let value = value.into_raw(builder);
                let res = builder.ins().icmp_imm(IntCC::Equal, value, 1);
                TypedValue::by_val(res, TypePair(TypeMap::BOOL, Some(bty)))
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
                .map(|refm| refm.value().clone())
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
                            .unbound_local_context(ctx)
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
                        "type" => TypeMap::TYPE,
                        "object" => TypeMap::OBJECT,
                        st => panic!("unknown builtin {:?}", st),
                    };

                    let klass_type = class_def.as_type_descriptor(type_id, mref.clone());
                    let klass_type = ctx.type_map.entry(klass_type);

                    let klass_scope = LocalScope::from(class_def.clone());

                    let mut klass = Class {
                        scope: klass_scope,
                        kind: type_id,
                        type_id: klass_type,
                        name: class_def.name().unwrap(),
                        properties: HashMap::default(),
                    };

                    macro_rules! const_prop {
                        ($prop:ident($reciever:expr) := ($($arg:expr),* $(,)?) -> $ret:expr) => ({
                            let span_ref = ctx.span_ref.borrow();
                            let $prop = span_ref.find(stringify!($prop)).unwrap();

                            let type_map = &ctx.type_map;

                            let func = FunctionType {
                                reciever: Some($reciever),
                                name: ($prop.clone(), $prop.clone()),
                                ret: $ret,
                                args: vec![$($arg,)*],
                                module_ref: ModuleRef(PathBuf::from("__monty:magical_names")),
                            };

                            klass.properties.insert($prop, type_map.insert(TypeDescriptor::Function(func)));
                        });
                    }

                    match type_id {
                        TypeMap::INTEGER => {
                            const_prop!(__add__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__sub__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__mul__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__div__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::FLOAT);
                            const_prop!(__eq__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::BOOL);
                            const_prop!(__ne__(TypeMap::INTEGER) := (TypeMap::INTEGER) -> TypeMap::BOOL);
                        },

                        TypeMap::STRING => {
                            const_prop!(__add__(TypeMap::STRING) := (TypeMap::STRING) -> TypeMap::STRING);
                            const_prop!(__mul__(TypeMap::STRING) := (TypeMap::INTEGER) -> TypeMap::STRING);
                            const_prop!(__eq__(TypeMap::STRING) := (TypeMap::STRING) -> TypeMap::BOOL);
                        }

                        TypeMap::BOOL => {
                            const_prop!(__add__(TypeMap::BOOL) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__sub__(TypeMap::BOOL) := (TypeMap::INTEGER) -> TypeMap::INTEGER);
                            const_prop!(__add__(TypeMap::BOOL) := (TypeMap::BOOL) -> TypeMap::INTEGER);
                            const_prop!(__sub__(TypeMap::BOOL) := (TypeMap::BOOL) -> TypeMap::INTEGER);
                            const_prop!(__eq__(TypeMap::BOOL) := (TypeMap::BOOL) -> TypeMap::BOOL);
                        }

                        TypeMap::FLOAT => {
                            const_prop!(__add__(TypeMap::FLOAT) := (TypeMap::FLOAT) -> TypeMap::FLOAT);
                            const_prop!(__sub__(TypeMap::FLOAT) := (TypeMap::FLOAT) -> TypeMap::FLOAT);
                            const_prop!(__eq__(TypeMap::FLOAT) := (TypeMap::FLOAT) -> TypeMap::BOOL);
                        }

                        _ => (),
                    };


                    let _ = ctx.builtins.insert(type_id, (Rc::new(klass), mref.clone()));

                    let klass_entry = ctx.database.entry(Rc::clone(&item.object), &mref);
                    let id = ctx.database.id_of(&klass_entry).unwrap();

                    let _ = ctx.database.set_type_of(id, klass_type);

                    log::trace!(
                        "\tAssociated builtin with class definition! {:?}",
                        klass_name
                    );
                }
                else if let Some(funcdef) = crate::isinstance!(object_unspanned.as_ref(), Statement, Statement::FnDef(f) => crate::func::is_externaly_defined(f, ctx, &mref).then_some(f)).flatten() {
                    let lctx = LocalContext {
                        global_context: ctx,
                        module_ref: mref.clone(),
                        scope: item.scope.clone(),
                        current_branch: None,
                        this: Some(item.object.clone()),
                    };

                    let func = Function::new(&item.object, &lctx).unwrap();

                    let name = funcdef.name.reveal(&module_context.source).unwrap();

                    let builtin_fn: RawBuiltinFn = match name {
                        "id" => |(ctx, fx), args| {
                            assert_eq!(args.len(), 1);
                            let arg = &args[0];

                            let raw = arg.ref_value(fx);
                            let ty = ctx.codegen_backend.scalar_type_of(TypeMap::INTEGER);

                            Some(TypedValue::by_val(raw, TypePair(TypeMap::INTEGER, Some(ty))))
                        },

                        "isinstance" => |(ctx, fx), args| {
                            if let [subject, kind] = args {

                                let t = ctx
                                    .codegen_backend
                                    .global_context
                                    .type_map
                                    .get_tagged::<Generic>(subject.kind().0)
                                    .map(|r| {
                                        r.map(|r| {
                                            match r.inner {
                                                Generic::Pointer { inner } => ctx.codegen_backend.global_context.type_map.get_tagged(inner).unwrap().unwrap(),
                                                _ => r
                                            }
                                        })
                                    });

                                let res = match t {
                                    Some(Ok(TaggedType { inner, type_id: _ })) =>  {

                                        if let Generic::Struct { inner } = inner {
                                            match inner.as_slice() {
                                                [TypeMap::I64, maybe_union] if ctx.codegen_backend.global_context.type_map.is_union(*maybe_union) => {
                                                    let ptr = if subject.kind().1 == Some(ir::types::I64) {
                                                        Pointer::new(subject.clone().deref_into_raw(fx))
                                                    } else {
                                                        subject.as_ptr()
                                                    };

                                                    let union_tag = ptr.load(ir::types::I64, 0, fx);

                                                    let k = kind.clone().into_raw(fx);
                                                    let r = fx.ins().icmp(IntCC::Equal, union_tag, k);

                                                    TypedValue::by_val(r, TypePair(TypeMap::BOOL, Some(ctx.codegen_backend.scalar_type_of(TypeMap::BOOL))))
                                                },

                                                _ => unimplemented!(),
                                            }
                                        } else {
                                            todo!()
                                        }
                                    },

                                    _ => {
                                        let tyid = subject.kind().0;
                                        let tyid = fx.ins().iconst(ctx.codegen_backend.scalar_type_of(tyid), tyid.as_usize() as i64);

                                        let k = kind.clone().into_raw(fx);

                                        let r = fx.ins().icmp(IntCC::Equal, tyid, k);

                                        TypedValue::by_val(r, TypePair(TypeMap::BOOL, Some(ctx.codegen_backend.scalar_type_of(TypeMap::BOOL))))
                                    }
                                };

                                Some(res)

                            } else {
                                unreachable!("expected exactly two arguments to `isinstance` builtin.");
                            }
                        },

                        _ => todo!(),
                    };

                    let _ = ctx.database.set_type_of(func.def_id, func.kind.type_id);
                    let _ = ctx.builtin_functions.insert(funcdef.name().unwrap(), (func, BuiltinFunction(builtin_fn)));

                    continue;  // skip the typechecking call below as it'll include this decl for codegen.
                }
                // run regular import machinery...
                else if let Some(Statement::Import(import)) =
                    (object_unspanned.as_ref()).downcast_ref()
                {
                    let this = Rc::new(import.clone());

                    for decl in this.decls() {
                        ctx.import_module(decl, &module_context).unwrap();
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

impl GlobalContext {
    pub fn register_string_literal(&self, atom: &Spanned<Atom>, mref: ModuleRef) -> StringRef {
        let st_ref: StringRef = atom.inner.clone().try_into().unwrap();

        let _ = self.strings.insert(
            st_ref,
            StringData {
                st_ref,
                mref,
                resolver: Rc::clone(&self.resolver),
            },
        );

        st_ref
    }

    pub fn is_builtin(&self, t: &dyn AstObject) -> Option<(LocalTypeId, LocalTypeId)> {
        log::trace!(
            "global_context:is_builtin: checking if object is a builtin ({:?})",
            t.name()
        );

        let t_name = t.name().unwrap();

        for (type_id, (object, _)) in self.builtins.iter() {
            if self.span_ref.borrow().crosspan_eq(object.name, t_name) {
                return Some((type_id.clone(), object.type_id));
            }
        }

        None
    }

    pub fn get_class_from_module(&self, mref: ModuleRef, name: SpanRef) -> Option<Rc<Class>> {
        let mctx = self.modules.get(&mref)?;

        for obj in mctx.scope.iter() {
            let o = obj.object.unspanned();

            if let Some(Statement::Class(class_def)) = (o.as_ref()).downcast_ref() {
                if class_def.is_named(name) {
                    let klass =
                        obj.with_context(self, |local, _this| Class::new(&local, class_def));

                    return Some(Rc::new(klass));
                }
            }
        }

        None
    }

    pub fn get_string_literal(&self, value: Rc<dyn AstObject>, mref: ModuleRef) -> String {
        let span = value.span().unwrap();

        match self.modules.get(&mref) {
            Some(mctx) => mctx
                .source
                .get((span.start + 1)..(span.end - 1))
                .unwrap()
                .to_string(),

            None => mref.to_string(),
        }
    }

    pub fn access_from_module(
        &self,
        module: &Rc<Spanned<Primary>>,
        item: &Rc<Spanned<Primary>>,
        _source: &str,
        mref: &ModuleRef,
    ) -> Option<Rc<dyn AstObject>> {
        let parts = module.inner.components();

        let qualname: Vec<String> = parts
            .into_iter()
            .map(|atom| match atom {
                Atom::Name((n, _)) => self.resolver.resolve(mref.clone(), n).unwrap(),
                _ => unreachable!(),
            })
            .collect();

        let qualname: Vec<_> = qualname.iter().map(|st| st.as_str()).collect();

        match qualname.as_slice() {
            [] => unreachable!(),
            ["__monty"] => {
                // its a magical builtin
                let item = item.inner.components();

                let (item, _) = if let [atom] = item.as_slice() {
                    match atom {
                        Atom::Name(n) => n.clone(),
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!();
                };

                self.phantom_objects
                    .iter()
                    .find(|obj| self.span_ref.borrow().crosspan_eq(obj.name.1, item))
                    .map(|obj| Rc::clone(&obj) as Rc<_>)
            }

            ["__monty", ..] => unreachable!(),

            _ => {
                let path =
                    self.resolve_import_to_path(qualname, std::iter::once(mref.0.clone()))?;

                let mref = ModuleRef(path);

                let _mctx = self.modules.get(&mref)?;

                todo!();
            }
        }
    }

    pub fn resolve_module(&self, _module: &Rc<Spanned<Primary>>) -> Option<Rc<dyn AstObject>> {
        todo!()
    }

    pub fn magical_name_of(&self, st: impl AsRef<str>) -> Option<(NonZeroUsize, NonZeroUsize)> {
        let group = self.span_ref.borrow().find(st)?;
        Some((group, group))
    }

    pub fn load_module<T>(
        &mut self,
        path: impl AsRef<Path>,
        f: impl Fn(&mut Self, ModuleRef) -> T,
    ) -> T {
        let path = path.as_ref();

        let module = if let Some(refm) = self.modules.get(&ModuleRef(path.into())) {
            refm.key().clone()
        } else {
            log::debug!("Loading module ({:?})", shorten(path));

            let source = match std::fs::read_to_string(&path) {
                Ok(st) => st.into_boxed_str(),
                Err(why) => {
                    log::error!("Failed to read module contents! why={:?}", why);
                    unreachable!();
                }
            };

            self.parse_and_register_module(source, path)
        };

        f(self, module)
    }

    fn resolve_import_to_path(
        &self,
        qualname: Vec<&str>,
        paths: impl Iterator<Item = PathBuf>,
    ) -> Option<PathBuf> {
        let paths_to_inspect: &[PathBuf] = &[PathBuf::from("."), self.libstd.clone()] as &[_];

        'outer: for path in paths_to_inspect.iter().cloned().chain(paths) {
            let mut root = path.to_owned();

            let mut path = ImportPath::new(root.clone());

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

    pub fn import_module(
        &self,
        decl: ImportDecl,
        mctx: &ModuleContext,
    ) -> Result<Vec<(ModuleRef, (SpanRef, SpanRef))>, Vec<String>> {
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

        let module_dir = mctx.path.parent().unwrap().to_path_buf();

        for qualname in &qualnames {
            let name = qualname.last().unwrap().as_name().unwrap();
            let qualname: Vec<&str> = qualname
                .into_iter()
                .map(|atom| match atom {
                    Atom::Name((n, _)) => {
                        mctx.source.get(self.span_ref.borrow().get(*n).unwrap()).unwrap()
                    }
                    _ => unreachable!(),
                })
                .collect();

            // the magical `__monty` module name is special.
            if matches!(qualname.as_slice(), ["__monty", ..]) {
                modules.push((ModuleRef("__monty".into()), name));
                continue;
            }

            let path = {
                let path = match self.resolve_import_to_path(qualname.clone(), std::iter::once(module_dir.clone())) {
                    Some(p) => p,
                    None => return Err(qualname.iter().map(ToString::to_string).collect()),
                };

                let module_ref = ModuleRef(path);

                if self.modules.contains_key(&module_ref) {
                    modules.push((module_ref, name));
                    continue;
                } else {
                    module_ref.0
                }
            };

            log::trace!("import: Import module ({:?})", shorten(&path));

            let source = match std::fs::read_to_string(&path) {
                Ok(st) => st.into_boxed_str(),
                Err(why) => {
                    log::error!("Failed to read module contents! why={:?}", why);
                    unreachable!();
                }
            };

            let module_ref = self.parse_and_register_module(source, path);

            modules.push((module_ref, name));
        }

        Ok(modules)
    }

    fn parse<T>(&self, s: Rc<str>, mref: ModuleRef) -> T
    where
        T: Parseable + Clone,
    {
        let Spanned { inner, .. }: Spanned<T> = (s, self.span_ref.clone(), mref).into();
        inner
    }

    fn parse_and_register_module<P>(&self, input: Box<str>, path: P) -> ModuleRef
    where
        P: Into<PathBuf>,
    {
        let source: Rc<_> = input.into();
        let path = path.into();

        let module: Module = self.parse(Rc::clone(&source), ModuleRef(path.clone()));

        self.register_module(module, path, source)
    }

    fn register_module(&self, module: Module, path: PathBuf, source: Rc<str>) -> ModuleRef {
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
                globals: DashMap::new(),
            },
        ) {
            panic!(
                "Overwrote previously registered module {:?} -> {:?}",
                key, previous
            );
        }

        key
    }

    /// Include a module specified at `path` to the global compilation context.
    ///
    /// This will invoke typechecking routines on the modules items.
    pub fn include_module(&mut self, path: &Path) -> ModuleRef {
        self.load_module(path, move |ctx, mref| {
            let module = crate::interpreter::exec_module(ctx, mref.clone());

            let mut mctx = ctx.modules.get_mut(&mref).unwrap().value().clone();

            let mut scope = OpaqueScope::from(module.clone() as Rc<_>);
            scope.module_ref = Some(mref.clone());

            let _ = std::mem::replace(&mut mctx.scope, Rc::new(scope) as Rc<_>);
            let _ = std::mem::replace(&mut mctx.module, module);

            ctx.database.insert_module(&mctx);

            for ScopedObject { object, .. } in mctx.scope.iter() {
                let mut lctx = mctx.unbound_local_context(ctx);

                lctx.this = Some(Rc::clone(&object));

                object.typecheck(&lctx).unwrap_or_compiler_error(&lctx);
            }

            mref
        })
    }

    pub fn as_codegen_module(&mut self) -> CodegenModule {
        let mut cctx = CodegenModule::new(self, self.opts.codegen_settings());

        for (mref, mctx) in self
            .modules
            .iter()
            .map(|refm| (refm.key().clone(), refm.value().clone()))
        {
            for (name, global) in mctx
                .globals
                .iter()
                .map(|refm| (refm.key().clone(), refm.value().clone()))
            {
                let symbol = format!(
                    "{}.{}",
                    mref.module_name(),
                    self.resolver.resolve_ident(name).unwrap()
                );

                cctx.declare_global_in_module(
                    (mref.clone(), name),
                    &symbol,
                    true,
                    Linkage::Hidden,
                    |cctx| {
                        let atom = if let Expr::Primary(Spanned {
                            inner: Primary::Atomic(atom),
                            ..
                        }) = &global.inner
                        {
                            atom
                        } else {
                            unimplemented!();
                        };

                        let mut dctx = DataContext::new();

                        let type_id = match &atom.inner {
                            Atom::None => TypeMap::NONE_TYPE,
                            Atom::Ellipsis => TypeMap::ELLIPSIS,
                            Atom::Int(i) => {
                                dctx.set_align(8);
                                dctx.define(Box::new(i.to_ne_bytes()));

                                TypeMap::INTEGER
                            }

                            Atom::Str(st) => {
                                let string = StringRef(*st)
                                    .resolve_as_string(&cctx.global_context)
                                    .unwrap();
                                let string = std::ffi::CString::new(string).unwrap();
                                let string = string.into_bytes_with_nul();
                                let string = string.into_boxed_slice();

                                dctx.set_align(16);
                                dctx.define(string);

                                TypeMap::STRING
                            }

                            Atom::Bool(b) => {
                                dctx.set_align(1);
                                dctx.define(Box::new(if *b { [1u8] } else { [0u8] }));

                                TypeMap::BOOL
                            }

                            Atom::Float(f) => {
                                dctx.set_align(8);
                                dctx.define(Box::new(f.to_ne_bytes()));

                                TypeMap::FLOAT
                            }

                            _ => unimplemented!(),
                        };

                        (type_id, dctx)
                    },
                );
            }
        }

        let funcs = self.functions.borrow();
        let funcs = funcs
            .iter()
            .enumerate()
            .filter(|(_, func)| !matches!(func.linkage, Some(Linkage::Import)))
            .map(|(idx, func)| {
                let f = func.as_ref();
                let m = func.scope.module_ref();
                let l = func.linkage.unwrap();
                let cc = func.callcov.unwrap_or(CallConv::SystemV);

                (idx, f, m, l, cc)
            });

        cctx.declare_functions(funcs);

        cctx
    }
}
