use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    ast::{module::Module, AstObject, Spanned},
    func::Function,
    parser::{Parseable, SpanRef},
    scope::{LocalScope, OpaqueScope, Scope},
    typing::TypeMap,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
pub struct ModuleRef(PathBuf);

/// Used to track global compilation state per-compilation.
#[derive(Debug)]
pub struct GlobalContext {
    pub modules: HashMap<ModuleRef, ModuleContext>,
    pub functions: Vec<Function>,
    pub span_ref: Rc<RefCell<SpanRef>>,
    pub type_map: RefCell<TypeMap>,
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            modules: HashMap::new(),
            functions: Vec::new(),
            span_ref: Default::default(),
            type_map: RefCell::new(TypeMap::new()),
        }
    }
}

impl GlobalContext {
    pub fn parse<T, S>(&self, s: S) -> T
    where
        S: AsRef<str>,
        T: Parseable + Clone,
    {
        let Spanned { inner, .. }: Spanned<T> = (s, self.span_ref.clone()).into();
        inner
    }

    pub fn parse_and_register_module<S>(&mut self, source: S, path: PathBuf) -> ModuleRef
    where
        S: AsRef<str>,
    {
        let module: Module = self.parse(source);
        self.register_module(module, path)
    }

    pub fn walk(&self, module_ref: ModuleRef) -> impl Iterator<Item = (Rc<dyn AstObject>, LocalContext)> {
        let module_context = self.modules.get(&module_ref).unwrap();
        let mut it = module_context.scope.iter();


        std::iter::from_fn(move || {
            let scoped = it.next()?;

            let object = scoped.object.unspanned();
            let ctx = scoped.make_local_context(module_ref.clone(), self);

            Some((object, ctx))
        })
    }

    pub fn register_module(&mut self, module: Module, path: PathBuf) -> ModuleRef {
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
