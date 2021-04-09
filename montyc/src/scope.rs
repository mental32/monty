use std::{
    any::{Any, TypeId},
    cell::RefCell,
    fmt,
    marker::PhantomData,
    rc::Rc,
};

use fmt::Debug;

use crate::{ast::{AstObject, assign::Assign, atom::Atom, funcdef::FunctionDef, stmt::Statement}, context::{GlobalContext, LocalContext, ModuleRef}, func::Function, parser::SpanEntry, typing::{LocalTypeId, TypeMap}};

fn collect_subnodes(object: &dyn AstObject) -> Vec<Rc<dyn AstObject>> {
    let stream = match object.walk() {
        Some(it) => it,
        None => return vec![],
    };

    let mut nodes = vec![];

    stream.for_each(|object| {
        nodes.push(object.clone());

        if let Some(Statement::FnDef(_)) = downcast_ref::<Statement>(object.unspanned().as_ref()) {
            return;
        }

        for subnode in collect_subnodes(object.as_ref()) {
            nodes.push(subnode)
        }
    });

    nodes
}

pub fn downcast_ref<T: Any>(o: &dyn AstObject) -> Option<&T> {
    if o.type_id() == TypeId::of::<T>() {
        // SAFETY: This is the exact same logic present in
        //         `std::any::Any::downcast_ref` minus the
        //         'static lifetime bound on the trait.
        //
        //         If this is unsound then that one probably is too.
        unsafe { Some(&*(o as *const _ as *const T)) }
    } else {
        None
    }
}

// -- enum ScopeRoot

#[derive(Debug, Clone)]
pub enum ScopeRoot {
    AstObject(Rc<dyn AstObject>),
    Func(Rc<Function>),
}

// -- trait Scope

pub trait Scope: core::fmt::Debug {
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject<'b>> + 'b)>;

    fn root(&self) -> ScopeRoot;

    fn parent(&self) -> Option<&dyn Scope> {
        None
    }

    fn lookup(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>>;

    fn lookup_any(&self, target: SpanEntry) -> Vec<Rc<dyn AstObject>>;
}

// -- OpaqueScope

#[derive(Debug, Clone)]
pub struct OpaqueScope {
    pub root: ScopeRoot,
    pub nodes: Vec<Rc<dyn AstObject>>,
}

impl<T> From<LocalScope<T>> for OpaqueScope {
    fn from(scope: LocalScope<T>) -> Self {
        let LocalScope { _root, root, nodes } = scope;

        Self { root, nodes }
    }
}


impl From<Rc<dyn AstObject>> for OpaqueScope {
    fn from(root: Rc<dyn AstObject>) -> Self {
        let nodes = collect_subnodes(root.as_ref());

        Self {
            root: ScopeRoot::AstObject(root),
            nodes,
        }
    }
}

impl Scope for OpaqueScope
{
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject<'b>> + 'b)> {
        let it = self.nodes.iter().map(move |object| ScopedObject {
            scope: self,
            object: object.clone(),
        });

        Box::new(it)
    }

    fn root<'b>(&'b self) -> ScopeRoot {
        self.root.clone()
    }

    fn lookup(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        self.nodes
            .iter()
            .filter(|node| key(node.as_ref()))
            .next()
            .cloned()
    }

    fn lookup_any(&self, target: SpanEntry) -> Vec<Rc<dyn AstObject>> {
        let mut results = vec![];

        for object in self.nodes.iter().map(|o| o.unspanned()) {
            let item = object.as_ref();

            if let Some(asn) = downcast_ref::<Assign>(item) {
                if matches!(asn.name.inner, Atom::Name(n) if n == target) {
                    results.push(object.clone());
                }
            } else {
                unimplemented!("unhandled lookup case: {:?}", object);
            }
        }

        results
    }
}

// -- LocalScope

#[derive(Debug, Clone)]
pub struct LocalScope<T> {
    pub _root: PhantomData<Rc<T>>,
    pub root: ScopeRoot,
    pub nodes: Vec<Rc<dyn AstObject>>,
}

impl<T> From<T> for LocalScope<T>
where
    T: AstObject,
{
    fn from(root: T) -> Self {
        let root = Rc::new(root);
        let nodes = collect_subnodes(root.as_ref());

        Self {
            root: ScopeRoot::AstObject(root as Rc<dyn AstObject>),
            nodes,
            _root: PhantomData,
        }
    }
}

impl<T: Debug> Scope for LocalScope<T>
{
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject<'b>> + 'b)> {
        let it = self.nodes.iter().map(move |object| ScopedObject {
            scope: self,
            object: object.clone(),
        });

        Box::new(it)
    }

    fn root<'b>(&'b self) -> ScopeRoot {
        self.root.clone()
    }

    fn lookup(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        self.nodes
            .iter()
            .filter(|node| key(node.as_ref()))
            .next()
            .cloned()
    }

    fn lookup_any(&self, target: SpanEntry) -> Vec<Rc<dyn AstObject>> {
        let mut results = vec![];

        for object in self.nodes.iter().map(|o| o.unspanned()) {
            let item = object.as_ref();

            if let Some(asn) = downcast_ref::<Assign>(item) {
                if matches!(asn.name.inner, Atom::Name(n) if n == target) {
                    results.push(object.clone());
                }
            } else {
                unimplemented!("unhandled lookup case: {:?}", object);
            }
        }

        results
    }
}

// -- ScopeObject

pub struct ScopedObject<'a> {
    scope: &'a dyn Scope,
    pub object: Rc<dyn AstObject>,
}

impl<'a> ScopedObject<'a> {
    pub fn make_local_context(&self, module_context: ModuleRef, global_context: &'a GlobalContext) -> LocalContext<'a> {
        LocalContext {
            global_context,
            module_context,
            scope: self.scope,
        }
    }

    pub fn infer_type(&self, ctx: LocalContext<'a>) -> Option<LocalTypeId> {
        self.object.infer_type(&ctx)
    }

    pub fn typecheck(&self, ctx: LocalContext<'a>) {
        self.object.typecheck(ctx)
    }
}

impl<'a> fmt::Debug for ScopedObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ScopedObject")
            .field("object", &self.object)
            .field("scope", &self.scope.root())
            .finish()
    }
}
