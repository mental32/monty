use std::{
    any::{Any, TypeId},
    cell::RefCell,
    fmt,
    marker::PhantomData,
    rc::Rc,
};

use fmt::Debug;

use crate::{
    ast::{
        assign::Assign, atom::Atom, class::ClassDef, funcdef::FunctionDef, import::Import,
        primary::Primary, stmt::Statement, AstObject,
    },
    context::{GlobalContext, LocalContext, ModuleRef},
    func::Function,
    parser::SpanEntry,
    typing::{LocalTypeId, TypeMap, TypedObject},
};

mod local;
mod object;
mod opaque;
mod renamed;
mod wrapped;

pub use self::{local::*, object::*, opaque::*, renamed::*, wrapped::*};

pub trait LookupTarget {
    fn is_named(&self, target: SpanEntry) -> bool;
    fn name(&self) -> SpanEntry;
    fn renamed_properties(&self) -> Option<ModuleRef> {
        None
    }
}

fn collect_subnodes(object: &dyn AstObject) -> Vec<Rc<dyn AstObject>> {
    let stream = match object.walk() {
        Some(it) => it,
        None => return vec![],
    };

    let mut nodes = vec![];

    stream.for_each(|object| {
        nodes.push(object.clone());

        if let Some(Statement::FnDef(_)) = object.unspanned().as_ref().downcast_ref() {
            return;
        } else if let Some(Statement::Import(_)) = object.unspanned().as_ref().downcast_ref() {
            return;
        }

        for subnode in collect_subnodes(object.as_ref()) {
            nodes.push(subnode)
        }
    });

    nodes
}

impl dyn AstObject {
    pub fn downcast_ref<'a, T: Any>(&'a self) -> Option<&'a T> {
        if self.type_id() == TypeId::of::<T>() {
            // SAFETY: This is the exact same logic present in
            //         `std::any::Any::downcast_ref` minus the
            //         'static lifetime bound on the trait.
            //
            //         If this is unsound then that one probably is too.
            unsafe { Some(&*(self as *const _ as *const T)) }
        } else {
            None
        }
    }
}

#[doc(hidden)]
#[inline(always)]
pub(in crate) fn _downcast_ref<T: Any>(o: &dyn AstObject) -> Option<&T> {
    o.downcast_ref::<T>()
}

// -- enum ScopeRoot

#[derive(Debug, Clone)]
pub enum ScopeRoot {
    AstObject(Rc<dyn AstObject>),
    Func(Rc<Function>),
    Class(Rc<crate::class::Class>),
}

// -- trait Scope

pub type ScopeIter<'object, 'iter, 'ctx> =
    Box<dyn Iterator<Item = (Rc<dyn AstObject + 'object>, LocalContext<'ctx>)> + 'iter>;

pub trait Scope: core::fmt::Debug {
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject> + 'b)>;

    fn root(&self) -> ScopeRoot;

    fn parent(&self) -> Option<Rc<dyn Scope>>;

    fn module_ref(&self) -> ModuleRef;

    fn walk<'a, 'b>(&'b self, global_context: &'a GlobalContext) -> ScopeIter<'a, 'b, 'b>;

    fn lookup_with(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>>;

    fn lookup_any(
        &self,
        target: SpanEntry,
        global_context: &GlobalContext,
    ) -> Vec<Rc<dyn AstObject>>;

    fn lookup_def(
        &self,
        target: SpanEntry,
        global_context: &GlobalContext,
    ) -> Vec<Rc<dyn AstObject>> {
        let mut results = self.lookup_any(target, global_context);

        let _ = results.drain_filter(|o| {
            crate::isinstance!(o.as_ref(), Assign).is_some()
                || crate::isinstance!(o.as_ref(), FunctionDef).is_some()
                || o.as_ref().downcast_ref::<Function>().is_some()
                || crate::isinstance!(o.as_ref(), ClassDef).is_some()
        });

        results
    }
}
