use std::fmt;
use std::{
    any::{Any, TypeId},
    rc::Rc,
};

use logos::Span;
use typing::TypedObject;

use crate::{
    context::LocalContext,
    database::DefEntry,
    parser::SpanRef,
    scope::LookupTarget,
    typing::{self, LocalTypeId},
};

use self::{funcdef::FunctionDef, stmt::Statement};

pub mod assign;
pub mod atom;
pub mod class;
pub mod expr;
pub mod funcdef;
pub mod ifelif;
pub mod import;
pub mod module;
pub mod primary;
pub mod retrn;
pub mod stmt;
pub mod while_;

pub type Iter<U> = Box<dyn Iterator<Item = U>>;

pub type ObjectIter = Iter<Rc<dyn AstObject>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn reveal<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.span.clone())
    }
}

impl<T> Spanned<T> {
    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }

    pub fn transparent<U>(self, u: U) -> Spanned<U> {
        Spanned {
            span: self.span,
            inner: u,
        }
    }

    pub fn transparent_with<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(Spanned<T>) -> U,
    {
        Spanned {
            span: self.span.clone(),
            inner: f(Self {
                inner: self.inner,
                span: self.span,
            }),
        }
    }
}

// -- trait AstObject

pub trait AstObject: fmt::Debug + TypedObject + LookupTarget + Any {
    fn span(&self) -> Option<Span>;

    fn unspanned(&self) -> Rc<dyn AstObject>;

    fn walk(&self) -> Option<ObjectIter>;
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

    pub fn as_function(&self) -> Option<&FunctionDef> {
        crate::isinstance!(self, FunctionDef)
            .or_else(|| crate::isinstance!(self, Statement, Statement::FnDef(f) => f))
    }
}

#[doc(hidden)]
#[inline(always)]
pub(in crate) fn _downcast_ref<T: Any>(o: &dyn AstObject) -> Option<&T> {
    o.downcast_ref::<T>()
}

// Spanned<T> trait impls

impl<T: 'static> AstObject for Spanned<T>
where
    T: AstObject + fmt::Debug + Clone,
{
    fn walk(&self) -> Option<ObjectIter> {
        self.inner.walk()
    }

    fn span(&self) -> Option<Span> {
        Some(self.span.clone())
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.inner.clone())
    }
}

impl<T> TypedObject for Spanned<T>
where
    T: TypedObject + fmt::Debug + AstObject + Clone,
{
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        self.inner.infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        self.inner.typecheck(ctx)
    }
}

impl<T: 'static> LookupTarget for Spanned<T>
where
    T: AstObject + fmt::Debug + Clone,
{
    fn is_named(&self, target: SpanRef) -> bool {
        self.inner.is_named(target)
    }

    fn name(&self) -> Option<SpanRef> {
        self.inner.name()
    }
}

// Rc<T> trait impls

impl<T: 'static> AstObject for Rc<T>
where
    T: AstObject + fmt::Debug,
{
    fn walk(&self) -> Option<ObjectIter> {
        self.as_ref().walk()
    }

    fn span(&self) -> Option<Span> {
        self.as_ref().span()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        self.as_ref().unspanned()
    }
}

fn with_object_in_database_and_then<'a, T, U>(
    object: &Rc<T>,
    ctx: &LocalContext<'a>,
    f: impl Fn(&dyn AstObject) -> U,
) -> U
where
    T: AstObject,
{
    log::trace!("ast:with_object_in_database_and_then {:?}", object);

    let dyn_object = Rc::clone(object) as Rc<dyn AstObject>;

    debug_assert_eq!(
        Rc::as_ptr(object) as *const (),
        Rc::as_ptr(&dyn_object) as *const (),
        "sanity check: the pointer to `object` and `object` as a trait object are different?!"
    );

    // lazily insert `object` into the database if it has a span (is well formed.)
    if let Some(_) = object.span() {
        let this = ctx.this.as_ref().expect("`ctx.this` must be set!");

        // NOTE: so like we're just assuming `this` and `object` are from the same module
        //       since the caller is supposed to set `ctx.this = self` before interacting with us.

        debug_assert_eq!(
            format!("{:?}", this),
            format!("{:?}", object),
            "ctx.this and the object produce different debug formats! (are they the same object?)"
        );

        let object_entry = ctx
            .global_context
            .database
            .entry(Rc::clone(&dyn_object), &ctx.module_ref);

        assert!(
            ctx.global_context.database.contains_object(&dyn_object),
            "{:?}",
            Rc::as_ptr(&dyn_object)
        );

        return f(object_entry.as_ref());
    }

    log::warn!(
        "Analyzing an unspanned but Rc'd object is discouraged {:?}",
        object.as_ref()
    );

    f(object.as_ref())
}

impl<T> TypedObject for Rc<T>
where
    T: AstObject,
{
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        if TypeId::of::<T>() == TypeId::of::<DefEntry>() {
            self.as_ref().infer_type(ctx)
        } else {
            with_object_in_database_and_then(self, ctx, |object| object.infer_type(ctx))
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        if TypeId::of::<T>() == TypeId::of::<DefEntry>() {
            self.as_ref().typecheck(ctx)
        } else {
            with_object_in_database_and_then(self, ctx, |object| object.typecheck(ctx))
        }
    }
}

impl<T: 'static> LookupTarget for Rc<T>
where
    T: AstObject + fmt::Debug,
{
    fn is_named(&self, target: SpanRef) -> bool {
        self.as_ref().is_named(target)
    }

    fn name(&self) -> Option<SpanRef> {
        self.as_ref().name()
    }
}
