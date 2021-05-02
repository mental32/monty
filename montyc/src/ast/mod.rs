use std::fmt;
use std::{
    any::{Any, TypeId},
    rc::Rc,
};

use logos::Span;
use typing::TypedObject;

use crate::{
    context::LocalContext,
    parser::token::PyToken,
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
pub struct Spanned<T>
where
    T: AstObject + Clone,
{
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T>
where
    T: AstObject + Clone,
{
    pub fn reveal<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.span.clone())
    }
}

impl<T> Spanned<T>
where
    T: AstObject + Clone,
{
    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
        U: AstObject + Clone,
    {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }

    pub fn transparent<U>(&self, u: U) -> Spanned<U>
    where
        U: AstObject + Clone,
    {
        Spanned {
            span: self.span.clone(),
            inner: u,
        }
    }

    pub fn transparent_with<U, F>(&self, f: F) -> Spanned<U>
    where
        F: FnOnce(Spanned<T>) -> U,
        U: AstObject + Clone,
    {
        Spanned {
            span: self.span.clone(),
            inner: f(self.clone()),
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

// PyToken trait impls

impl AstObject for PyToken {
    fn span(&self) -> Option<Span> {
        unreachable!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<ObjectIter> {
        None
    }
}

impl LookupTarget for PyToken {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        match self {
            Self::Ident(n) => n.clone() == target,
            _ => false,
        }
    }

    fn name(&self) -> crate::parser::SpanEntry {
        match self {
            Self::Ident(n) => n.clone(),
            _ => None,
        }
    }
}

impl TypedObject for PyToken {
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        unreachable!()
    }

    fn typecheck<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<()> {
        unreachable!()
    }
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
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        self.inner.is_named(target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
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

fn with_object_in_database_and_then<'a, T, U>(object: &Rc<T>, ctx: &LocalContext<'a>, f: impl Fn(&dyn AstObject) -> U) -> U
where
    T: AstObject,
{
    if ctx
        .global_context
        .database
        .contains(Rc::as_ptr(object) as *const ())
    {
        // `object` is a `DefEntry` already.
        return f(object.as_ref());
    }

    // lazily insert `object` into the database if it has a span (is well formed.)
    if let Some(span) = object.span() {
        let this = ctx.this.as_ref().expect("`ctx.this` must be set!");
        let this_span = this.span().expect("`ctx.this` must be spanned!");

        // NOTE: so like we're just assuming `this` and `object` are from the same module
        //       since the caller is supposed to set `ctx.this = self` before interacting with us.

        assert_eq!(span, this_span, "{:?} != {:?}", object, this);
        assert_eq!(
            Rc::as_ptr(object) as *const (),
            Rc::as_ptr(&(Rc::clone(object) as Rc<dyn AstObject>)) as *const ()
        );

        let object = ctx
            .global_context
            .database
            .insert(Rc::clone(object) as Rc<dyn AstObject>, &ctx.module_ref);

        return f(object.as_ref());
    }

    f(object.as_ref())
}

impl<T> TypedObject for Rc<T>
where
    T: AstObject,
{
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        with_object_in_database_and_then(self, ctx, |object| object.infer_type(ctx))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        with_object_in_database_and_then(self, ctx, |object| object.typecheck(ctx))
    }
}

impl<T: 'static> LookupTarget for Rc<T>
where
    T: AstObject + fmt::Debug,
{
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        self.as_ref().is_named(target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
        self.as_ref().name()
    }
}

// Option<T> trait impls

impl<T: 'static> AstObject for Option<T>
where
    T: AstObject + fmt::Debug,
{
    fn walk(&self) -> Option<ObjectIter> {
        self.as_ref()?.walk()
    }

    fn span(&self) -> Option<Span> {
        self.as_ref()?.span()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        self.as_ref().unwrap().unspanned()
    }
}

impl<T> TypedObject for Option<T>
where
    T: TypedObject + fmt::Debug,
{
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        self.as_ref().unwrap().infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        self.as_ref().unwrap().typecheck(ctx)
    }
}

impl<T: 'static> LookupTarget for Option<T>
where
    T: AstObject + fmt::Debug,
{
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        self.as_ref().unwrap().is_named(target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
        self.as_ref().unwrap().name()
    }
}
