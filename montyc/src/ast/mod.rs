use std::{
    any::{Any, TypeId},
    mem::MaybeUninit,
    rc::Rc,
};
use std::{fmt, sync::Arc};

use logos::{source, Span};
use typing::{TypeMap, TypedObject};

use crate::{
    context::LocalContext,
    parser::token::PyToken,
    scope::LookupTarget,
    typing::{self, LocalTypeId},
};

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

pub trait AstObject: fmt::Debug + TypedObject + LookupTarget + Any {
    fn span(&self) -> Option<Span>;

    fn unspanned(&self) -> Rc<dyn AstObject>;

    fn walk(&self) -> Option<ObjectIter>;
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        unreachable!()
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        self.inner.infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
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

impl<T> TypedObject for Rc<T>
where
    T: TypedObject + fmt::Debug,
{
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        self.as_ref().infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        self.as_ref().typecheck(ctx)
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

// Arc<T> trait impls

impl<T: 'static> AstObject for Arc<T>
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

impl<T> TypedObject for Arc<T>
where
    T: TypedObject + fmt::Debug,
{
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        self.as_ref().infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        self.as_ref().typecheck(ctx)
    }
}

impl<T: 'static> LookupTarget for Arc<T>
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        self.as_ref()?.infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
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
