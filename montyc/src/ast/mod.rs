use std::{any::{Any, TypeId}, mem::MaybeUninit, rc::Rc};
use std::{fmt, sync::Arc};

use logos::Span;
use typing::{TypeMap, TypedObject};

use crate::{context::LocalContext, parser::token::PyToken, typing::{self, LocalTypeId}};

pub mod assign;
pub mod atom;
pub mod expr;
pub mod ifelif;
pub mod primary;
pub mod module;
pub mod funcdef;
pub mod retrn;
pub mod stmt;
pub mod class;
pub mod import;

pub type Iter<U> = Box<dyn Iterator<Item = U>>;

pub type ObjectIter = Iter<Rc<dyn AstObject>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> where T: AstObject + Clone {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> where T: AstObject + Clone {
    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
        U: AstObject + Clone
    {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }
}

pub trait AstObject: fmt::Debug + TypedObject + Any {
    fn span(&self) -> Option<Span>;

    fn unspanned(&self) -> Rc<dyn AstObject>;

    fn walk(&self) -> Option<ObjectIter>;
}

impl AstObject for PyToken {
    fn span(&self) -> Option<Span> {
        unreachable!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        unreachable!()
    }

    fn walk(&self) -> Option<ObjectIter> {
        unreachable!()
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
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>
    ) -> Option<LocalTypeId> {
        self.inner.infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        self.inner.typecheck(ctx)
    }
}

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
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>
    ) -> Option<LocalTypeId> {
        self.as_ref().infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        self.as_ref().typecheck(ctx)
    }
}

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
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>
    ) -> Option<LocalTypeId> {
        self.as_ref().infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        self.as_ref().typecheck(ctx)
    }
}

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
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>
    ) -> Option<LocalTypeId> {
        self.as_ref()?.infer_type(ctx)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        self.as_ref().unwrap().typecheck(ctx)
    }
}
