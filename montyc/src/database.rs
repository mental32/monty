#![allow(warnings)]

use std::{
    cell::RefCell,
    num::NonZeroU32,
    rc::{Rc, Weak},
    sync::atomic::AtomicUsize,
};

use dashmap::DashMap;

use crate::{
    ast::AstObject,
    context::{GlobalContext, LocalContext, ModuleContext, ModuleRef},
    prelude::{Span, SpanRef},
    scope::LookupTarget,
    typing::{LocalTypeId, TypeMap, TypedObject},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct DefId(usize);

#[derive(Debug)]
pub struct DefEntry {
    object: Weak<dyn AstObject>,
    span: Span,
    infered_type: RefCell<Option<crate::Result<LocalTypeId>>>,
    typechecked: RefCell<Option<crate::Result<()>>>,
    module: ModuleRef,
}

impl DefEntry {
    #[inline]
    fn update<'a, T>(
        &self,
        ctx: &LocalContext<'a>,
        field: &RefCell<Option<T>>,
        f: impl Fn(LocalContext, Rc<dyn AstObject>) -> T,
    ) -> T
    where
        T: Clone + std::fmt::Debug,
    {
        if let Some(result) = field.borrow().clone() {
            log::trace!("database:update DefEntry cache hit! {:?}", result);
            return result;
        }

        let this = if let Some(object) = self.object.upgrade() {
            object
        } else {
            panic!(
                "Failed to upgrade weak-ref on DefEntry @ <{:?}: {:?}>",
                self.module, self.span
            )
        };

        let result = {
            let mut ctx = ctx.clone();

            // ctx.this = Some(Rc::clone(&this));

            f(ctx, this)
        };

        field.borrow_mut().replace(result.clone());

        result
    }
}

impl TypedObject for DefEntry {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        log::trace!("infer_type:defentry {:?}", self);
        self.update(ctx, &self.infered_type, |ctx, this| this.infer_type(&ctx))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:defentry {:?}", self);
        self.update(ctx, &self.typechecked, |ctx, this| this.typecheck(&ctx))
    }
}

impl AstObject for DefEntry {
    fn span(&self) -> Option<logos::Span> {
        self.object.upgrade()?.span()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        self.object.upgrade().unwrap()
    }

    fn walk(&self) -> Option<crate::ast::ObjectIter> {
        self.object.upgrade()?.walk()
    }
}

impl LookupTarget for DefEntry {
    fn is_named(&self, target: SpanRef) -> bool {
        self.object
            .upgrade()
            .map(|obj| obj.is_named(target))
            .unwrap_or(false)
    }

    fn name(&self) -> Option<SpanRef> {
        self.object.upgrade()?.name()
    }
}

#[derive(Debug, Default)]
pub struct ObjectDatabase {
    last_def_id: AtomicUsize,
    entries: DashMap<DefId, Rc<DefEntry>>,
    by_pointer: DashMap<*const (), DefId>,
    by_module: DashMap<ModuleRef, (DefId, Vec<DefId>)>,
    by_span: DashMap<(ModuleRef, Span), Vec<DefId>>,
}

impl ObjectDatabase {
    fn insert_directly(&self, entry: DefEntry) -> DefId {
        let key = self
            .last_def_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let key = DefId(key);

        log::trace!("database:insert_directly {:?} = {:?}", key, entry);

        assert!(
            self.by_pointer
                .insert(Weak::as_ptr(&entry.object) as *const (), key)
                .is_none(),
            "{:?}",
            Weak::as_ptr(&entry.object) as *const ()
        );
        assert!(self.entries.insert(key, Rc::new(entry)).is_none());

        key
    }

    fn find(&self, entry: &Rc<dyn AstObject>) -> Option<DefId> {
        let ptr = Rc::as_ptr(entry) as *const ();

        let entry = if let Some(refm) = self.by_pointer.get(&ptr) {
            refm.value().clone()
        } else if let Some(refm) = self
            .entries
            .iter()
            .find(|refm| Rc::as_ptr(refm.value()) as *const () == ptr)
        {
            refm.key().clone()
        } else {
            return None;
        };

        Some(entry)
    }

    fn find_by_span(&self, mref: &ModuleRef, span: Span) -> Option<DefId> {
        let entry = self.entries.iter().find(|refm| {
            let entry = refm.value();

            entry.module == *mref && entry.span == span
        })?;

        Some(entry.key().clone())
    }

    pub fn id_of(&self, object: &Rc<dyn AstObject>) -> Option<DefId> {
        self.find(object)
    }

    pub fn as_weak_object(&self, id: DefId) -> Option<Rc<dyn AstObject>> {
        self.entries.get(&id)?.value().object.upgrade()
    }

    pub fn type_of(
        &self,
        object: &Rc<dyn AstObject>,
        mref: Option<&ModuleRef>,
    ) -> Option<LocalTypeId> {
        let id = self.find(object).or_else(|| {
            log::warn!(
                "database:type_of falling back to a span-based search for {:?}",
                object
            );

            let mref = mref?;
            let span = object.span()?;

            self.find_by_span(mref, span)
        })?;

        let entry = self.entries.get(&id)?;

        let x = entry.value().infered_type.borrow().clone()?.ok();
        x
    }

    pub fn set_type_of(&self, id: DefId, ty: LocalTypeId) -> Option<LocalTypeId> {
        let mut entry = self.entries.get_mut(&id)?;

        let previous = entry.value_mut().infered_type.borrow_mut().replace(Ok(ty));

        previous?.ok()
    }

    pub fn size_of(
        &self,
        mref: ModuleRef,
        thing: Rc<dyn AstObject>,
        gctx: &GlobalContext,
    ) -> Option<NonZeroU32> {
        let id = self
            .find(&thing)
            .or_else(|| self.find_by_span(&mref, thing.span()?))?;

        let entry = self.entries.get(&id)?;

        let kind = entry.infered_type.borrow().clone()?.ok()?;

        crate::typing::SizeOf::size_of(gctx.type_map.get(kind)?.value(), gctx)
    }

    pub fn insert_module(&mut self, mctx: &ModuleContext) -> DefId {
        let module = Rc::downgrade(&mctx.module);

        let entry = DefEntry {
            object: module,
            span: 0..mctx.source.len(),
            infered_type: RefCell::new(Some(Ok(TypeMap::MODULE))),
            typechecked: RefCell::new(None),
            module: mctx.module_ref(),
        };

        let key = self.insert_directly(entry);

        self.by_module.insert(mctx.module_ref(), (key, vec![]));

        key
    }

    pub fn contains_entry(&self, object: &Rc<dyn AstObject>) -> bool {
        self.entries
            .iter()
            .any(|refm| Rc::as_ptr(refm.value()) as *const () == Rc::as_ptr(object) as *const ())
    }

    pub fn contains_object(&self, object: &Rc<dyn AstObject>) -> bool {
        self.by_pointer
            .get(&(Rc::as_ptr(object) as *const ()))
            .is_some()
    }

    pub fn entry(&self, entry: Rc<dyn AstObject>, mref: &ModuleRef) -> Rc<dyn AstObject> {
        let span = entry.span().expect("AstDatabase entries must be spanned!");

        assert_eq!(self.entries.len(), self.by_pointer.len());

        if let Some(id) = self.by_pointer.get(&(Rc::as_ptr(&entry) as *const ())) {
            let entry = self.entries.get(id.value()).unwrap();

            return Rc::clone(entry.value()) as Rc<_>;
        }

        log::trace!("database:entry Inserting new DefEntry = {:?}", entry);

        let id = self.insert_directly(DefEntry {
            object: Rc::downgrade(&entry),
            span: span.clone(),
            infered_type: RefCell::new(None),
            typechecked: RefCell::new(None),
            module: mref.clone(),
        });

        assert!(self.contains_object(&entry));

        self.by_span
            .entry((mref.clone(), span.clone()))
            .or_default()
            .push(id);

        if let Some(mut result) = self.by_module.get_mut(mref) {
            let (_, v) = result.value_mut();

            if !v.contains(&id) {
                v.push(id);
            }
        }

        let entry = self.entries.get(&id).unwrap();

        Rc::clone(entry.value()) as Rc<_>
    }

    pub fn query(&self) -> QueryIter<'_> {
        let mut results = Vec::with_capacity(self.entries.len());

        results.extend(self.entries.iter().map(|refm| refm.key().clone()));

        QueryIter {
            database: self,
            results,
        }
    }
}

#[derive(Debug)]
pub struct LazyDefEntries<'a> {
    inner: Vec<DefId>,
    database: &'a ObjectDatabase,
}

impl<'a> Iterator for LazyDefEntries<'a> {
    type Item = Rc<dyn AstObject>;

    fn next(&mut self) -> Option<Self::Item> {
        let def_id = self.inner.pop()?;
        let def_entry = self.database.entries.get(&def_id)?.value().clone();

        Some(def_entry as Rc<_>)
    }
}

#[derive(Debug)]
pub struct QueryIter<'a> {
    database: &'a ObjectDatabase,
    results: Vec<DefId>,
}

impl<'a> QueryIter<'a> {
    pub fn module(mut self, mref: ModuleRef) -> Self {
        if let Some(refm) = self.database.by_module.get(&mref) {
            let (_, module) = refm.value();
            let _ = self.results.drain_filter(|id| module.contains(id));
        }

        self
    }

    pub fn span(mut self, span: Span) -> Self {
        let Self {
            database,
            mut results,
        } = self;

        let _ = results.drain_filter(|id| {
            database
                .entries
                .get(id)
                .map(|refm| refm.value().span == span)
                .unwrap_or(false)
        });

        Self { database, results }
    }

    pub fn filter(mut self, f: impl Fn(&dyn AstObject) -> bool) -> Self {
        let Self {
            database,
            mut results,
        } = self;

        let _ = results.drain_filter(|id| match database.entries.get(id) {
            Some(refm) => refm
                .value()
                .object
                .upgrade()
                .map(|obj| f(obj.as_ref()))
                .unwrap_or(true),
            None => true,
        });

        Self { database, results }
    }
}

impl<'a> IntoIterator for QueryIter<'a> {
    type Item = Rc<dyn AstObject>;

    type IntoIter = LazyDefEntries<'a>;

    fn into_iter(self) -> Self::IntoIter {
        let Self {
            database,
            results: inner,
        } = self;

        LazyDefEntries { inner, database }
    }
}
