#![allow(warnings)]

use std::{
    cell::RefCell,
    rc::{Rc, Weak},
    sync::atomic::AtomicUsize,
};

use dashmap::DashMap;

use crate::{
    ast::AstObject,
    context::{LocalContext, ModuleContext, ModuleRef},
    prelude::Span,
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
    fn is_named(&self, target: crate::prelude::SpanEntry) -> bool {
        self.object
            .upgrade()
            .map(|obj| obj.is_named(target))
            .unwrap_or(false)
    }

    fn name(&self) -> crate::prelude::SpanEntry {
        self.object.upgrade()?.name()
    }
}

#[derive(Debug, Default)]
pub struct AstDatabase {
    last_def_id: AtomicUsize,
    entries: DashMap<DefId, Rc<DefEntry>>,
    by_pointer: DashMap<*const (), DefId>,
    by_module: DashMap<ModuleRef, (DefId, Vec<DefId>)>,
    by_span: DashMap<(ModuleRef, Span), Vec<DefId>>,
}

impl AstDatabase {
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
        let entry  = self.entries.iter().find(|refm| Rc::as_ptr(refm.value()) as *const () == ptr)?;

        Some(entry.key().clone())
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
        type_map: &TypeMap,
    ) -> Option<usize> {
        let id = self.find_by_span(&mref, thing.span()?)?;
        let entry = self.entries.get(&id)?;

        let kind = entry.infered_type.borrow().clone()?.ok()?;

        type_map.size_of(kind)
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
        QueryIter {
            database: self,
            module: None,
            span: None,
            filters: vec![],
        }
    }
}

#[derive(Debug)]
pub struct LazyDefEntries<'a> {
    inner: Vec<DefId>,
    database: &'a AstDatabase,
}

impl<'a> Iterator for LazyDefEntries<'a> {
    type Item = Rc<dyn AstObject>;

    fn next(&mut self) -> Option<Self::Item> {
        let def_id = self.inner.pop()?;
        let def_entry = self.database.entries.get(&def_id)?.value().clone();

        Some(def_entry as Rc<_>)
    }
}

// #[derive(Debug)]
pub struct QueryIter<'a> {
    database: &'a AstDatabase,
    module: Option<ModuleRef>,
    span: Option<Span>,
    filters: Vec<Box<dyn Fn(&dyn AstObject) -> bool>>,
}

impl<'a> QueryIter<'a> {
    pub fn module(mut self, mref: ModuleRef) -> Self {
        self.module.replace(mref);
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span.replace(span);
        self
    }

    pub fn filter(mut self, f: impl Fn(&dyn AstObject) -> bool + 'static) -> Self {
        self.filters.push(Box::new(f));
        self
    }

    pub fn finish(self) -> impl Iterator<Item = Rc<dyn AstObject>> + 'a {
        let Self {
            database,
            module,
            span,
            filters,
        } = self;

        let mut defs = if let Some(mref) = module {
            if let Some(span) = span {
                let entry = database.by_span.get(&(mref, span));
                entry.as_ref().unwrap().value().clone()
            } else {
                let entry = database.by_module.get(&mref);
                let (_, defs) = entry.as_ref().unwrap().value();
                defs.clone()
            }
        } else {
            self.database
                .entries
                .iter()
                .filter(|r| {
                    span.clone()
                        .map(|span| r.value().span == span)
                        .unwrap_or(true)
                })
                .map(|r| r.key().clone())
                .collect::<Vec<_>>()
        };

        let _ = defs.drain_filter(|def_id| match database.entries.get(def_id) {
            None => true,
            Some(def_entry) => match def_entry.value().object.upgrade() {
                Some(object) => !filters.iter().all(move |f| (f)(object.as_ref())),
                None => false,
            },
        });

        LazyDefEntries {
            inner: defs,
            database,
        }
    }
}
