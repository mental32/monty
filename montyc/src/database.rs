use std::{cell::RefCell, rc::{Rc, Weak}, sync::atomic::AtomicUsize};

use dashmap::DashMap;

use crate::{
    ast::{AstObject},
    context::{LocalContext, ModuleContext, ModuleRef},
    prelude::Span,
    scope::LookupTarget,
    typing::{LocalTypeId, TypeMap, TypedObject},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct DefId(usize);

#[derive(Debug)]
struct DefEntry {
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
        T: Clone,
    {
        if let Some(result) = field.borrow().clone() {
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

        let result = ctx.with(Rc::clone(&this), f);

        field.borrow_mut().replace(result.clone());

        result
    }
}

impl TypedObject for DefEntry {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        self.update(ctx, &self.infered_type, |ctx, this| this.infer_type(&ctx))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
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

    by_module: DashMap<ModuleRef, (DefId, Vec<DefId>)>,
    by_span: DashMap<(ModuleRef, Span), Vec<DefId>>,
}

impl AstDatabase {
    fn insert_directly(&self, entry: DefEntry) -> DefId {
        let key = self.last_def_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let key = DefId(key);

        log::trace!("database:insert_directly {:?} = {:?}", key, entry);

        assert!(self.entries.insert(key, Rc::new(entry)).is_none());

        key
    }

    pub fn enumerate(&mut self, mctx: &ModuleContext) -> DefId {
        let module = Rc::downgrade(&mctx.module);

        let entry = DefEntry {
            object: module,
            span: 0..mctx.source.len(),
            infered_type: RefCell::new(Some(Ok(TypeMap::MODULE))),
            typechecked: RefCell::new(None),
            module: mctx.module_ref(),
        };

        self.insert_directly(entry)
    }

    pub fn contains(&self, ptr: *const ()) -> bool {
        self.entries.iter().any(|refm| Rc::as_ptr(refm.value()) as *const () == ptr)
    }

    pub fn insert(&self, entry: Rc<dyn AstObject>, mref: &ModuleRef) -> Rc<dyn AstObject>
    {
        log::trace!("database:insert {:?}", entry);

        let span = entry.span().expect("AstDatabase entries must be spanned!");

        if let Some(entry) = self.by_span.get(&(mref.clone(), span.clone())) {
            let value = entry.value();

            if !value.is_empty() {
                assert_eq!(value.len(), 1, "multiple spanned entries!");

                let def_id = value.last().unwrap();

                let entry = self.entries.get(&def_id).unwrap();

                return Rc::clone(entry.value()) as Rc<_>;
            }
        }


        let id = self.insert_directly(DefEntry {
            object: Rc::downgrade(&entry),
            span: span.clone(),
            infered_type: RefCell::new(None),
            typechecked: RefCell::new(None),
            module: mref.clone(),
        });

        self.by_span.entry((mref.clone(), span.clone())).or_default().push(id);

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

#[derive(Debug)]
pub struct QueryIter<'a> {
    database: &'a AstDatabase,
    module: Option<ModuleRef>,
    span: Option<Span>,
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

    pub fn finish(self) -> impl Iterator<Item = Rc<dyn AstObject>> + 'a {
        let Self {
            database,
            module,
            span,
        } = self;

        let defs = if let Some(mref) = module {
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

        LazyDefEntries {
            inner: defs,
            database,
        }
    }
}
