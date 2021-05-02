use std::rc::Rc;

use crate::{
    ast::{
        class::ClassDef,
        funcdef::{FunctionDef, TypedFuncArg},
        import::Import,
        stmt::Statement,
    },
    prelude::*,
};

use super::{collect_subnodes, LookupOrder, Renamed, ScopeIter, ScopeRoot, ScopedObject};

// -- LookupIter

pub struct LookupIter<'a>(&'a OpaqueScope);

impl<'a> LookupIter<'a> {
    #[allow(warnings)]
    pub fn search_ordered(
        self,
        target: SpanEntry,
        global_context: &GlobalContext,
        object: Rc<dyn AstObject>,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        log::trace!(
            "lookup:search_ordered Performing ordered search on={:?}",
            target
        );

        let layout: Layout<Rc<dyn AstObject>> = match self.0.root() {
            ScopeRoot::AstObject(object) => {
                if let Some(f) = crate::isinstance!(object.as_ref(), FunctionDef) {
                    f.lower()
                } else {
                    return Self(self.0).search_unordered(target, global_context);
                }
            }
            ScopeRoot::Func(func) => func.as_ref().lower(),
            ScopeRoot::Class(klass) => todo!(),
        };

        let end = match layout.blocks.iter().find(|(id, block)| {
            block
                .nodes
                .last()
                .map(|top| {
                    Rc::ptr_eq(top, &object)
                    // sometimes pointer equality is not enough since we clone stuff around pretty forceably
                    // and may be working on two separate copies of the same data... so a span range check is performed just in case.
                        || top
                            .span()
                            .and_then(|l| {
                                let r = object.span()?;
                                Some(l == r || (l.start < r.start && l.end >= r.end))
                            })
                            .unwrap_or(false)
                })
                .unwrap_or(false)
        }) {
            Some((id, _)) => id,
            None => return self.search_unordered(target, global_context),
        };

        let results = layout
            .rev_iter(*end)
            .filter_map(|(_, block)| block.nodes.last())
            .cloned()
            .collect::<Vec<_>>();

        if results.is_empty() {
            // try to peek ahead in the layout in case its an unbound local

            if let Some(span) = layout
                .iter_from(*end)
                .filter_map(|(_, block)| block.nodes.last())
                .filter_map(|obj| {
                    obj.as_ref()
                        .downcast_ref::<Spanned<crate::ast::assign::Assign>>()
                        .and_then(|asn| {
                            if asn.is_named(target) {
                                Some(asn.span.clone())
                            } else {
                                None
                            }
                        })
                        .or_else(|| {
                            match obj
                                .as_ref()
                                .downcast_ref::<Spanned<crate::ast::stmt::Statement>>()
                            {
                                Some(Spanned {
                                    inner: crate::ast::stmt::Statement::Asn(a),
                                    span,
                                }) if a.is_named(target) => Some(span.clone()),
                                _ => None,
                            }
                        })
                })
                .next()
            {
                return Err(MontyError::UnboundLocal {
                    name: target,
                    assign: span,
                    used: object.span().unwrap(),
                });
            }

            return Self(self.0).search_unordered(target, global_context);
        }

        Ok(results)
    }

    pub fn search_unordered(
        self,
        target: SpanEntry,
        global_context: &GlobalContext,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        log::trace!(
            "lookup:search_unordered Performing generic lookup on target={:?}",
            target,
        );

        let mut results = vec![];

        // inspect immediate scope

        let extra = match &self.0.root {
            ScopeRoot::AstObject(o) => {
                if let Some(f) = o.as_ref().downcast_ref::<FunctionDef>() {
                    f.args.clone().unwrap_or_default()
                } else {
                    vec![]
                }
            }

            ScopeRoot::Func(f) => f.def.inner.args.clone().unwrap_or_default(),
            _ => vec![],
        };

        for (name, object) in extra {
            log::trace!(
                "lookup:search_undordered checking function arg: {:?} = {:?}",
                name,
                object
            );

            if name == target {
                let arg = TypedFuncArg {
                    name,
                    annotation: Rc::clone(&object),
                };

                results.push(Rc::new(arg) as Rc<_>);
            }
        }

        let source = global_context
            .resolver
            .sources
            .get(self.0.module_ref.as_ref().unwrap());

        for object in self.0.nodes.iter().map(|o| o.unspanned()) {
            let item = object.as_ref();

            if let Some(import) =
                crate::isinstance!(object.as_ref(), Statement, Statement::Import(i) => i)
            {
                let (module, item) = match import {
                    Import::Names(names) => (None, names.iter().find(|name| name.is_named(target))),
                    Import::From { module, names, .. } => (
                        Some(module),
                        names.iter().find(|name| name.is_named(target)),
                    ),
                };

                if let Some(item) = item {
                    let object = if let Some(module) = module {
                        global_context.access_from_module(module, item, source.as_ref().unwrap())
                    } else {
                        global_context.resolve_module(item)
                    };

                    if let Some(object) = object {
                        log::trace!("lookup:search_unordered {:?}", item);

                        results.push(object.clone())
                    }
                }
            } else if item.is_named(target) {
                    log::trace!("lookup:search_unordered {:?}", item);

                    results.push(object.clone());
                }
        }

        // inspect parent scope or the builtins

        if let Some(parent_scope) = self.0.parent.as_ref() {
            if let Ok(n) = parent_scope.lookup_any(target, global_context, LookupOrder::Unspecified)
            {
                results.extend(n);
            }
        } else {
            log::trace!("lookup: checking builtins for matches");

            let local_mref = self.0.module_ref.as_ref().unwrap();

            let mctx = global_context.modules.get(local_mref).unwrap();
            let st = global_context
                .span_ref
                .borrow()
                .resolve_ref(target, mctx.source.as_ref())
                .unwrap();

            for (_type_id, (object, mref)) in global_context.builtins.iter() {
                let _item = object.as_ref();
                let object = Some(object.scope.root())
                    .and_then(|root| match root {
                        ScopeRoot::AstObject(obj) => Some(obj),
                        _ => None,
                    })
                    .and_then(|obj| {
                        if obj.as_ref().downcast_ref::<ClassDef>().is_some() {
                            Some(obj)
                        } else {
                            None
                        }
                    })
                    .unwrap();

                if local_mref != mref {
                    let mctx = global_context.modules.get(mref).unwrap();
                    let lst = global_context
                        .span_ref
                        .borrow()
                        .resolve_ref(object.name(), mctx.source.as_ref())
                        .unwrap();

                    if lst == st {
                        log::trace!(
                            "lookup: renaming builtin object to={:?} from={:?}",
                            target,
                            object.name()
                        );

                        let renamed = Renamed {
                            inner: object.clone(),
                            name: target.clone(),
                            mref: self.0.module_ref.clone().unwrap(),
                        };
                        let renamed = Rc::new(renamed) as Rc<dyn AstObject>;

                        results.push(renamed);
                    }
                } else if local_mref == mref && object.is_named(target) {
                    results.push(object.clone())
                }
            }
        }

        Ok(results)
    }
}

// -- OpaqueScope

#[derive(Debug, Clone)]
pub struct OpaqueScope {
    pub root: ScopeRoot,
    pub module_ref: Option<ModuleRef>,
    pub nodes: Vec<Rc<dyn AstObject>>,
    pub parent: Option<Rc<dyn Scope>>,
}

impl<T> From<LocalScope<T>> for OpaqueScope {
    fn from(scope: LocalScope<T>) -> Self {
        let LocalScope { inner, .. } = scope;

        inner
    }
}

impl From<Rc<dyn AstObject>> for OpaqueScope {
    fn from(root: Rc<dyn AstObject>) -> Self {
        let nodes = collect_subnodes(root.as_ref());

        Self {
            root: ScopeRoot::AstObject(root),
            nodes,
            module_ref: None,
            parent: None,
        }
    }
}

impl Scope for OpaqueScope {
    fn iter<'b>(&'b self) -> Box<(dyn Iterator<Item = ScopedObject> + 'b)> {
        let it = self.nodes.iter().map(move |object| ScopedObject {
            scope: Rc::new(self.clone()),
            object: object.clone(),
        });

        Box::new(it)
    }

    fn root<'b>(&'b self) -> ScopeRoot {
        self.root.clone()
    }

    fn lookup_with(&self, key: &dyn Fn(&dyn AstObject) -> bool) -> Option<Rc<dyn AstObject>> {
        self.nodes
            .iter()
            .filter(|node| key(node.as_ref()))
            .next()
            .cloned()
    }

    fn lookup_any(
        &self,
        target: SpanEntry,
        global_context: &GlobalContext,
        order: LookupOrder,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        assert!(target.is_some());

        match order {
            LookupOrder::ControlFlowSensitive(object) => {
                LookupIter(self).search_ordered(target, global_context, object)
            }
            LookupOrder::Unspecified => LookupIter(self).search_unordered(target, global_context),
        }
    }

    fn walk<'a, 'b>(&'b self, global_context: &'a GlobalContext) -> ScopeIter<'a, 'b, 'b> {
        let nodes: Vec<_> = self.iter().collect();
        let mut nodes = nodes.into_iter();

        let it = std::iter::from_fn(move || {
            let scoped = nodes.next()?;

            let object = scoped.object.unspanned();
            let ctx = LocalContext {
                global_context,
                module_ref: scoped.scope.module_ref(),
                scope: scoped.scope,
                this: Some(object.clone()),
            };

            Some((object, ctx))
        });

        Box::new(it)
    }

    fn module_ref(&self) -> ModuleRef {
        self.module_ref.clone().unwrap()
    }

    fn parent(&self) -> Option<Rc<dyn Scope>> {
        self.parent.clone()
    }
}
