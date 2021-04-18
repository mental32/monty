use std::rc::Rc;

use crate::{ast::{class::ClassDef, funcdef::FunctionDef}, prelude::*};

use super::{Renamed, ScopeIter, ScopeRoot, ScopedObject, collect_subnodes};

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
    ) -> Vec<Rc<dyn AstObject>> {
        assert!(target.is_some());

        log::trace!(
            "lookup: Performing generic lookup on target=({:?} -> {:?})",
            target,
            {
                let mctx = global_context
                    .modules
                    .get(self.module_ref.as_ref().expect("no module ref"))
                    .unwrap();
                global_context
                    .span_ref
                    .borrow()
                    .resolve_ref(target, mctx.source.as_ref())
            }
        );

        let mut results = vec![];

        // inspect immediate scope

        let extra = match &self.root {
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
            if name == target {
                results.push(object.clone() as Rc<dyn AstObject>);
            }
        }

        for object in self.nodes.iter().map(|o| o.unspanned()) {
            let item = object.as_ref();

            log::trace!("lookup: immediate -> {:?}", item);

            if item.is_named(target) {
                results.push(object.clone());
            }
        }

        // inspect parent scope or the builtins

        if let Some(parent_scope) = self.parent.as_ref() {
            results.extend(parent_scope.lookup_any(target, global_context))
        } else {
            log::trace!("lookup: checking builtins for matches");

            let local_mref = self.module_ref.as_ref().unwrap();

            let mctx = global_context.modules.get(local_mref).unwrap();
            let st = global_context
                .span_ref
                .borrow()
                .resolve_ref(target, mctx.source.as_ref())
                .unwrap();

            for (type_id, (object, mref)) in global_context.builtins.iter() {
                let item = object.as_ref();
                let object = Some(object.scope.root())
                    .and_then(|root| match root {
                        ScopeRoot::AstObject(obj) => Some(obj),
                        _ => None,
                    })
                    .and_then(|obj| if obj.as_ref().downcast_ref::<ClassDef>().is_some() { Some(obj) } else { None })
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
                            mref: self.module_ref.clone().unwrap(),
                        };
                        let renamed = Rc::new(renamed) as Rc<dyn AstObject>;

                        results.push(renamed);
                    }
                } else if local_mref == mref && object.is_named(target) {
                    results.push(object.clone())
                }
            }
        }

        results
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
                parent: None,
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
