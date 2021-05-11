use std::rc::Rc;

use crate::ast::{
    class::ClassDef,
    funcdef::{FunctionDef, TypedFuncArg},
    import::Import,
    stmt::Statement,
};
use crate::prelude::*;

use super::LookupOrder;

// -- LookupIter

pub struct LookupIter<'a>(pub &'a OpaqueScope);

impl<'a> LookupIter<'a> {
    #[allow(warnings)]
    pub fn search_ordered(
        self,
        target: SpanRef,
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
            ScopeRoot::Func(func) => func
                .as_ref()
                .def(global_context)
                .unwrap()
                .unspanned()
                .as_ref()
                .as_function()
                .unwrap()
                .lower(),
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

        let mut results = layout
            .rev_iter(*end)
            .filter_map(|(_, block)| block.nodes.last())
            .filter(|node| {
                node.name()
                    .map(|n| global_context.span_ref.borrow().crosspan_eq(n, target))
                    .unwrap_or(false)
            })
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
        } else {
            results.extend(Self(self.0).search_unordered(target, global_context)?);
        }

        Ok(results)
    }

    pub fn search_unordered(
        self,
        target: SpanRef,
        global_context: &GlobalContext,
    ) -> crate::Result<Vec<Rc<dyn AstObject>>> {
        log::trace!(
            "lookup:search_unordered Performing generic lookup on target={:?}",
            target,
        );

        let mut results = vec![];

        // inspect immediate scope

        let extra = match &*self.0.root.borrow() {
            ScopeRoot::AstObject(o) => {
                if let Some(f) = o.as_ref().downcast_ref::<FunctionDef>() {
                    f.args.clone().unwrap_or_default()
                } else {
                    vec![]
                }
            }

            ScopeRoot::Func(f) => f
                .def(global_context)
                .unwrap()
                .unspanned()
                .as_ref()
                .as_function()
                .unwrap()
                .args
                .clone()
                .unwrap_or_default(),
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

        for (object, original) in self.0.nodes.iter().map(|o| (o.unspanned(), Rc::clone(&o))) {
            let item = object.as_ref();

            if let Some(import) =
                crate::isinstance!(object.as_ref(), Statement, Statement::Import(i) => i)
            {
                let (module, item) = match import {
                    Import::Names(names) => (
                        None,
                        names.iter().find(|name| {
                            global_context
                                .span_ref
                                .borrow()
                                .crosspan_eq(name.name().unwrap(), target)
                        }),
                    ),
                    Import::From { module, names, .. } => (
                        Some(module),
                        names.iter().find(|name| {
                            global_context
                                .span_ref
                                .borrow()
                                .crosspan_eq(name.name().unwrap(), target)
                        }),
                    ),
                };

                if let Some(item) = item {
                    let object = if let Some(module) = module {
                        global_context.access_from_module(
                            module,
                            item,
                            source.as_ref().unwrap(),
                            &self.0.module_ref(),
                        )
                    } else {
                        global_context.resolve_module(item)
                    };

                    if let Some(object) = object {
                        log::trace!("lookup:search_unordered:imported {:?}", item);

                        results.push(object.clone())
                    }
                }
            } else if item.is_named(target) {
                log::trace!("lookup:search_unordered {:?}", original);

                results.push(Rc::clone(&original));
            }
        }

        // inspect parent scope or the builtins

        if let Some(parent_scope) = self.0.parent.as_ref() {
            if let Ok(n) = parent_scope.lookup_any(target, global_context, LookupOrder::Unspecified)
            {
                results.extend(n);
            }
        } else {
            log::trace!("lookup:search_unordered checking builtins...");

            for (_, (object, _)) in global_context.builtins.iter() {
                let classdef = Some(object.scope.root())
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

                if global_context
                    .span_ref
                    .borrow()
                    .crosspan_eq(object.name, target)
                {
                    results.push(classdef as Rc<_>);
                }
            }
        }

        Ok(results)
    }
}
