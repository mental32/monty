use std::{collections::HashMap, rc::Rc};

use crate::prelude::*;

use super::{atom::Atom, primary::Primary, stmt::Statement, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: Rc<Spanned<Atom>>,
    pub decorator_list: Vec<Rc<Spanned<Primary>>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl<'a> From<(&LocalContext<'a>, &ClassDef)> for crate::class::Class {
    fn from((ctx, def): (&LocalContext<'a>, &ClassDef)) -> Self {
        let type_id = match ctx.global_context.is_builtin(def, &ctx.module_ref) {
            Some(type_id) => type_id,
            None => {
                let mut type_map = ctx.global_context.type_map.borrow_mut();

                type_map.insert(crate::typing::TypeDescriptor::Class(crate::typing::ClassType {
                    name: def.name.inner.name(),
                    mref: ctx.module_ref.clone(),
                    resolver: ctx.global_context.resolver.clone(),
                }))
            }
        };

        let def = LocalScope::from(def.clone());

        Self {
            scope: def,
            kind: type_id,
            properties: HashMap::new(),
        }
    }
}

impl AstObject for ClassDef {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        Some(Box::new(std::iter::empty()))
    }
}

impl TypedObject for ClassDef {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        let this = ctx.this.as_ref().unwrap();

        if let Some(type_id) = ctx
            .global_context
            .is_builtin(this.as_ref(), &ctx.module_ref)
        {
            return Ok(type_id);
        } else {
            todo!();
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        let scope = LocalScope::from(self.clone());

        for node in scope.inner.nodes.iter().map(|n| n.unspanned()) {
            node.typecheck(&ctx)?;
        }

        Ok(())
    }
}

impl LookupTarget for ClassDef {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
        self.name.name()
    }
}
