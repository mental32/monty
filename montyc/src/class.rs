use std::collections::HashMap;

use crate::{
    ast::class::ClassDef,
    context::LocalContext,
    prelude::SpanRef,
    scope::{LocalScope, LookupTarget},
    typing::{FunctionType, LocalTypeId, TypeDescriptor, TypedObject},
};

#[derive(Debug)]
pub struct Class {
    pub scope: LocalScope<ClassDef>,
    pub kind: LocalTypeId,
    pub type_id: LocalTypeId,
    pub name: SpanRef,
    pub properties: HashMap<SpanRef, LocalTypeId>,
}

impl Class {
    pub fn new(ctx: &LocalContext<'_>, def: &ClassDef) -> Self {
        let (kind, kty) = match ctx.global_context.is_builtin(def) {
            Some(type_id) => type_id,
            None => unimplemented!("{:?}", def),
        };

        let scope = LocalScope::from(def.clone());

        Self {
            name: def.name.name().unwrap(),
            scope,
            kind,
            type_id: kty,
            properties: HashMap::new(),
        }
    }
}

impl Class {
    pub fn try_unify_method(
        &self,
        ctx: &LocalContext,
        template: &FunctionType,
    ) -> Option<LocalTypeId> {
        for (name, kind) in self.properties.iter() {
            if ctx
                .global_context
                .span_ref
                .borrow()
                .crosspan_eq(*name, template.name)
            {
                if ctx
                    .global_context
                    .type_map
                    .unify_func(kind.clone(), &template)
                {
                    let ret = match ctx
                        .global_context
                        .type_map
                        .get(kind.clone())
                        .map(|i| i.value().clone())
                    {
                        Some(TypeDescriptor::Function(f)) => f.ret,
                        _ => todo!(),
                    };

                    return Some(ret);
                }
            }
        }

        None
    }
}

impl TypedObject for Class {
    fn infer_type<'a>(&self, _ctx: &crate::context::LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(self.kind)
    }

    fn typecheck<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<()> {
        todo!()
    }
}
