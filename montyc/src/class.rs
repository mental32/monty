use std::{collections::HashMap, num::NonZeroUsize};

use crate::{ast::class::ClassDef, context::LocalContext, scope::LocalScope, typing::{LocalTypeId, TypedObject}};

#[derive(Debug)]
pub struct Class {
    pub scope: LocalScope<ClassDef>,
    pub kind: LocalTypeId,
    pub properties: HashMap<NonZeroUsize, LocalTypeId>,
}

impl TypedObject for Class {
    fn infer_type<'a>(&self, ctx: &crate::context::LocalContext<'a>) -> crate::Result<LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        todo!()
    }
}
