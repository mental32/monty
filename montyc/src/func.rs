use std::{marker::PhantomData, rc::Rc};

use crate::{ast::{funcdef::FunctionDef, retrn::Return, AstObject}, context::LocalContext, scope::{downcast_ref, LocalScope, Scope, ScopeRoot}, typing::{FunctionType, TaggedType, TypeMap, TypedObject}};

#[derive(Debug)]
pub struct Function {
    pub scope: LocalScope<FunctionDef>,
    pub kind: TaggedType<FunctionType>,
}

impl TypedObject for Function {
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>
    ) -> Option<crate::typing::LocalTypeId> {
        Some(self.kind.type_id)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        let mut implicit_return = true;

        for node in self.scope.inner.nodes.iter().map(|n| n.unspanned()) {
            if downcast_ref::<Return>(node.as_ref()).is_some() {
                implicit_return = false;
            }

            node.typecheck(ctx.clone())
        }

        if implicit_return && self.kind.inner.ret != TypeMap::NONE_TYPE {
            panic!("return error")
        }
    }
}
