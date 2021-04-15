use std::{marker::PhantomData, rc::Rc};

use crate::{
    ast::{funcdef::FunctionDef, retrn::Return, stmt::Statement, AstObject, Spanned},
    context::LocalContext,
    scope::{downcast_ref, LocalScope, Scope, ScopeRoot},
    typing::{FunctionType, TaggedType, TypeMap, TypedObject},
    MontyError,
};

#[derive(Debug)]
pub struct Function {
    pub def: Rc<Spanned<FunctionDef>>,
    pub scope: LocalScope<FunctionDef>,
    pub kind: TaggedType<FunctionType>,
}

impl TypedObject for Function {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<crate::typing::LocalTypeId> {
        Some(self.kind.type_id)
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        let mut implicit_return = true;

        for scoped_object in self.scope.iter() {
            let node = scoped_object.object.clone();

            if downcast_ref::<Spanned<Return>>(node.as_ref()).is_some()
                || downcast_ref::<Return>(node.as_ref()).is_some()
                || downcast_ref::<Spanned<Statement>>(node.as_ref())
                    .map(|Spanned { inner, .. }| matches!(inner, Statement::Ret(_)))
                    .unwrap_or(false)
            {
                implicit_return = false;
            }

            scoped_object.with_context(ctx.global_context, |local_context, object| {
                object.typecheck(local_context)
            });
        }

        if implicit_return && self.kind.inner.ret != TypeMap::NONE_TYPE {
            let def_node = match ctx.scope.root() {
                ScopeRoot::Func(f) => f.def.clone(),
                _ => unreachable!(),
            };

            ctx.error(MontyError::MissingReturn {
                expected: TypeMap::NONE_TYPE,
                actual: self.kind.inner.ret,
                def_node,
                ctx: &ctx,
            });
        }
    }
}
