use std::{marker::PhantomData, rc::Rc};

use crate::{ast::{funcdef::FunctionDef, retrn::Return, stmt::Statement}, prelude::*, scope::ScopeRoot, typing::TaggedType};

#[derive(Debug)]
pub struct Function {
    pub def: Rc<Spanned<FunctionDef>>,
    pub scope: LocalScope<FunctionDef>,
    pub kind: TaggedType<FunctionType>,
}

impl TypedObject for Function {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(self.kind.type_id)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck: {}", &self.kind.inner);

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
                object.typecheck(&local_context)
            });
        }

        if implicit_return && self.kind.inner.ret != TypeMap::NONE_TYPE {
            let def_node = match ctx.scope.root() {
                ScopeRoot::Func(f) => f.def.clone(),
                _ => unreachable!(),
            };

            ctx.exit_with_error(MontyError::MissingReturn {
                expected: TypeMap::NONE_TYPE,
                actual: self.kind.inner.ret,
                def_node,
            });
        }

        Ok(())
    }
}
