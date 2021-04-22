use std::{rc::Rc};

use crate::{ast::{funcdef::FunctionDef, retrn::Return, stmt::Statement}, prelude::*, scope::ScopeRoot, typing::TaggedType};

#[derive(Debug)]
pub struct Function {
    pub def: Rc<Spanned<FunctionDef>>,
    pub scope: LocalScope<FunctionDef>,
    pub kind: TaggedType<FunctionType>,
}

impl Function {
    pub fn new(def: &FunctionDef, ctx: &LocalContext) -> Self {

        let def = Rc::new(Spanned {
            inner: def.clone(),
            span: def.name.span.clone(),
        });

        let scope = LocalScope::from(def.inner.clone());

        let kind = {
            let type_id = def.infer_type(ctx).unwrap_or_compiler_error(ctx);
            ctx.global_context.type_map.get_tagged(type_id).unwrap().unwrap()
        };

        Self {
            def,
            scope,
            kind,
        }
    }
}


impl TypedObject for Function {
    fn infer_type<'a>(&self, _: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(self.kind.type_id)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:function {}", &self.kind.inner);

        let mut implicit_return = true;

        for scoped_object in self.scope.iter() {
            let node = scoped_object.object.clone();

            if node.as_ref().downcast_ref::<Spanned<Return>>().is_some()
                || node.as_ref().downcast_ref::<Return>().is_some()
                || node.as_ref().downcast_ref::<Spanned<Statement>>()
                    .map(|Spanned { inner, .. }| matches!(inner, Statement::Ret(_)))
                    .unwrap_or(false)
            {
                implicit_return = false;
            }

            scoped_object.with_context(ctx.global_context, |local_context, object| {
                object.typecheck(&local_context)
            })?;
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

impl Lower for &Function {
    type Output = Layout<Rc<dyn AstObject>>;

    fn lower(&self) -> Self::Output {
        self.def.inner.lower()
    }
}
