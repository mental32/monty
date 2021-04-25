use std::{marker::PhantomData, rc::Rc};

use dashmap::DashMap;

use crate::{
    ast::{funcdef::FunctionDef, retrn::Return, stmt::Statement},
    prelude::*,
    scope::ScopeRoot,
    typing::TaggedType,
};

#[derive(Debug)]
pub struct Function {
    pub def: Rc<Spanned<FunctionDef>>,
    pub scope: LocalScope<FunctionDef>,
    pub kind: TaggedType<FunctionType>,
    pub vars: DashMap<SpanEntry, (LocalTypeId, Span)>,
}

impl Function {
    pub fn new(def: Rc<dyn AstObject>, ctx: &LocalContext) -> Self {
        let fndef = def.as_ref().as_function().unwrap();

        let mut scope = OpaqueScope::from(def.clone());

        scope.module_ref = Some(ctx.module_ref.clone());

        let scope = LocalScope {
            _t: PhantomData,
            inner: scope,
        };

        let kind = {
            let type_id = def.infer_type(ctx).unwrap_or_compiler_error(ctx);

            ctx.global_context
                .type_map
                .get_tagged(type_id)
                .unwrap()
                .unwrap()
        };

        let def = Rc::new(Spanned {
            inner: fndef.clone(),
            span: fndef.name.span.clone(),
        });

        let vars = DashMap::default();

        Self { def, scope, kind, vars }
    }
}

impl TypedObject for Function {
    fn infer_type<'a>(&self, _: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(self.kind.type_id)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:function {}", &self.kind.inner);

        assert_matches!(ctx.scope.root(), ScopeRoot::Func(_));

        let mut implicit_return = true;

        for scoped_object in self.scope.iter() {
            let node = scoped_object.object.clone();

            assert_matches!(scoped_object.scope.root(), ScopeRoot::Func(_));

            if node.as_ref().downcast_ref::<Spanned<Return>>().is_some()
                || node.as_ref().downcast_ref::<Return>().is_some()
                || node
                    .as_ref()
                    .downcast_ref::<Spanned<Statement>>()
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
