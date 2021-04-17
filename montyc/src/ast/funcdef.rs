use std::rc::Rc;

use crate::{
    context::LocalContext,
    func::Function,
    parser::SpanEntry,
    scope::{downcast_ref, LocalScope, LookupTarget, OpaqueScope, Scope, ScopeRoot},
    typing::{FunctionType, LocalTypeId, TaggedType, TypeMap, TypedObject},
    MontyError,
};

use super::{atom::Atom, primary::Primary, stmt::Statement, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: Spanned<Atom>,
    pub args: Option<Vec<(SpanEntry, Rc<Spanned<Primary>>)>>,
    pub body: Vec<Rc<dyn AstObject>>,
    // decorator_list: Option<Vec<Rc<dyn AstObject>>>,
    pub returns: Option<Spanned<Primary>>,
    // type_comment: Option<Rc<Expr>>,
}

impl<'a, 'b> From<(&'b FunctionDef, &'a LocalContext<'a>)> for FunctionType {
    fn from((def, ctx): (&'b FunctionDef, &'a LocalContext)) -> Self {
        let ret = match def.returns.as_ref() {
            Some(node) => match node.infer_type(&ctx) {
                Some(tid) => tid,
                None => ctx.error(MontyError::UndefinedVariable { node: Rc::new(def.returns.clone().unwrap()), ctx})
            },
            None => TypeMap::NONE_TYPE,
        };

        let name = if let Atom::Name(n) = def.name.inner {
            n
        } else {
            unreachable!();
        };

        let mut args = vec![];

        if let Some(def_args) = &def.args {
            for (arg_name, arg_ann) in def_args {
                let type_id = match arg_ann.infer_type(ctx) {
                    Some(tyid) => tyid,
                    None => ctx.error(MontyError::UndefinedVariable { node: arg_ann.clone(), ctx }),
                };

                args.push(type_id);
            }
        }

        Self {
            name,
            args,
            ret,
            decl: Some(Rc::new(def.clone())),
            resolver: ctx.global_context.resolver.clone(),
            module_ref: ctx.module_ref.clone(),
        }
    }
}

impl AstObject for FunctionDef {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        Some(Box::new(self.body.clone().into_iter()))
    }
}

impl TypedObject for FunctionDef {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        let func_type: FunctionType = (self, ctx).into();

        Some(ctx.global_context.type_map.borrow_mut().insert(func_type))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) {
        let type_id = self.infer_type(&ctx).unwrap();

        let this = ctx.this.as_ref().unwrap().unspanned();

        let this = if let Some(f) = downcast_ref::<FunctionDef>(this.as_ref()) {
            Spanned {
                span: f.name.span().unwrap(),
                inner: f.clone(),
            }
        } else {
            match downcast_ref::<Statement>(this.as_ref()) {
                Some(Statement::FnDef(f)) => Spanned {
                    span: f.returns.span().unwrap().clone(),
                    inner: f.clone(),
                },

                _ => panic!("{:?}", &ctx.this),
            }
        };

        let mut scope: LocalScope<FunctionDef> = LocalScope::from(self.clone()).into();

        scope.inner.module_ref.replace(ctx.module_ref.clone());
        scope.inner.parent = Some(ctx.scope.clone());

        let kind = ctx
            .global_context
            .type_map
            .borrow()
            .get_tagged::<FunctionType>(type_id)
            .unwrap()
            .unwrap();

        let func = Rc::new(Function {
            scope,
            kind,
            def: Rc::new(this),
        });

        let mut scope = func.scope.clone();
        scope.inner.root = ScopeRoot::Func(func.clone());

        let ctx = LocalContext {
            global_context: ctx.global_context,
            module_ref: ctx.module_ref.clone(),
            scope: Rc::new(scope) as Rc<_>,
            this: None,
            parent: ctx.parent.clone(),
        };

        func.typecheck(&ctx)
    }
}

impl LookupTarget for FunctionDef {
    fn is_named(&self, target: SpanEntry) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> SpanEntry {
        self.name.name()
    }
}
