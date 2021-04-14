use std::rc::Rc;

use crate::{
    context::LocalContext,
    func::Function,
    parser::SpanEntry,
    scope::{downcast_ref, LocalScope, LookupTarget, OpaqueScope, Scope, ScopeRoot},
    typing::{FunctionType, LocalTypeId, TaggedType, TypeMap, TypedObject},
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
            Some(node) => node.infer_type(&ctx).unwrap(),
            None => TypeMap::NONE_TYPE,
        };

        let name = if let Atom::Name(n) = def.name.inner {
            n
        } else {
            unreachable!();
        };

        Self {
            name,
            args: vec![],
            ret,
            decl: None,
        }
    }
}

impl AstObject for FunctionDef {
    fn span(&self) -> Option<logos::Span> {
        todo!()
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

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        let type_id = self.infer_type(&ctx).unwrap();

        let this = ctx.this.unwrap().as_ref().unspanned();
        let this = match downcast_ref::<Statement>(this.as_ref()) {
            Some(Statement::FnDef(f)) => Spanned {
                span: f.returns.span().unwrap().clone(),
                inner: f.clone(),
            },
            x => panic!("{:?}", x),
        };

        let scope = LocalScope::from(self.clone()).into();

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
            module_ref: ctx.module_ref,
            scope: Rc::new(scope) as Rc<_>,
            this: None,
        };

        func.typecheck(ctx)
    }
}

impl LookupTarget for FunctionDef {
    fn is_named(&self, target: SpanEntry) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> SpanEntry {
        todo!()
    }
}
