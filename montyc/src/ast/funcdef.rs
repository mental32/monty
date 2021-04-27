use std::rc::Rc;

use dashmap::DashMap;

use crate::{func::Function, prelude::*, scope::ScopeRoot};

use super::{atom::Atom, primary::Primary, stmt::Statement, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub reciever: Option<Spanned<PyToken>>,
    pub name: Spanned<Atom>,
    pub args: Option<Vec<(SpanEntry, Rc<Spanned<Primary>>)>>,
    pub body: Vec<Rc<dyn AstObject>>,
    // pub decorator_list: Vec<Rc<Spanned<Primary>>>,
    pub returns: Option<Spanned<Primary>>,
    // type_comment: Option<Rc<Expr>>,
}

impl<'a, 'b> From<(&'b FunctionDef, &'a LocalContext<'a>)> for FunctionType {
    fn from((def, ctx): (&'b FunctionDef, &'a LocalContext)) -> Self {
        let ret = match def.returns.as_ref() {
            Some(node) => match node.infer_type(&ctx) {
                Ok(tid) => tid,
                Err(_) => ctx.exit_with_error(MontyError::UndefinedVariable {
                    node: Rc::new(def.returns.clone().unwrap()),
                }),
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
            for (_arg_name, arg_ann) in def_args {
                let type_id = match arg_ann.infer_type(ctx) {
                    Ok(tyid) => tyid,
                    Err(err) => ctx.exit_with_error(err),
                };

                args.push(type_id);
            }
        }

        let reciever = if let Some(Spanned {
            inner: PyToken::Ident(r),
            ..
        }) = def.reciever
        {
            Some(Atom::Name(r).infer_type(ctx).unwrap_or_compiler_error(ctx))
        } else {
            None
        };

        Self {
            reciever,
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        let func_type: FunctionType = (self, ctx).into();

        Ok(ctx.global_context.type_map.insert(func_type))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        let type_id = self.infer_type(&ctx).unwrap();

        let this = ctx.this.as_ref().unwrap().unspanned();

        let this = if let Some(f) = this.as_ref().downcast_ref::<FunctionDef>() {
            Spanned {
                span: f.name.span().unwrap(),
                inner: f.clone(),
            }
        } else {
            match this.as_ref().downcast_ref::<Statement>() {
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
            .get_tagged::<FunctionType>(type_id)
            .unwrap()
            .unwrap();

        let mut func = Rc::new(Function {
            scope,
            kind,
            vars: DashMap::default(),
            def: Rc::new(this),
        });

        let mut root = ScopeRoot::Func(Rc::clone(&func));

        // SAFETY: The only other reference to `func` is in the scope root.
        //         which doesn't get dereferenced in the call to `std::mem::swap`.
        unsafe {
            let func = Rc::get_mut_unchecked(&mut func);
            std::mem::swap(&mut func.scope.inner.root, &mut root);
        }

        let ctx = LocalContext {
            global_context: ctx.global_context,
            module_ref: ctx.module_ref.clone(),
            scope: Rc::new(func.scope.clone()) as Rc<_>,
            this: ctx.this.clone(),
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

impl Lower<Layout<Rc<dyn AstObject>>> for FunctionDef {
    fn lower(&self) -> Layout<Rc<dyn AstObject>> {
        let mut layout = Layout::new();
        let mut prev = layout.start.clone();

        for object in self.body.iter() {
            let new = layout.insert_into_new_block(object.clone());

            layout.succeed(prev, new);

            prev = new;
        }

        layout.succeed(prev, layout.end);

        layout
    }
}
