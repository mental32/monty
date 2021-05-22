use std::rc::Rc;

use crate::{func::Function, prelude::*, typing::ClassType};

use super::{AstObject, Spanned, atom::Atom, expr::Expr, primary::Primary, stmt::Statement};

#[derive(Debug, Clone)]
pub struct TypedFuncArg {
    pub(crate) name: SpanRef,
    pub(crate) annotation: Rc<Spanned<Expr>>,
}

impl LookupTarget for TypedFuncArg {
    fn is_named(&self, target: SpanRef) -> bool {
        target == self.name
    }

    fn name(&self) -> Option<SpanRef> {
        Some(self.name.clone())
    }
}

impl AstObject for TypedFuncArg {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        None
    }
}

impl TypedObject for TypedFuncArg {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        ctx.with(Rc::clone(&self.annotation), |ctx, ann| ann.infer_type(&ctx))
    }

    fn typecheck<'a>(&self, _: &LocalContext<'a>) -> crate::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub reciever: Option<Spanned<Atom>>,
    pub name: Spanned<Atom>,
    pub args: Option<Vec<((SpanRef, SpanRef), Option<Rc<Spanned<Expr>>>)>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
    pub decorator_list: Vec<Rc<Spanned<Primary>>>,
    pub returns: Option<Rc<Spanned<Expr>>>,
    // type_comment: Option<Rc<Expr>>,
}

impl FunctionDef {
    pub fn is_dynamically_typed(&self) -> bool {
        self.args
            .as_ref()
            .map(|args| args.iter().any(|arg| arg.1.is_none()))
            .unwrap_or(false)
    }
}

impl<'a, 'b> From<(&'b FunctionDef, &'a LocalContext<'a>)> for FunctionType {
    fn from((def, ctx): (&'b FunctionDef, &'a LocalContext)) -> Self {
        let mut ret_ctx = ctx.clone();
        ret_ctx.this = def.returns.clone().map(|r| Rc::new(r) as Rc<_>);

        let ret = match def.returns.as_ref() {
            Some(node) => node.infer_type(&ret_ctx).unwrap_or_compiler_error(ctx),
            None => TypeMap::NONE_TYPE,
        };

        let ret = match ctx.global_context.type_map.get_tagged::<ClassType>(ret) {
            Some(Ok(klass)) => klass.inner.kind,
            Some(Err(_)) => ret,
            None => unreachable!(),
        };

        let name = if let Atom::Name(n) = def.name.inner {
            n
        } else {
            unreachable!();
        };

        let mut args = vec![];

        if let Some(def_args) = &def.args {
            for (_, ann) in def_args {
                let ann = ann.as_ref().unwrap();
                let mut ctx = ctx.clone();
                ctx.this = Some(ann.clone());

                let (ty, _) = {
                    let ty = match crate::utils::try_parse_union_literal(&ctx, &ann.inner, false)
                        .unwrap_or_compiler_error(&ctx)
                    {
                        Some(tys) => (ctx.global_context.type_map.tagged_union(tys), true),
                        None => {
                            let mut ctx = ctx.clone();
                            ctx.this = Some(Rc::clone(&ann) as Rc<_>);
                            let ty = ann.infer_type(&ctx).unwrap_or_compiler_error(&ctx);
                            (ty, false)
                        }
                    };

                    ctx.cache_type(&(Rc::clone(&ann) as Rc<dyn AstObject>), ty.0);

                    ty
                };

                args.push(ty);
            }
        }

        let reciever = if let Some(Spanned {
            inner: Atom::Name(r),
            ..
        }) = def.reciever
        {
            let mut ctx = ctx.clone();
            ctx.this = def
                .reciever
                .clone()
                .map(|rec| Rc::new(rec) as Rc<dyn AstObject>);

            Some(
                Atom::Name(r)
                    .infer_type(&ctx)
                    .unwrap_or_compiler_error(&ctx),
            )
        } else {
            None
        };

        Self {
            reciever,
            name,
            args,
            ret,
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
        let body: Vec<Rc<dyn AstObject>> = self.body.iter().map(|stmt| stmt.clone() as Rc<_>).collect();

        Some(Box::new(body.into_iter()))
    }
}

impl TypedObject for FunctionDef {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        if self.is_dynamically_typed() {
            return Err(MontyError::Unsupported {
                span: self.name.span.clone(),
                message: "Can not infer the type of a function that has unannotated arguments"
                    .into(),
            });
        }

        let func_type: FunctionType = (self, ctx).into();

        Ok(ctx.global_context.type_map.insert(func_type))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        if self.is_dynamically_typed() {
            return Err(MontyError::Unsupported {
                span: self.name.span.clone(),
                message: "Can not typecheck a function that has unannotated arguments".into(),
            });
        }

        let this = ctx.this.as_ref().unwrap();

        let func = Function::new(this, ctx).unwrap_or_compiler_error(ctx);

        let lctx = LocalContext {
            global_context: ctx.global_context,
            module_ref: ctx.module_ref.clone(),
            scope: Rc::clone(&func.scope) as Rc<_>,
            this: ctx.this.clone(),
            current_branch: None,
        };

        func.typecheck(&lctx)?;

        ctx.global_context.functions.borrow_mut().push(func);

        Ok(())
    }
}

impl LookupTarget for FunctionDef {
    fn is_named(&self, target: SpanRef) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> Option<SpanRef> {
        self.name.name()
    }
}

impl Lower<Layout<Rc<dyn AstObject>>> for FunctionDef {
    fn lower(&self) -> Layout<Rc<dyn AstObject>> {
        let mut layout = Layout::new();
        let mut prev = layout.start.clone();

        for object in self.body.iter() {
            let new = layout.insert_into_new_block(Rc::clone(&object) as Rc<_>);

            layout.succeed(prev, new);

            prev = new;
        }

        layout.succeed(prev, layout.end);

        layout
    }
}
