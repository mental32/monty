use std::rc::Rc;

use crate::{func::Function, prelude::*};

use super::{atom::Atom, primary::Primary, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct TypedFuncArg {
    pub(crate) name: SpanEntry,
    pub(crate) annotation: Rc<Spanned<Primary>>,
}

impl LookupTarget for TypedFuncArg {
    fn is_named(&self, target: SpanEntry) -> bool {
        target == self.name
    }

    fn name(&self) -> SpanEntry {
        self.name.clone()
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
        self.annotation.infer_type(ctx)
    }

    fn typecheck<'a>(&self, _: &LocalContext<'a>) -> crate::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub reciever: Option<Spanned<Atom>>,
    pub name: Spanned<Atom>,
    pub args: Option<Vec<(SpanEntry, Rc<Spanned<Primary>>)>>,
    pub body: Vec<Rc<dyn AstObject>>,
    pub decorator_list: Vec<Rc<Spanned<Primary>>>,
    pub returns: Option<Spanned<Primary>>,
    // type_comment: Option<Rc<Expr>>,
}

impl<'a, 'b> From<(&'b FunctionDef, &'a LocalContext<'a>)> for FunctionType {
    fn from((def, ctx): (&'b FunctionDef, &'a LocalContext)) -> Self {
        let mut ret_ctx = ctx.clone();
        ret_ctx.this = def.returns.clone().map(|r| Rc::new(r) as Rc<_>);

        let ret = match def.returns.as_ref() {
            Some(node) => match node.infer_type(&ret_ctx) {
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
                let mut ctx = ctx.clone();
                ctx.this = Some(arg_ann.clone());

                let type_id = match arg_ann.infer_type(&ctx) {
                    Ok(tyid) => tyid,
                    Err(err) => ctx.exit_with_error(err),
                };

                args.push(type_id);
            }
        }

        let reciever = if let Some(Spanned {
            inner: Atom::Name(r),
            ..
        }) = def.reciever
        {
            let mut ctx = ctx.clone();
            ctx.this = def.reciever.clone().map(|rec| Rc::new(rec) as Rc<dyn AstObject>);

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
        Some(Box::new(self.body.clone().into_iter()))
    }
}

impl TypedObject for FunctionDef {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        let func_type: FunctionType = (self, ctx).into();

        Ok(ctx.global_context.type_map.insert(func_type))
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {

        let this = ctx.this.as_ref().unwrap();

        let func = Function::new(this, ctx).unwrap_or_compiler_error(ctx);

        let lctx = LocalContext {
            global_context: ctx.global_context,
            module_ref: ctx.module_ref.clone(),
            scope: Rc::new(func.scope.clone()) as Rc<_>,
            this: ctx.this.clone(),
        };

        func.typecheck(&lctx)?;

        ctx.global_context.functions.borrow_mut().push(func);

        Ok(())
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
            let new = layout.insert_into_new_block(Rc::clone(&object));

            layout.succeed(prev, new);

            prev = new;
        }

        layout.succeed(prev, layout.end);

        layout
    }
}
