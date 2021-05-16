#![allow(warnings)]

use std::{any::Any, collections::HashMap, rc::Rc};

use crate::{ast::primary::Primary, fmt::Formattable, prelude::{FunctionType, SpanRef}, scope::LookupTarget, typing::{TypeMap, TypedObject}, utils::lens};

use super::{expr::Expr, stmt::Statement, AstObject, Spanned};

#[derive(Debug, Clone)]
pub enum BranchTail {
    Else(Rc<Spanned<Statement>>),
    If(Rc<Spanned<If>>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Rc<Spanned<Expr>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl AstObject for If {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> std::rc::Rc<dyn AstObject> {
        todo!()
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        None
    }
}

impl LookupTarget for If {
    fn is_named(&self, target: SpanRef) -> bool {
        false
    }

    fn name(&self) -> Option<SpanRef> {
        None
    }
}

impl TypedObject for If {
    fn infer_type<'a>(
        &self,
        ctx: &crate::context::LocalContext<'a>,
    ) -> crate::Result<crate::typing::LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, ctx: &crate::context::LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:ifstmt {:?}", self);

        let (test_t, id) = ctx.with(Rc::clone(&self.test), |ctx, test| {
            test.typecheck(&ctx)?;

            let id = {
                let id = ctx
                .global_context
                .database
                .entry(ctx.this.clone().unwrap(), &ctx.module_ref);
    
                ctx.global_context.database.id_of(&id).unwrap()
            };        

            let ty = test.infer_type(&ctx)?;

            if let Expr::Primary(Spanned { inner: Primary::Call { func, args: Some(args) }, .. }) = &test.inner {

                let func_t = ctx.with(Rc::clone(func), |ctx, this| this.infer_type(&ctx))?;

                match ctx.global_context.type_map.get_tagged::<FunctionType>(func_t) {
                    Some(Ok(func_t)) => {
                        for (name, (func, _)) in ctx.global_context.builtin_functions.iter() {
                            if ctx.global_context.type_map.unify_func(func.kind.type_id, &func_t.inner) && *name == ctx.global_context.magical_name_of("isinstance").unwrap().0 {
                                let mut branches = ctx.global_context.branches.borrow_mut();
                                let branch_info = branches.entry(id).or_insert_with(|| HashMap::new());

                                let subject = &args[0];
                                let subject = ctx.global_context.database.entry((Rc::clone(subject) as Rc<_>), &ctx.module_ref);
                                let subject = ctx.global_context.database.id_of(&subject).unwrap();

                                let kind = &args[1];
                                let kind = ctx.with(Rc::clone(kind), |ctx, this| this.infer_type(&ctx))?;

                                branch_info.entry(subject).or_default().push(kind.canonicalize(&ctx.global_context.type_map));
                            }
                        }
                    },
                    _ => (),
                };
            }

            Ok((ty, id))
        })?;

        if !ctx.global_context.type_map.type_eq(test_t, TypeMap::BOOL) {
            todo!("{:?}", (test_t, TypeMap::BOOL));
        }

        for node in self.body.clone().into_iter().map(|s| s as Rc<_>) {
            let mut ctx = ctx.clone();
            ctx.this = Some(Rc::clone(&node));
            ctx.current_branch = Some(id);
            node.typecheck(&ctx)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IfChain {
    pub branches: Vec<Rc<Spanned<If>>>,
    pub orelse: Option<Vec<Rc<Spanned<Statement>>>>,
}

impl AstObject for IfChain {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> std::rc::Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        let mut it = self.branches.iter();

        let head = it.next()?;

        let mut objects = head.walk();

        while let Some(elif) = it.next() {
            if let Some(elif) = elif.walk() {
                if let Some(oit) = objects {
                    objects = Some(Box::new(oit.chain(elif)));
                } else {
                    objects = Some(elif);
                }
            }
        }

        if let Some(orelse) = &self.orelse {
            let orelse = Box::new(orelse.clone().into_iter().map(|o| o as Rc<dyn AstObject>));

            if let Some(oit) = objects {
                objects = Some(Box::new(oit.chain(orelse)));
            } else {
                objects = Some(orelse);
            }
        }

        objects
    }
}

impl LookupTarget for IfChain {
    fn is_named(&self, target: SpanRef) -> bool {
        false
    }

    fn name(&self) -> Option<SpanRef> {
        None
    }
}

impl TypedObject for IfChain {
    fn infer_type<'a>(
        &self,
        ctx: &crate::context::LocalContext<'a>,
    ) -> crate::Result<crate::typing::LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, ctx: &crate::context::LocalContext<'a>) -> crate::Result<()> {
        for elif in self.branches.iter() {
            ctx.with(Rc::clone(&elif), |mut ctx, elif| {
                elif.typecheck(&ctx)
            })?;
        }

        if let Some(orelse) = &self.orelse {
            for stmt in orelse.iter() {
                ctx.with(Rc::clone(&stmt), |ctx, stmt| stmt.typecheck(&ctx))?;
            }
        }

        Ok(())
    }
}
