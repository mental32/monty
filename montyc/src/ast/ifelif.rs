#![allow(warnings)]

use std::rc::Rc;

use crate::{prelude::SpanRef, scope::LookupTarget, typing::{TypeMap, TypedObject}};

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
        Some(Box::new(self.body.clone().into_iter().map(|s| s as Rc<_>)))
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

        let mut ctx = ctx.clone();
        ctx.this = Some(self.test.clone());

        let test_t = ctx.with(Rc::clone(&self.test), |ctx, test| test.infer_type(&ctx))?;

        if !ctx.global_context.type_map.type_eq(test_t, TypeMap::BOOL) {
            todo!("{:?}", (test_t, TypeMap::BOOL));
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
        todo!()
    }

    fn unspanned(&self) -> std::rc::Rc<dyn AstObject> {
        todo!()
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
            ctx.with(Rc::clone(&elif), |ctx, elif| elif.typecheck(&ctx))?;
        }

        if let Some(orelse) = &self.orelse {
            for stmt in orelse.iter() {
                ctx.with(Rc::clone(&stmt), |ctx, stmt| stmt.typecheck(&ctx))?;
            }
        }

        Ok(())
    }
}
