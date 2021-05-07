use std::rc::Rc;

use crate::prelude::*;

use super::{expr::Expr, stmt::Statement};

#[derive(Debug, Clone)]
pub struct While {
    pub test: Rc<Spanned<Expr>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl LookupTarget for While {
    fn is_named(&self, _: SpanRef) -> bool {
        false
    }

    fn name(&self) -> Option<SpanRef> {
        None
    }
}

impl AstObject for While {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone()) as _
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        Some(Box::new(self.body.clone().into_iter().map(|o| o as Rc<_>)))
    }
}

impl TypedObject for While {
    fn infer_type<'a>(&self, _: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(TypeMap::NONE_TYPE)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:while {:?}", self);

        {
            let mut ctx = ctx.clone();
            ctx.this = Some(self.test.clone());

            let test_ty = self.test.infer_type(&ctx)?;

            if !ctx.global_context.type_map.type_eq(test_ty, TypeMap::BOOL) {
                todo!("{:?}", (test_ty, TypeMap::BOOL));
            }
        }

        for thing in self.body.iter() {
            {
                let mut ctx = ctx.clone();
                ctx.this = Some(thing.clone());

                thing.typecheck(&ctx)?;
            }
        }

        Ok(())
    }
}
