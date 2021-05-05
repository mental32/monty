use std::rc::Rc;

use crate::prelude::*;

use super::{expr::Expr, stmt::Statement};

#[derive(Debug, Clone)]
pub struct While {
    pub test: Rc<Spanned<Expr>>,
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl LookupTarget for While {
    fn is_named(&self, _: SpanEntry) -> bool {
        false
    }

    fn name(&self) -> SpanEntry {
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

            if self.test.infer_type(&ctx)? != TypeMap::BOOL {
                todo!("boolean in if");
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
