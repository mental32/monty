use std::rc::Rc;

use crate::{context::LocalContext, parser::{Parseable, ParserT}, scope::LookupTarget, typing::{LocalTypeId, TypeMap, TypedObject}};

use super::{AstObject, ObjectIter, Spanned, atom::Atom, expr::Expr};

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Spanned<Atom>,
    pub value: Spanned<Expr>,
    pub kind: Option<Spanned<Atom>>,
}

impl Parseable for Assign {
    const PARSER: ParserT<Self> = crate::parser::comb::assignment_unspanned;
}

impl AstObject for Assign {
    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn walk<'a>(&'a self) -> Option<ObjectIter> {
        let mut it = vec![
            // Rc::new(self.name.clone()) as Rc<dyn AstObject>,
            Rc::new(self.value.clone()) as Rc<dyn AstObject>,
        ];

        if let Some(kind) = self.kind.clone() {
            it.push(Rc::new(kind) as Rc<dyn AstObject>);
        }

        Some(Box::new(it.into_iter()))
    }
}

impl TypedObject for Assign {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Err(crate::MontyError::InferenceFailure)  // assignments do not have types, their values do however.
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        let expected = self.kind.as_ref().ok_or(crate::MontyError::InferenceFailure).and_then(|at| at.infer_type(&ctx))?;

        let actual = {
            let mut ctx = ctx.clone();
            ctx.this = Some((Rc::new(self.value.clone()) as Rc<dyn AstObject>));
            self.value.infer_type(&ctx)?
        };

        if expected != actual {
            todo!("assignment type error");
        }

        Ok(())

    }
}

impl LookupTarget for Assign {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
        self.name.name()
    }
}
