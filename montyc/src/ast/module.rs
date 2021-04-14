use std::rc::Rc;

use crate::{context::LocalContext, parser::{Parseable, ParserT, SpanEntry}, scope::LookupTarget, typing::{LocalTypeId, TypedObject}};

use super::{AstObject, Spanned, stmt::Statement};

#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Rc<Spanned<Statement>>>,
}

impl AstObject for Module {
    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(Self {
            body: self.body.iter().cloned().collect(),
        })
    }

    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        let it: Vec<Rc<dyn AstObject>> = self
            .body
            .iter()
            .cloned()
            .map(|s| Rc::new(s) as Rc<_>)
            .collect();

        let it = it.into_iter();

        Some(Box::new(it))
    }
}

impl TypedObject for Module {
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>
    ) -> Option<LocalTypeId> {
        None
    }

    fn typecheck<'a>(
        &self,
        ctx: LocalContext<'a>
    ) {
        todo!();
    }
}

impl Parseable for Module {
    const PARSER: ParserT<Self> = crate::parser::comb::module;
}

impl LookupTarget for Module {
    fn is_named(&self, target: SpanEntry) -> bool {
        false
    }

    fn name(&self) -> SpanEntry {
        todo!()
    }
}
