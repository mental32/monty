use std::rc::Rc;

use super::{stmt::Statement, AstObject, Spanned};
use crate::prelude::*;

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
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        todo!();
    }

    fn typecheck<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<()> {
        todo!();
    }
}

impl Parseable for Module {
    const PARSER: ParserT<Self> = crate::parser::comb::module;
}

impl LookupTarget for Module {
    fn is_named(&self, _target: SpanRef) -> bool {
        false
    }

    fn name(&self) -> Option<SpanRef> {
        todo!()
    }
}

impl Lower<Layout<Rc<Spanned<Statement>>>> for Module {
    fn lower(&self) -> Layout<Rc<Spanned<Statement>>> {
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
