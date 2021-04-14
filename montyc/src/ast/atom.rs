use std::rc::Rc;

use typing::TypedObject;

use crate::{
    context::LocalContext,
    parser::{token::PyToken, Parseable, ParserT, SpanEntry},
    scope::{downcast_ref, LookupTarget, Scope},
    typing::{self, LocalTypeId, TypeMap},
};

use super::{assign::Assign, AstObject};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Atom {
    None,
    Ellipsis,
    Int(isize),
    Str(SpanEntry),
    Bool(bool),
    Float(f64),
    Name(SpanEntry),
}

impl Parseable for Atom {
    const PARSER: ParserT<Self> = crate::parser::comb::atom_unspanned;
}

impl From<PyToken> for Atom {
    fn from(value: PyToken) -> Self {
        match value {
            PyToken::Ellipsis => Self::Ellipsis,
            PyToken::None => Self::None,
            PyToken::True => Self::Bool(true),
            PyToken::False => Self::Bool(false),
            PyToken::Digits(n) => Self::Int(n),
            PyToken::SpanRef(n) => Self::Str(n),
            _ => unreachable!("{:?}", value),
        }
    }
}

impl AstObject for Atom {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }
}

impl TypedObject for Atom {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        match self {
            Atom::None => Some(TypeMap::NONE_TYPE),
            Atom::Ellipsis => Some(TypeMap::ELLIPSIS),
            Atom::Int(_) => Some(TypeMap::INTEGER),
            Atom::Str(_) => Some(TypeMap::STRING),
            Atom::Bool(_) => Some(TypeMap::BOOL),
            Atom::Float(_) => Some(TypeMap::FLOAT),
            Atom::Name(None) => unreachable!(),
            Atom::Name(target) => {
                log::trace!("infer_type: performing name lookup on atom {:?}", self);

                let results = ctx.scope.lookup_any(target.clone(), &ctx.global_context);

                match results.as_slice() {
                    [] => None,
                    [top, ..] => {
                        if let Some(asn) = downcast_ref::<Assign>(top.as_ref()) {
                            asn.value.inner.infer_type(ctx)
                        } else {
                            log::info!("infer_type: lookup successfull! top={:?}", top.name());
                            let mut ctx = ctx.clone();
                            ctx.this = Some(top.clone());
                            top.infer_type(&ctx)
                        }
                    }
                }
            }
        }
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        // always okay
    }
}

impl LookupTarget for Atom {
    fn is_named(&self, target: SpanEntry) -> bool {
        let target = match target {
            Some(n) => n,
            None => return false,
        };

        match self {
            Self::Name(Some(n)) => *n == target,
            _ => false,
        }
    }

    fn name(&self) -> SpanEntry {
        match self {
            Self::Name(n) => n.clone(),
            _ => None,
        }
    }
}
