use std::rc::Rc;

use typing::TypedObject;

use crate::{context::LocalContext, parser::{Parseable, ParserT, SpanEntry, token::PyToken}, scope::{downcast_ref, Scope}, typing::{self, LocalTypeId, TypeMap}};

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
                let key = |o: &dyn AstObject| -> bool {
                    let asn = match downcast_ref::<Assign>(o) {
                        Some(asn) => asn,
                        None => return false,
                    };

                    match &asn.name.inner {
                        Atom::Name(key) => key == target,
                        _ => unreachable!(),
                    }
                };

                let obj = ctx.scope.lookup(&key)?;

                downcast_ref::<Assign>(obj.unspanned().as_ref())?
                    .value
                    .infer_type(ctx)
            }
        }
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        // always okay
    }
}
