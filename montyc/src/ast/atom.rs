use std::rc::Rc;

use typing::TypedObject;

use crate::{
    context::LocalContext,
    parser::{token::PyToken, Parseable, ParserT, SpanEntry},
    scope::{downcast_ref, LookupTarget, Scope},
    typing::{self, LocalTypeId, TypeMap},
    MontyError,
};

use super::{assign::Assign, primary::Primary, stmt::Statement, AstObject};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Atom {
    None,
    Ellipsis,
    Int(isize),
    Str(SpanEntry),
    Bool(bool),
    Float(f64),
    Comment(SpanEntry),
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
            PyToken::CommentRef(n) => Self::Comment(n),
            PyToken::StringRef(n) => Self::Str(n),
            PyToken::Ident(n) => Self::Name(n),
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'_>) -> crate::Result<LocalTypeId> {
        match self {
            Atom::None => Ok(TypeMap::NONE_TYPE),
            Atom::Ellipsis => Ok(TypeMap::ELLIPSIS),
            Atom::Int(_) => Ok(TypeMap::INTEGER),
            Atom::Str(_) => Ok(TypeMap::STRING),
            Atom::Bool(_) => Ok(TypeMap::BOOL),
            Atom::Float(_) => Ok(TypeMap::FLOAT),
            Atom::Comment(_) => todo!(),
            Atom::Name(None) => unreachable!(),
            Atom::Name(target) => {
                log::trace!("infer_type: performing name lookup on atom {:?}", self);

                let results = ctx.scope.lookup_def(target.clone(), &ctx.global_context);

                if results.is_empty() {
                    ctx.error(MontyError::UndefinedVariable {
                        node: ctx.this.clone().unwrap(),
                    });
                }

                for top in results {
                    let top_u = top.unspanned();

                    if let Some(asn) = downcast_ref::<Assign>(top_u.as_ref()) {
                        match asn.value.inner.infer_type(ctx) {
                            Ok(i) => return Ok(i),
                            Err(err) => ctx.error(err),
                        }
                    } else if let Some(atom) =
                        downcast_ref::<Atom>(top.as_ref())
                            .cloned()
                            .or(downcast_ref::<Primary>(top.as_ref()).and_then(|p| {
                                if let Primary::Atomic(atom) = p {
                                    Some(atom.inner.clone())
                                } else {
                                    None
                                }
                            }))
                    {
                        if atom == *self {
                            continue;
                        } else {
                            todo!()
                        }
                    } else {
                        log::trace!("infer_type: lookup successfull! top={:?}", top.name());
                        let mut ctx = ctx.clone();
                        ctx.this = Some(top.clone());

                        match top.infer_type(&ctx) {
                            Err(err) => ctx.error(err),
                            Ok(i) => return Ok(i),
                        }
                    }
                }

                ctx.error(MontyError::UndefinedVariable {
                    node: ctx.this.clone().unwrap(),
                });
            }
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck: {:?}", self);

        if let Self::Name(target) = self {
            let _ = self.infer_type(ctx)?;
        } else {
            log::trace!("Skipping typecheck: {:?}", self);
        }

        Ok(())
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
