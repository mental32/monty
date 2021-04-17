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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        match self {
            Atom::None => Some(TypeMap::NONE_TYPE),
            Atom::Ellipsis => Some(TypeMap::ELLIPSIS),
            Atom::Int(_) => Some(TypeMap::INTEGER),
            Atom::Str(_) => Some(TypeMap::STRING),
            Atom::Bool(_) => Some(TypeMap::BOOL),
            Atom::Float(_) => Some(TypeMap::FLOAT),
            Atom::Comment(_) => None,
            Atom::Name(None) => unreachable!(),
            Atom::Name(target) => {
                log::trace!("infer_type: performing name lookup on atom {:?}", self);

                let results = ctx.scope.lookup_def(target.clone(), &ctx.global_context);

                match results.as_slice() {
                    [] => None,
                    results => {
                        for top in results {
                            let top_u = top.unspanned();

                            if let Some(asn) = downcast_ref::<Assign>(top_u.as_ref()) {
                                return asn.value.inner.infer_type(ctx);
                            } else if let Some(atom) = downcast_ref::<Atom>(top.as_ref())
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
                                return top.infer_type(&ctx);
                            }
                        }

                        None
                    }
                }
            }
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) {
        log::trace!("typecheck: {:?}", self);

        if let Self::Name(target) = self {
            let result = ctx.scope.lookup_def(*target, ctx.global_context);

            match result.as_slice() {
                [] => ctx.error(MontyError::UndefinedVariable {
                    node: ctx.this.clone().unwrap(),
                    ctx: &ctx,
                }),
                [top, ..] => {
                    if let Some(asn) = crate::isinstance!(top.as_ref(), Assign).or(
                        crate::isinstance!(top.as_ref(), Statement).and_then(|s| match s {
                            Statement::Asn(a) => Some(a),
                            _ => None,
                        }),
                    ) {
                        return asn.typecheck(ctx);
                    }

                    match top.infer_type(&ctx) {
                        Some(_) => {}
                        None => ctx.error(MontyError::UnknownType {
                            node: ctx.this.clone().unwrap(),
                            ctx: &ctx,
                        }),
                    }
                }
            }
        } else {
            log::trace!("Skipping typecheck: {:?}", self);
        }
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
