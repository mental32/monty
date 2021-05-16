use std::{convert::TryFrom, rc::Rc};

use crate::{ast::{class::ClassDef, module::Module}, prelude::*, scope::LookupOrder, typing::Generic};

use super::{assign::Assign, expr::Expr, stmt::Statement, AstObject};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringRef(pub SpanRef);

impl StringRef {
    pub fn resolve_as_string(&self, global_context: &GlobalContext) -> Option<String> {
        let string_data = global_context.strings.get(self)?;

        let mref = string_data.value().mref.clone();
        let span = global_context.span_ref.borrow().get(self.0)?;

        let refm = global_context.resolver.sources.get(&mref).unwrap();

        let st = refm.value().get(span)?;

        let st = match st {
            "\"\"" => "",
            st => &st[1..(st.len() - 1)],
        };

        Some(st.to_string())
    }
}

impl TryFrom<Atom> for StringRef {
    type Error = Atom;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        match value {
            Atom::Str(n) => Ok(Self(n)),
            _ => Err(value),
        }
    }
}

impl From<StringRef> for SpanRef {
    fn from(st: StringRef) -> Self {
        st.0
    }
}

#[derive(Debug, Clone)]
pub enum Atom {
    None,
    Ellipsis,
    Int(isize),
    Str(SpanRef),
    Bool(bool),
    Float(f64),
    Tuple(Vec<Rc<Spanned<Expr>>>),
    Comment(SpanRef),
    Name((SpanRef, SpanRef)),
}

impl Atom {
    pub fn as_name(&self) -> Option<(SpanRef, SpanRef)> {
        match self {
            Self::Name(t) => Some(t.clone()),
            _ => None,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(a), Self::Bool(k)) => a == k,
            (Self::Int(n), Self::Int(i)) => n == i,
            (Self::Str(s), Self::Str(t)) => s == t,
            (Self::Name((a, _)), Self::Name((b, _))) => a == b,


            (Self::Ellipsis, Self::Ellipsis)
            | (Self::None, Self::None) => true,

            _ => unimplemented!(),
        }
    }
}

impl Atom {
    pub fn resolve_name_to_definition(
        &self,
        scope: &dyn Scope,
        global_context: &GlobalContext,
    ) -> Option<Rc<dyn AstObject>> {
        let target = if let Self::Name((t, _)) = self { t } else { todo!() };

        scope
            .lookup_def(
                target.clone(),
                &global_context,
                LookupOrder::Unspecified,
            )
            .unwrap()
            .drain(..)
            .next()
    }
}

impl Parseable for Atom {
    const PARSER: ParserT<Self> = crate::parser::comb::atom::atom_unspanned;
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

            Atom::Tuple(values) => {
                let mut inner = vec![];

                for value in values {
                    let ty = ctx.with(Rc::clone(value), |ctx, value| value.infer_type(&ctx))?.canonicalize(&ctx.global_context.type_map);
                    inner.push(ty);
                }

                let ty = ctx
                    .global_context
                    .type_map
                    .entry(TypeDescriptor::Generic(Generic::Struct { inner }));

                Ok(ty)
            }

            Atom::Name((target, _)) => {
                log::trace!("infer_type:atom performing name lookup on atom {:?}", self);

                let results = ctx
                    .scope
                    .lookup_def(
                        target.clone(),
                        &ctx.global_context,
                        LookupOrder::ControlFlowSensitive(ctx.this.clone().unwrap()),
                    )
                    .unwrap_or_compiler_error(ctx);

                if results.is_empty() {
                    ctx.exit_with_error(MontyError::UndefinedVariable {
                        node: ctx.this.clone().unwrap(),
                    });
                }

                for top in results {
                    let top_ = top.unspanned();

                    if let Some(asn) = crate::isinstance!(top.as_ref(), Assign).or_else(
                        || crate::isinstance!(top_.as_ref(), Statement, Statement::Asn(n) => n),
                    ) {
                        if let Some(ty) = ctx.global_context.database.type_of(&top, None) {
                            return Ok(ty)
                        }

                        let expected = {
                            let mut ctx = ctx.clone();
                            ctx.this = Some(Rc::clone(&top));
                            asn.expected(&ctx)?
                        };

                        return match expected {
                            Some((ty, _)) => Ok(ty),
                            None => asn.value.inner.infer_type(ctx)
                        };

                    } else if let Some(klass) = crate::isinstance!(top.as_ref(), ClassDef).or_else(
                        || crate::isinstance!(top_.as_ref(), Statement, Statement::Class(k) => k),
                    ) {
                        let mut ctx = ctx.clone();
                        ctx.this = Some(top.clone());

                        return Ok(crate::class::Class::new(&ctx, klass).type_id);
                    } else {
                        log::trace!("infer_type: lookup successfull! top={:?}", top.name());
                        let mut ctx = ctx.clone();
                        ctx.this = Some(top.clone());

                        match top.infer_type(&ctx) {
                            Err(err) => ctx.exit_with_error(err),
                            Ok(i) => return Ok(i),
                        }
                    }
                }

                ctx.exit_with_error(MontyError::UndefinedVariable {
                    node: ctx.this.clone().unwrap(),
                });
            }
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck: {:?}", self);

        match self {
            Self::Name(_) => {
                self.infer_type(ctx)?;
            }

            Self::Str(_) => {
                let st_ref = ctx.global_context.register_string_literal(
                    &Spanned {
                        span: ctx.this.clone().unwrap().span().unwrap(),
                        inner: self.clone(),
                    },
                    ctx.module_ref.clone(),
                );

                match ctx.scope.root() {
                    ScopeRoot::Func(f) => f.refs.borrow_mut().push(DataRef::StringConstant(st_ref)),
                    ScopeRoot::AstObject(obj) if crate::isinstance!(obj.as_ref(), Module).is_some() => (),  // `register_string_literal` has already taken care of adding the string to the module's context. 
                    ScopeRoot::AstObject(_) | ScopeRoot::Class(_) => unimplemented!(),
                }
            }

            Self::Tuple(values) => {
                for value in values.iter() {
                    ctx.with(Rc::clone(value), |ctx, value| value.typecheck(&ctx))?;
                }
            }

            Self::Comment(_) | Self::Int(_) => {}

            _ => log::warn!("typecheck:atom Skipping check for self={:?}", self),
        }

        Ok(())
    }
}

impl LookupTarget for Atom {
    fn is_named(&self, target: SpanRef) -> bool {
        matches!(self, Self::Name((n, _)) if *n == target)
    }

    fn name(&self) -> Option<SpanRef> {
        match self {
            Self::Name((n, _)) => Some(n.clone()),
            _ => None,
        }
    }
}
