use std::rc::Rc;

use crate::prelude::*;

use super::{atom::Atom, expr::Expr, AstObject, ObjectIter, Spanned};

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Spanned<Atom>,
    pub value: Rc<Spanned<Expr>>,
    pub kind: Option<Rc<Spanned<Expr>>>,
}

impl Assign {
    pub fn expected(&self, ctx: &LocalContext<'_>) -> crate::Result<Option<(LocalTypeId, bool)>> {
        match self.kind.as_ref() {
            Some(at) => {
                let ty = match crate::utils::try_parse_union_literal(ctx, &at.inner, false)? {
                    Some(tys) => (ctx.global_context.type_map.tagged_union(tys), true),
                    None => {
                        let mut ctx = ctx.clone();
                        ctx.this = Some(Rc::clone(at) as Rc<_>);
                        (at.infer_type(&ctx)?, false)
                    }
                };

                ctx.cache_type(&(Rc::clone(at) as Rc<dyn AstObject>), ty.0);

                Ok(Some(ty))
            }

            None => Ok(None),
        }
    }
}

impl Parseable for Assign {
    const PARSER: ParserT<Self> = crate::parser::comb::assign::assignment_unspanned;
}

impl AstObject for Assign {
    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }

    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn walk<'a>(&'a self) -> Option<ObjectIter> {
        let it = vec![Rc::new(self.value.clone()) as Rc<dyn AstObject>];

        Some(Box::new(it.into_iter()))
    }
}

impl TypedObject for Assign {
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        unreachable!()
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:assign {:?}", self);

        let expected = self.expected(ctx)?;

        let actual = {
            let mut ctx = ctx.clone();
            ctx.this = Some(Rc::new(self.value.clone()) as Rc<dyn AstObject>);
            self.value.infer_type(&ctx)?
        };

        let apparent = if let Some((expected, is_union)) = expected {
            if is_union {
                let tunion = ctx
                    .global_context
                    .type_map
                    .get_tagged::<Generic>(expected)
                    .unwrap()
                    .unwrap();

                let inner = match tunion.inner {
                    Generic::Union { inner } => inner,
                    Generic::Struct { inner } => match ctx
                        .global_context
                        .type_map
                        .get_tagged::<Generic>(inner[1])
                        .unwrap()
                        .unwrap()
                        .inner
                    {
                        Generic::Union { inner } => inner,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                let is_ok = inner
                    .iter()
                    .any(|variant| ctx.global_context.type_map.type_eq(actual, *variant));

                if !is_ok {
                    ctx.exit_with_error(MontyError::IncompatibleTypes {
                        left_span: self.name.span.clone(),
                        left: expected,
                        right_span: self.value.span.clone(),
                        right: actual,
                    });
                }
            } else {
                if !ctx.global_context.type_map.type_eq(actual, expected) {
                    ctx.exit_with_error(MontyError::IncompatibleTypes {
                        left_span: self.name.span.clone(),
                        left: expected,
                        right_span: self.value.span.clone(),
                        right: actual,
                    });
                }
            }

            if expected != actual {
                expected
            } else {
                actual
            }
        } else {
            actual
        };

        if let ScopeRoot::Func(func) = ctx.scope.root() {
            if let Atom::Name(name) = &self.name.inner {
                if let Some((ty, span, _)) = func.vars.get(name).map(|kv| kv.value().clone()) {
                    if !ctx.global_context.type_map.type_eq(ty, actual)
                        && !ctx
                            .global_context
                            .type_map
                            .is_variant_of_tagged_union(actual, ty)
                    {
                        ctx.exit_with_error(MontyError::IncompatibleReassignment {
                            name: name.1.clone(),
                            first_assigned: span.clone(),
                            incorrectly_reassigned: ctx.this.clone().unwrap().span().unwrap(),
                            expected: ty,
                            actual,
                        })
                    }
                } else if !func.vars.contains_key(name) {
                    log::trace!("typecheck:assign setting var {:?} = {:?}", name, apparent);
                    func.vars.insert(
                        name.clone(),
                        (
                            apparent.canonicalize(&ctx.global_context.type_map),
                            ctx.this.clone().unwrap().span().unwrap(),
                            crate::func::VarType::Local,
                        ),
                    );
                }
            }
        } else {
            log::trace!(
                "typecheck:assign assignment in non-function scope being treated as global {:?}",
                self
            );

            let mctx = ctx.global_context.modules.get(&ctx.module_ref).unwrap();

            mctx.globals
                .insert(self.name.name().unwrap(), Rc::clone(&self.value));
        }

        ctx.with(Rc::clone(&self.value), |ctx, this| this.typecheck(&ctx))
    }
}

impl LookupTarget for Assign {
    fn is_named(&self, target: SpanRef) -> bool {
        self.name.is_named(target)
    }

    fn name(&self) -> Option<SpanRef> {
        self.name.name()
    }
}
