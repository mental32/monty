use std::rc::Rc;

use crate::prelude::*;

use super::{atom::Atom, expr::Expr, AstObject, ObjectIter, Spanned};

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Spanned<Atom>,
    pub value: Rc<Spanned<Expr>>,
    pub kind: Option<Rc<Spanned<Atom>>>,
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
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        unreachable!()
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        let expected = match self.kind.as_ref() {
            Some(at) => {
                let mut ctx = ctx.clone();
                ctx.this = Some(Rc::clone(at) as Rc<_>);
                Some(at.infer_type(&ctx)?)
            },

            None => None,
        };

        let actual = {
            let mut ctx = ctx.clone();
            ctx.this = Some(Rc::new(self.value.clone()) as Rc<dyn AstObject>);
            self.value.infer_type(&ctx)?
        };

        let apparent = if let Some(expected) = expected {
            if !ctx.global_context.type_map.type_eq(expected, actual) {
                ctx.exit_with_error(MontyError::IncompatibleTypes {
                    left_span: self.name.span.clone(),
                    left: expected,
                    right_span: self.value.span.clone(),
                    right: actual,
                });
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
                if let Some((ty, span)) = func.vars.get(name).map(|kv| kv.value().clone()) {
                    if !ctx.global_context.type_map.type_eq(ty, actual) {
                        ctx.exit_with_error(MontyError::IncompatibleReassignment {
                            name: name.clone(),
                            first_assigned: span.clone(),
                            incorrectly_reassigned: ctx.this.clone().unwrap().span().unwrap(),
                            expected: ty,
                            actual,
                        })
                    }
                } else {
                    log::trace!("typecheck:assign setting var {:?} = {:?}", name, apparent);
                    func.vars.insert(
                        name.clone(),
                        (apparent, ctx.this.clone().unwrap().span().unwrap()),
                    );
                }
            }
        } else {
            log::warn!(
                "typecheck:assign unbound assignment in non-function scope {:?}",
                self
            );
        }

        Ok(())
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
