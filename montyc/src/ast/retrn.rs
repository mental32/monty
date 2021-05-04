use std::rc::Rc;

use crate::{parser::comb::return_unspanned, prelude::*, scope::ScopeRoot};

use super::{expr::Expr, funcdef::FunctionDef, stmt::Statement, AstObject, Spanned};

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Rc<Spanned<Expr>>>,
}

impl AstObject for Return {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone()) as Rc<_>
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        let v: Rc<dyn AstObject> = Rc::new(self.value.as_ref()?.clone());

        Some(Box::new(vec![v].into_iter()))
    }
}

impl TypedObject for Return {
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(TypeMap::NEVER)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:return {:?}", self);

        let expected = match ctx.scope.root() {
            ScopeRoot::AstObject(object) => {
                match object.unspanned().as_ref().downcast_ref::<FunctionDef>() {
                    Some(func) => {
                        let kind: FunctionType = (func, ctx).into();
                        kind.ret
                    }

                    None => unreachable!("{:?}", ctx.scope.root()),
                }
            }

            ScopeRoot::Func(func) => func.kind.inner.ret,
            ScopeRoot::Class(_) => panic!("return in a class def"),
        };

        let actual = match &self.value {
            Some(value) => {
                let mut ctx = ctx.clone();
                ctx.this = Some(value.clone() as Rc<_>);
                value.infer_type(&ctx)?
            }

            None => TypeMap::NONE_TYPE,
        };

        if !ctx.global_context.type_map.type_eq(expected, actual) {
            let ret_node = ctx
                .this
                .as_ref()
                .and_then(|n| n.as_ref().downcast_ref::<Spanned<Statement>>().cloned())
                .and_then(|n| {
                    Some(n.map(|st| match st {
                        Statement::Ret(r) => r,
                        _ => unreachable!(),
                    }))
                })
                .map(|ret| Rc::new(ret))
                .unwrap();

            let def_node = match ctx.scope.root() {
                ScopeRoot::Func(f) => {
                    let fndef = f
                        .def(ctx.global_context)
                        .unwrap()
                        .as_ref()
                        .as_function()
                        .cloned()
                        .unwrap();

                    let fndef = Spanned {
                        span: fndef.name.span.start
                            ..fndef
                                .returns
                                .clone()
                                .unwrap()
                                .span()
                                .unwrap_or(fndef.name.span.clone())
                                .end,
                        inner: fndef.clone(),
                    };

                    Rc::new(fndef)
                }

                ScopeRoot::AstObject(object) => {
                    let fndef = object.as_ref().downcast_ref::<FunctionDef>().unwrap();
                    let fndef = Spanned {
                        span: fndef.name.span.start
                            ..fndef
                                .returns
                                .clone()
                                .unwrap()
                                .span()
                                .unwrap_or(fndef.name.span.clone())
                                .end,
                        inner: fndef.clone(),
                    };

                    Rc::new(fndef)
                }
                _ => panic!("{:?}", ctx.scope.root()),
            };

            ctx.exit_with_error(MontyError::BadReturnType {
                expected,
                actual,
                ret_node,
                def_node,
            })
        }

        Ok(())
    }
}

impl Parseable for Return {
    const PARSER: ParserT<Self> = return_unspanned;
}

impl LookupTarget for Return {
    fn is_named(&self, _target: crate::parser::SpanEntry) -> bool {
        false
    }

    fn name(&self) -> crate::parser::SpanEntry {
        None
    }
}
