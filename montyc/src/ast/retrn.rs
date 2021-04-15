use std::rc::Rc;

use codespan_reporting::term::termcolor::{Color, ColorSpec, WriteColor};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{termcolor::StandardStream, ColorArg},
};

use nom::IResult;

use crate::{
    context::LocalContext,
    func::Function,
    parser::{comb::return_stmt, Parseable, ParserT, TokenSlice},
    scope::{downcast_ref, LookupTarget, Scope, ScopeRoot},
    typing::{LocalTypeId, TypeMap, TypedObject},
    MontyError,
};

use super::{AstObject, Spanned, expr::Expr, funcdef::FunctionDef, stmt::Statement};

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Spanned<Expr>>,
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId> {
        None
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        let expected = match ctx.scope.root() {
            ScopeRoot::AstObject(object) => {
                match downcast_ref::<FunctionDef>(object.unspanned().as_ref()) {
                    Some(func) => func.infer_type(&ctx).expect("oof"),
                    None => return unreachable!("{:?}", ctx.scope.root()),
                }
            }

            ScopeRoot::Func(func) => func.kind.inner.ret,
            ScopeRoot::Class(_) => return panic!("return in a class def"),
        };

        let actual = match &self.value {
            Some(value) => value.infer_type(&ctx).expect("failed to infer type for return value."),
            None => TypeMap::NONE_TYPE,
        };

        let type_map = ctx.global_context.type_map.borrow();

        if expected != actual {
            let ret_node = ctx
                .this
                .as_ref()
                .and_then(|n| downcast_ref::<Spanned<Statement>>(n.as_ref()).cloned())
                .and_then(|n| {
                    Some(n.map(|st| match st {
                        Statement::Ret(r) => r,
                        _ => unreachable!(),
                    }))
                })
                .map(|ret| Rc::new(ret))
                .unwrap();

            let def_node = match ctx.scope.root() {
                ScopeRoot::Func(f) => f.def.clone(),
                ScopeRoot::AstObject(object) => {
                    let fndef = downcast_ref::<FunctionDef>(object.as_ref()).unwrap();
                    let fndef = Spanned {
                        span: fndef.name.span.start..fndef.returns.span().unwrap_or(fndef.name.span.clone()).end,
                        inner: fndef.clone(),
                    };

                    Rc::new(fndef)
                }, 
                _ => panic!("{:?}", ctx.scope.root()),
            };

            ctx.error(MontyError::BadReturnType {
                expected,
                actual,
                ret_node,
                def_node,
            })
        }
    }
}

#[inline]
pub fn return_unspanned<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Return> {
    let (stream, Spanned { inner, .. }) = return_stmt(stream)?;

    Ok((stream, inner))
}

impl Parseable for Return {
    const PARSER: ParserT<Self> = return_unspanned;
}

impl LookupTarget for Return {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        false
    }

    fn name(&self) -> crate::parser::SpanEntry {
        todo!()
    }
}
