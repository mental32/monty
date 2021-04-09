use std::rc::Rc;

use nom::IResult;

use crate::{context::LocalContext, func::Function, parser::{Parseable, ParserT, TokenSlice, comb::return_stmt}, scope::{downcast_ref, Scope, ScopeRoot}, typing::{LocalTypeId, TypeMap, TypedObject}};

use super::{expr::Expr, AstObject, Spanned};

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
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>,
    ) -> Option<LocalTypeId> {
        None
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        let expected = match ctx.scope.root() {
            ScopeRoot::AstObject(object) => {
                match downcast_ref::<Function>(object.unspanned().as_ref()) {
                    Some(func) => func.kind.inner.ret,
                    None => return unreachable!("{:?}", ctx.scope.root()),
                }
            }

            ScopeRoot::Func(func) => func.kind.inner.ret,
        };

        let actual = match &self.value {
            Some(value) => value.infer_type(&ctx).unwrap(),
            None => TypeMap::NONE_TYPE,
        };

        let type_map = ctx.global_context.type_map.borrow();

        assert_eq!(
            expected,
            actual,
            "{:?} != {:?}",
            type_map.get(expected),
            type_map.get(actual)
        );
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
