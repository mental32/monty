use std::rc::Rc;

use nom::IResult;

use crate::{context::LocalContext, parser::{Parseable, ParserT, TokenSlice, comb::stmt::statement}, scope::downcast_ref, typing::TypedObject};

use super::{AstObject, Spanned, assign::Assign, expr::Expr, funcdef::FunctionDef, retrn::Return};


#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expr),
    FnDef(FunctionDef),
    Ret(Return),
    Asn(Assign),
}

impl AstObject for Statement {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> std::rc::Rc<dyn AstObject> {
        todo!()
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        match self {
            Statement::Expression(e) => e.walk(),
            Statement::FnDef(f) => f.walk(),
            Statement::Ret(r) => r.walk(),
            Statement::Asn(a) => a.walk(),
        }
    }
}

impl TypedObject for Statement {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<crate::typing::LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        match self {
            Statement::Expression(e) => e.typecheck(ctx),
            Statement::FnDef(f) => f.typecheck(ctx),
            Statement::Ret(r) => r.typecheck(ctx),
            Statement::Asn(a) => a.typecheck(ctx),
        }
    }
}

#[inline]
pub fn statement_unspanned<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Statement> {
    let (stream, Spanned { inner, .. }) = statement(stream)?;

    Ok((stream, inner))
}

impl Parseable for Statement {
    const PARSER: ParserT<Self> = statement_unspanned;
}
