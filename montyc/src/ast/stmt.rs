use std::rc::Rc;

use nom::IResult;

use crate::{
    context::LocalContext,
    parser::{comb::stmt::statement, token::PyToken, Parseable, ParserT, TokenSlice},
    scope::{downcast_ref, LookupTarget},
    typing::{TypeMap, TypedObject},
};

use super::{
    assign::Assign, class::ClassDef, expr::Expr, funcdef::FunctionDef, import::Import,
    retrn::Return, AstObject, Spanned,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expr),
    FnDef(FunctionDef),
    Ret(Return),
    Asn(Assign),
    Import(Import),
    Class(ClassDef),
    SpanRef(PyToken),
    Pass,
}

impl AstObject for Statement {
    fn span(&self) -> Option<logos::Span> {
        match self {
            Statement::Expression(e) => e.span(),
            Statement::FnDef(f) => f.span(),
            Statement::Ret(r) => r.span(),
            Statement::Asn(a) => a.span(),
            Statement::Import(i) => i.span(),
            Statement::Class(c) => c.span(),
            Statement::SpanRef(s) => s.span(),
            Statement::Pass => None,
        }
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
            Statement::Import(i) => i.walk(),
            Statement::Class(c) => c.walk(),
            Statement::SpanRef(s) => Some(Box::new(std::iter::once(Rc::new(s.clone()) as Rc<_>))),
            Statement::Pass => None,
        }
    }
}

impl TypedObject for Statement {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<crate::typing::LocalTypeId> {
        match self {
            Statement::Expression(e) => e.infer_type(ctx),
            Statement::FnDef(f) => f.infer_type(ctx),
            Statement::Ret(r) => r.infer_type(ctx),
            Statement::Asn(a) => a.infer_type(ctx),
            Statement::Import(i) => i.infer_type(ctx),
            Statement::Class(c) => c.infer_type(ctx),
            Statement::SpanRef(_) => {
                todo!()
            }
            Statement::Pass => Some(TypeMap::NONE_TYPE),
        }
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        match self {
            Statement::Expression(e) => e.typecheck(ctx),
            Statement::FnDef(f) => f.typecheck(ctx),
            Statement::Ret(r) => r.typecheck(ctx),
            Statement::Asn(a) => a.typecheck(ctx),
            Statement::Import(i) => i.typecheck(ctx),
            Statement::Class(c) => c.typecheck(ctx),
            Statement::SpanRef(_) => {}
            Statement::Pass => {}
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

impl LookupTarget for Statement {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        match self {
            Statement::Expression(e) => e.is_named(target),
            Statement::FnDef(e) => e.is_named(target),
            Statement::Ret(e) => e.is_named(target),
            Statement::Asn(e) => e.is_named(target),
            Statement::Import(e) => e.is_named(target),
            Statement::Class(e) => e.is_named(target),
            Statement::SpanRef(e) => e.is_named(target),
            Statement::Pass => false,
        }
    }

    fn name(&self) -> crate::parser::SpanEntry {
        match self {
            Statement::Expression(e) => e.name(),
            Statement::FnDef(e) => e.name(),
            Statement::Ret(e) => e.name(),
            Statement::Asn(e) => e.name(),
            Statement::Import(e) => e.name(),
            Statement::Class(e) => e.name(),
            Statement::SpanRef(e) => e.name(),
            Statement::Pass => None,
        }
    }
}
