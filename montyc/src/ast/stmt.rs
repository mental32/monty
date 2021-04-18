use std::rc::Rc;

use crate::{parser::comb::stmt::statement_unspanned, prelude::*};
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
            Statement::Pass => None,
        }
    }

    fn unspanned(&self) -> std::rc::Rc<dyn AstObject> {
        match self {
            Statement::Expression(e) => e.unspanned(),
            Statement::FnDef(f) => f.unspanned(),
            Statement::Ret(r) => r.unspanned(),
            Statement::Asn(a) => a.unspanned(),
            Statement::Import(i) => i.unspanned(),
            Statement::Class(c) => c.unspanned(),
            Statement::Pass => Rc::new(Self::Pass),
        }
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        match self {
            Statement::Expression(e) => e.walk(),
            Statement::FnDef(f) => f.walk(),
            Statement::Ret(r) => r.walk(),
            Statement::Asn(a) => a.walk(),
            Statement::Import(i) => i.walk(),
            Statement::Class(c) => c.walk(),
            Statement::Pass => None,
        }
    }
}

impl TypedObject for Statement {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        match self {
            Statement::Expression(e) => e.infer_type(ctx),
            Statement::FnDef(f) => f.infer_type(ctx),
            Statement::Ret(r) => r.infer_type(ctx),
            Statement::Asn(a) => a.infer_type(ctx),
            Statement::Import(i) => i.infer_type(ctx),
            Statement::Class(c) => c.infer_type(ctx),
            Statement::Pass => Ok(TypeMap::NONE_TYPE),
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        match self {
            Statement::Expression(e) => e.typecheck(ctx),
            Statement::FnDef(f) => f.typecheck(ctx),
            Statement::Ret(r) => r.typecheck(ctx),
            Statement::Asn(a) => a.typecheck(ctx),
            Statement::Import(i) => i.typecheck(ctx),
            Statement::Class(c) => c.typecheck(ctx),
            Statement::Pass => Ok(())
        }
    }
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
            Statement::Pass => None,
        }
    }
}
