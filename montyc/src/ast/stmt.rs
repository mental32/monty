use std::{rc::Rc};

use super::{
    assign::Assign, class::ClassDef, expr::Expr, funcdef::FunctionDef, ifelif::IfChain,
    import::Import, retrn::Return, while_::While, AstObject,
};

use crate::{parser::{SpanRef, comb::stmt::statement_unspanned}, prelude::*};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expr),
    FnDef(FunctionDef),
    Ret(Return),
    Asn(Assign),
    Import(Import),
    Class(ClassDef),
    If(IfChain),
    While(While),
    Pass,
}

impl Statement {
    pub fn inner_as_object(&self) -> Rc<dyn AstObject> {
        match self.clone() {
            Statement::Expression(e) => Rc::new(e),
            Statement::FnDef(f) => Rc::new(f),
            Statement::Ret(r) => Rc::new(r),
            Statement::Asn(a) => Rc::new(a),
            Statement::Import(i) => Rc::new(i),
            Statement::Class(c) => Rc::new(c),
            Statement::If(f) => Rc::new(f),
            Statement::While(w) => Rc::new(w),
            Statement::Pass => todo!(),
        }
    }
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
            Statement::If(f) => f.span(),
            Statement::While(w) => w.span(),
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
            Statement::If(f) => f.unspanned(),
            Statement::While(w) => w.unspanned(),
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
            Statement::If(f) => f.walk(),
            Statement::While(w) => w.walk(),
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
            Statement::While(w) => w.infer_type(ctx),
            Statement::If(_) => Ok(TypeMap::NONE_TYPE),
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
            Statement::If(f) => f.typecheck(ctx),
            Statement::While(w) => w.typecheck(ctx),
            Statement::Pass => Ok(()),
        }
    }
}

impl Parseable for Statement {
    const PARSER: ParserT<Self> = statement_unspanned;
}

impl LookupTarget for Statement {
    fn is_named(&self, target: SpanRef) -> bool {
        match self {
            Statement::Expression(e) => e.is_named(target),
            Statement::FnDef(e) => e.is_named(target),
            Statement::Ret(e) => e.is_named(target),
            Statement::Asn(e) => e.is_named(target),
            Statement::Import(e) => e.is_named(target),
            Statement::Class(e) => e.is_named(target),
            Statement::If(f) => f.is_named(target),
            Statement::While(w) => w.is_named(target),
            Statement::Pass => false,
        }
    }

    fn name(&self) -> Option<SpanRef> {
        match self {
            Statement::Expression(e) => e.name(),
            Statement::FnDef(e) => e.name(),
            Statement::Ret(e) => e.name(),
            Statement::Asn(e) => e.name(),
            Statement::Import(e) => e.name(),
            Statement::Class(e) => e.name(),
            Statement::If(f) => f.name(),
            Statement::While(w) => w.name(),
            Statement::Pass => None,
        }
    }
}
