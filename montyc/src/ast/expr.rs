use std::rc::Rc;

use crate::{
    context::LocalContext,
    parser::{Parseable, ParserT, Span},
    typing::{TypeMap, TypedObject},
};

use super::{atom::Atom, primary::Primary, AstObject, ObjectIter, Spanned};

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Invert,
    Not,
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOp {
    Add,
    Sub,
    Power,
    Invert,
    FloorDiv,
    MatMult,
    Mod,
    Div,
    Mult,
    LeftShift,
    RightShift,
    NotEq,
    Eq,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    If {
        test: Rc<Spanned<Expr>>,
        body: Rc<Spanned<Expr>>,
        orelse: Rc<Spanned<Expr>>,
    },

    BinOp {
        left: Rc<Spanned<Expr>>,
        op: InfixOp,
        right: Rc<Spanned<Expr>>,
    },

    Unary {
        op: UnaryOp,
        value: Rc<Spanned<Expr>>,
    },

    Named {
        target: Spanned<Atom>,
        value: Rc<Spanned<Expr>>,
    },

    Primary(Primary),
}

impl Parseable for Expr {
    const PARSER: ParserT<Self> = crate::parser::comb::expression_unspanned;
}

impl AstObject for Expr {
    fn span(&self) -> Option<Span> {
        None
    }

    fn walk<'a>(&'a self) -> Option<ObjectIter> {
        let it = match &self {
            Expr::If { test, body, orelse } => vec![
                test.clone() as Rc<dyn AstObject>,
                body.clone() as Rc<dyn AstObject>,
                orelse.clone() as Rc<dyn AstObject>,
            ],

            Expr::BinOp { left, op: _, right } => vec![
                left.clone() as Rc<dyn AstObject>,
                right.clone() as Rc<dyn AstObject>,
            ],

            Expr::Unary { op: _, value } => vec![value.clone() as Rc<dyn AstObject>],

            Expr::Named { target, value } => vec![
                Rc::new(target.clone()) as Rc<dyn AstObject>,
                value.clone() as Rc<dyn AstObject>,
            ],

            Expr::Primary(primary) => return primary.walk(),
        };

        Some(Box::new(it.into_iter()))
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }
}

impl TypedObject for Expr {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<crate::typing::LocalTypeId> {
        match self {
            Expr::If {
                test: _,
                body: _,
                orelse: _,
            } => None,

            Expr::BinOp {
                left: _,
                op: _,
                right: _,
            } => None,

            Expr::Unary { op: _, value: _ } => None,

            Expr::Named {
                target: _,
                value: _,
            } => None,

            Expr::Primary(p) => p.infer_type(ctx),
        }
    }

    fn typecheck<'a>(&self, ctx: LocalContext<'a>) {
        match self {
            Expr::If { test, body, orelse } => {}
            Expr::BinOp { left, op, right } => {}
            Expr::Unary { op, value } => {}
            Expr::Named { target, value } => {}
            Expr::Primary(p) => p.typecheck(ctx),
        }
    }
}
