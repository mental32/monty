use montyc_lexer::Span;

use crate::spanned::Spanned;
use crate::AstVisitor;

use super::atom::Atom;
use super::primary::Primary;
use super::{AstNode, AstObject};

#[derive(Debug, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum UnaryOp {
    Invert,
    Not,
    Add,
    Sub,
}

impl AsRef<str> for UnaryOp {
    fn as_ref(&self) -> &str {
        match self {
            UnaryOp::Invert => "invert",
            UnaryOp::Not => unimplemented!(),
            UnaryOp::Add => "pos",
            UnaryOp::Sub => "neg",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
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
    Xor,
}

impl InfixOp {
    pub fn sigil(&self) -> &str {
        match self {
            InfixOp::Add => "+",
            InfixOp::Sub => "-",
            InfixOp::Power => "^",
            InfixOp::Invert => "~",
            InfixOp::FloorDiv => "//",
            InfixOp::MatMult => "@",
            InfixOp::Mod => "%",
            InfixOp::Div => "/",
            InfixOp::Mult => "*",
            InfixOp::LeftShift => "<<",
            InfixOp::RightShift => ">>",
            InfixOp::NotEq => "!=",
            InfixOp::Eq => "==",
            InfixOp::And => "and",
            InfixOp::Or => "or",
            InfixOp::Xor => "^",
        }
    }
}

impl AsRef<str> for InfixOp {
    fn as_ref(&self) -> &str {
        match self {
            InfixOp::Add => "add",
            InfixOp::Sub => "sub",
            InfixOp::Power => "pow",
            InfixOp::Invert => todo!(),
            InfixOp::FloorDiv => "floordiv",
            InfixOp::MatMult => "matmul",
            InfixOp::Mod => "mod",
            InfixOp::Div => "div",
            InfixOp::Mult => "mul",
            InfixOp::LeftShift => "lshift",
            InfixOp::RightShift => "rshift",
            InfixOp::NotEq => "ne",
            InfixOp::Eq => "eq",
            InfixOp::And => "and",
            InfixOp::Or => "or",
            InfixOp::Xor => "xor",
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Expr {
    If {
        test: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>,
        orelse: Box<Spanned<Expr>>,
    },

    BinOp {
        left: Box<Spanned<Expr>>,
        op: InfixOp,
        right: Box<Spanned<Expr>>,
    },

    Unary {
        op: UnaryOp,
        value: Box<Spanned<Expr>>,
    },

    Named {
        target: Spanned<Atom>,
        value: Box<Spanned<Expr>>,
    },

    Primary(Spanned<Primary>),
}

impl AstObject for Expr {
    fn into_ast_node(&self) -> AstNode {
        match self {
            Expr::If { .. } => AstNode::IfExpr(self.clone()),
            Expr::BinOp { .. } => AstNode::BinOp(self.clone()),
            Expr::Unary { .. } => AstNode::Unary(self.clone()),
            Expr::Named { .. } => AstNode::NamedExpr(self.clone()),
            Expr::Primary(primary) => primary.into_ast_node(),
        }
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        match self {
            Expr::If { .. } => visitor.visit_ternary(self, span),
            Expr::BinOp { .. } => visitor.visit_binop(self, span),
            Expr::Unary { .. } => visitor.visit_unary(self, span),
            Expr::Named { .. } => visitor.visit_named_expr(self, span),
            Expr::Primary(primary) => {
                primary.visit_with(visitor, span.or(Some(primary.span.clone())))
            }
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primary(p), Self::Primary(r)) => p.inner == r.inner,
            (
                Self::If {
                    test: ltest,
                    body: lbody,
                    orelse: lorelse,
                },
                Self::If {
                    test: rtest,
                    body: rbody,
                    orelse: rorelse,
                },
            ) => {
                (ltest.inner == rtest.inner)
                    && (lbody.inner == rbody.inner)
                    && (lorelse.inner == rorelse.inner)
            }

            _ => false,
        }
    }
}
