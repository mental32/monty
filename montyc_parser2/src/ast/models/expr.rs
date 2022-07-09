use super::*;

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, Clone)]
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

impl AstObject for Expr {
    fn into_ast_node(&self) -> AstNode {
        todo!()
    }

    fn type_name(&self) -> &str {
        "Expr"
    }

    fn call_visitor_handler<T>(
        &self,
        visitor: &dyn AstVisitor<T>,
        span: Option<montyc_core::Span>,
    ) -> T
    where
        Self: Sized,
    {
        todo!()
    }
}
