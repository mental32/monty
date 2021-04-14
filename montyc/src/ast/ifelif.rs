use super::{expr::Expr, Spanned};

#[derive(Debug)]
pub enum BranchTail {
    Else(Box<Spanned<Expr>>),
    If(Box<If>),
}

#[derive(Debug)]
pub struct If {
    pub test: Box<Spanned<Expr>>,
    pub body: Vec<Spanned<Expr>>,
    pub orelse: Option<BranchTail>,
}
