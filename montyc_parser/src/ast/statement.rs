use super::*;

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    FnDef(FunctionDef),
    Ret(Return),
    Asn(Assign),
    Ann(Annotation),
    Import(Import),
    Class(ClassDef),
    If(IfChain),
    While(While),
    Pass,
}

impl AstObject for Statement {
    fn into_ast_node(&self) -> AstNode {
        match self {
            Self::Pass => AstNode::Pass,
            Self::Expr(node) => node.into_ast_node(),
            Self::FnDef(node) => node.into_ast_node(),
            Self::Ret(node) => node.into_ast_node(),
            Self::Asn(node) => node.into_ast_node(),
            Self::Ann(node) => node.into_ast_node(),
            Self::Import(node) => node.into_ast_node(),
            Self::Class(node) => node.into_ast_node(),
            Self::If(node) => node.into_ast_node(),
            Self::While(node) => node.into_ast_node(),
        }
    }

    fn span(&self) -> Option<Span> {
        None
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        match self {
            Statement::Expr(ref e) => e,
            Statement::FnDef(ref f) => f,
            Statement::Ret(ref r) => r,
            Statement::Asn(ref a) => a,
            Statement::Ann(ref a) => a,
            Statement::Import(ref i) => i,
            Statement::Class(ref c) => c,
            Statement::If(ref i) => i,
            Statement::While(ref w) => w,
            Statement::Pass => self,
        }
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>) -> U
    where
        Self: Sized,
    {
        match self {
            Statement::Expr(inner) => inner.visit_with(visitor),
            Statement::FnDef(inner) => inner.visit_with(visitor),
            Statement::Ret(inner) => inner.visit_with(visitor),
            Statement::Asn(inner) => inner.visit_with(visitor),
            Statement::Ann(inner) => inner.visit_with(visitor),
            Statement::Import(inner) => inner.visit_with(visitor),
            Statement::Class(inner) => inner.visit_with(visitor),
            Statement::If(inner) => inner.visit_with(visitor),
            Statement::While(inner) => inner.visit_with(visitor),
            Statement::Pass => visitor.visit_pass(),
        }
    }
}
