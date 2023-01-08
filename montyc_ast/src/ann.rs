use crate::atom::Atom;
use crate::expr::Expr;
use crate::funcdef::FunctionDefParam;
use crate::spanned::Spanned;
use crate::AstObject;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Annotation {
    pub name: Spanned<Atom>,
    pub annotation: Spanned<Expr>,
}

impl From<Spanned<FunctionDefParam>> for Annotation {
    fn from(param: Spanned<FunctionDefParam>) -> Self {
        Self {
            name: Spanned {
                span: param.span,
                inner: Atom::Name(param.inner.named),
            },
            annotation: param.inner.annotation.unwrap(),
        }
    }
}

impl AstObject for Annotation {
    fn into_ast_node(&self) -> crate::AstNode {
        crate::AstNode::Annotation(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(
        &self,
        visitor: &mut dyn crate::AstVisitor<U>,
        span: Option<montyc_lexer::Span>,
    ) -> U
    where
        Self: Sized,
    {
        visitor.visit_annotation(self, span)
    }
}
