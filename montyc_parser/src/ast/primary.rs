use logos::Span;
use montyc_core::SpanRef;

use crate::{spanned::Spanned, AstNode, AstObject, AstVisitor};

use super::{Atom, Expr};

#[derive(Debug, Clone)]
pub enum Primary {
    Atomic(Spanned<Atom>),

    /// `<value:primary>[<index?>]`
    Subscript {
        value: Box<Spanned<Primary>>,
        index: Box<Spanned<Expr>>,
    },

    /// `<func:primary>(<args?>)`
    Call {
        func: Box<Spanned<Primary>>,
        args: Option<Vec<Spanned<Expr>>>,
    },

    /// `<primary> DOT(.) <atom>`
    Attribute {
        left: Box<Spanned<Primary>>,
        attr: Spanned<Atom>,
    },

    /// `(await +)+<primary>`
    Await(Box<Spanned<Primary>>),
}

impl AstObject for Primary {
    fn into_ast_node(&self) -> AstNode {
        match self {
            Primary::Atomic(atom) => atom.into_ast_node(),
            Primary::Subscript { .. } => AstNode::Subscript(self.clone()),
            Primary::Call { .. } => AstNode::Call(self.clone()),
            Primary::Attribute { .. } => AstNode::Attr(self.clone()),
            Primary::Await(_) => todo!(),
        }
    }

    fn span(&self) -> Option<Span> {
        match self {
            Primary::Atomic(s) => s.span(),
            Primary::Subscript { value, index } => Some(value.span()?.start..index.span()?.end),
            Primary::Call { func, args } => args
                .as_ref()
                .and_then(|args| Some(func.span()?.start..args.last()?.span()?.end)),

            Primary::Attribute { left, attr } => Some(left.span()?.start..attr.span()?.end),
            Primary::Await(_) => todo!(),
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
            Primary::Atomic(atom) => atom.visit_with(visitor, span.or(atom.span())),
            Primary::Subscript { .. } => visitor.visit_subscript(self, span.or(self.span())),
            Primary::Call { .. } => visitor.visit_call(self, span.or(self.span())),
            Primary::Attribute { .. } => visitor.visit_attr(self, span.or(self.span())),
            Primary::Await(_) => todo!(),
        }
    }
}

impl Primary {
    pub fn as_name(&self) -> Option<SpanRef> {
        match self {
            Self::Atomic(at) => at.inner.as_name(),
            _ => None,
        }
    }
}

impl PartialEq for Primary {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Atomic(a), Self::Atomic(b)) => a.inner == b.inner,
            _ => false,
        }
    }
}

impl Primary {
    /// break a dotted name down into its named components
    pub fn components(&self) -> Vec<Atom> {
        let mut names = vec![];

        match self {
            Primary::Atomic(atom) => {
                names.push(atom.inner.clone());
            }

            Primary::Attribute { left, attr } => {
                names.extend(left.inner.components());
                names.push(attr.inner.clone());
            }

            _ => unreachable!(),
        }

        names
    }

    pub fn is_comment(&self) -> bool {
        matches!(
            self,
            Self::Atomic(atom) if matches!(atom, Spanned {
                inner: Atom::Comment(_),
                ..
            })
        )
    }
}
