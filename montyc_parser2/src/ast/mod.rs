//! The collection of various AST nodes and trait definitions.

mod models;
mod spanned;

pub use models::*;
pub use spanned::Spanned;

/// [AstObject] is used to represent type-erased AST nodes.
pub trait AstObject {
    fn into_ast_node(&self) -> AstNode;

    /// The name of the type implementing the trait.
    fn type_name(&self) -> &str;

    fn spanned(self, span: montyc_core::Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned::new(self, span)
    }

    /// The span of the current object, None if no span information is available.
    fn span(&self) -> Option<montyc_core::Span> {
        None
    }

    /// Given a `visitor` have this object call the correct handler for it.
    fn call_visitor_handler<T>(
        &self,
        visitor: &dyn AstVisitor<T>,
        span: Option<montyc_core::Span>,
    ) -> T
    where
        Self: Sized;
}

pub trait AstObjectExt {
    fn into_ast_node(&self) -> AstNode;
}

macro_rules! impl_ast_node {
    ( $( ( $variant:ident => $payload:ty ) ),+ ) => {
        #[derive(Debug, Clone)]
        pub enum AstNode {
            $(
                $variant($payload)
            ),+
        }

        impl AstNode {
            pub fn as_inner(&self) -> &dyn AstObject {
                match self {
                    $(
                        Self::$variant(inner) => inner
                    ),+
                }
            }
        }

        impl From<AstNode> for Box<dyn AstObject> {
            fn from(node: AstNode) -> Self {
                match node {
                    $(
                        AstNode::$variant(inner) => Box::new(inner)
                    ),+
                }
            }
        }

        #[allow(non_snake_case)]
        pub trait AstVisitor<T = ()> {
            fn visit(&self, root: impl AstObject) -> T where Self: Sized{
                root.call_visitor_handler(self as &dyn AstVisitor<T>, root.span())
            }

            $(
                paste::paste! {
                    #[track_caller]
                    fn [<visit_ $variant>] (&self, node: &$payload, _span: Option<::montyc_core::Span>) -> T {
                        unimplemented!("visitor handler is unimplemented {:?}", node.type_name());
                    }
                }
            )+
        }
    };
}

impl_ast_node!(
    (Import => models::Import),
    (ClassDef => models::ClassDef),
    (FuncDef => models::FunctionDef),
    (If => models::IfChain),
    (Assign => models::Assign),
    (Int => models::Atom),
    (Str => models::Atom),
    (Comment => models::Atom),
    (Bool => models::Atom),
    (Float => models::Atom),
    (Tuple => models::Atom),
    (Name => models::Atom),
    (BinOp => models::Expr),
    (IfExpr => models::Expr),
    (Unary => models::Expr),
    (NamedExpr => models::Expr),
    (None => models::Atom),
    (Ellipsis => models::Atom),
    (Subscript => models::Primary),
    (Call => models::Primary),
    (Attr => models::Primary),
    (Ret => models::Return),
    (While => models::While)
);
