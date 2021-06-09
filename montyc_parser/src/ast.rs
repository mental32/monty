//! The collection of various AST nodes.

use logos::Span;

use crate::spanned::Spanned;

/// An explicit enumeration of all AST nodes.
#[derive(Debug)]
pub enum AstNode {}

impl From<AstNode> for Box<dyn AstObject> {
    fn from(node: AstNode) -> Self {
        match node {}
    }
}

/// An opaque representation of an AST node.
pub trait AstObject {
    /// A copy of `Self` wrapped into an `AstNode` variant.
    fn into_ast_node(&self) -> AstNode;

    /// The span of the AST object, if available.
    fn span(&self) -> Option<Span>;

    /// The inner unspanned AST object.
    fn unspanned<'a>(&'a self) -> &'a dyn AstObject;

    /// Apply the visitor to the AST objects fields/sub-nodes.
    fn visit_with(&self, visitor: &mut dyn AstVisitor);
}

impl<T> AstObject for Spanned<T>
where
    T: AstObject,
{
    fn into_ast_node(&self) -> AstNode {
        self.inner.into_ast_node()
    }

    fn span(&self) -> Option<Span> {
        Some(self.span.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        &self.inner
    }

    fn visit_with(&self, visitor: &mut dyn AstVisitor) {
        self.inner.visit_with(visitor)
    }
}

/// A visitor trait used for walking an AST object.
pub trait AstVisitor {
    fn visit_funcdef(&mut self, f: &FunctionDef) {}

    fn visit_atom(&mut self, a: &Atom) {}

    fn visit_primary(&mut self, p: &Primary) {}

    fn visit_statement(&mut self, s: &Statement) {}

    fn visit_expr(&mut self, e: &Expr) {}

    fn visit_while(&mut self, w: &While) {}
}

pub use models::*;

pub mod models {
    use std::convert::TryFrom;

    use montyc_core::SpanRef;

    use crate::token::PyToken;

    use super::*;

    #[derive(Debug, Clone)]
    pub struct FunctionDef {
        pub reciever: Option<Spanned<Atom>>,
        pub name: Spanned<Atom>,
        pub args: Option<Vec<(SpanRef, Option<Spanned<Expr>>)>>,
        pub body: Vec<Spanned<Statement>>,
        pub decorator_list: Vec<Spanned<Primary>>,
        pub returns: Option<Spanned<Expr>>,
        // type_comment: Option<Expr>,
    }

    impl AstObject for FunctionDef {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    impl FunctionDef {
        pub fn is_dynamically_typed(&self) -> bool {
            self.args
                .as_ref()
                .map(|args| args.iter().any(|arg| arg.1.is_none()))
                .unwrap_or(false)
        }
    }

    #[derive(Debug, Clone)]
    pub struct While {
        pub test: Spanned<Expr>,
        pub body: Vec<Spanned<Statement>>,
    }

    impl AstObject for While {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Return {
        pub value: Option<Spanned<Expr>>,
    }

    impl AstObject for Return {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    #[derive(Debug, Clone)]
    pub struct ClassDef {
        pub name: Spanned<Atom>,
        pub decorator_list: Vec<Spanned<Primary>>,
        pub body: Vec<Spanned<Statement>>,
    }

    impl AstObject for ClassDef {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Assign {
        pub name: Spanned<Primary>,
        pub value: Spanned<Expr>,
        pub kind: Option<Spanned<Expr>>,
    }

    impl AstObject for Assign {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Annotation {
        pub name: Spanned<Atom>,
        pub kind: Spanned<Expr>,
    }

    impl AstObject for Annotation {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    #[derive(Debug, Clone)]
    pub enum BranchTail {
        Else(Spanned<Statement>),
        If(Spanned<If>),
    }

    #[derive(Debug, Clone)]
    pub struct If {
        pub test: Spanned<Expr>,
        pub body: Vec<Spanned<Statement>>,
    }

    #[derive(Debug, Clone)]
    pub struct IfChain {
        pub branches: Vec<Spanned<If>>,
        pub orelse: Option<Vec<Spanned<Statement>>>,
    }

    impl AstObject for IfChain {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

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
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
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

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
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
                Statement::Pass => todo!(),
            }
        }
    }

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

    #[derive(Debug, Clone)]
    pub struct Module {
        pub body: Vec<Spanned<Statement>>,
    }

    #[derive(Debug, Clone)]
    pub struct ImportDecl {
        pub parent: Import,
        pub name: Spanned<Primary>,
        pub alias: Option<SpanRef>,
    }

    #[derive(Debug, Clone)]
    pub enum Import {
        Names(Vec<Spanned<Primary>>),
        From {
            module: Spanned<Primary>,
            names: Vec<Spanned<Primary>>,
            level: usize,
        },
    }

    impl AstObject for Import {
        fn into_ast_node(&self) -> AstNode {
            todo!()
        }

        fn span(&self) -> Option<Span> {
            todo!()
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            todo!()
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            todo!()
        }
    }

    impl Import {
        pub fn decls(self: Self) -> Vec<ImportDecl> {
            let decls: Vec<ImportDecl> = match self.clone() {
                Import::Names(names) => names
                    .iter()
                    .map(|target| ImportDecl {
                        parent: self.clone(),
                        name: target.clone(),
                        alias: None,
                    })
                    .collect(),

                Import::From {
                    module: _,
                    names,
                    level: _,
                } => names
                    .iter()
                    .map(|target| ImportDecl {
                        parent: self.clone(),
                        name: target.clone(),
                        alias: None,
                    })
                    .collect(),
            };

            decls
        }
    }

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

    impl AstObject for Expr {
        fn into_ast_node(&self) -> AstNode {
            match self {
                Expr::If { test, body, orelse } => todo!(),
                Expr::BinOp { left, op, right } => todo!(),
                Expr::Unary { op, value } => todo!(),
                Expr::Named { target, value } => todo!(),
                Expr::Primary(_) => todo!(),
            }
        }

        fn span(&self) -> Option<Span> {
            None
        }

        fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
            self
        }

        fn visit_with(&self, visitor: &mut dyn AstVisitor) {
            match self {
                Expr::If { test, body, orelse } => todo!(),
                Expr::BinOp { left, op, right } => todo!(),
                Expr::Unary { op, value } => todo!(),
                Expr::Named { target, value } => todo!(),
                Expr::Primary(_) => todo!(),
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct StringRef(pub SpanRef);

    impl TryFrom<Atom> for StringRef {
        type Error = Atom;

        fn try_from(value: Atom) -> Result<Self, Self::Error> {
            match value {
                Atom::Str(n) => Ok(Self(n)),
                _ => Err(value),
            }
        }
    }

    impl From<StringRef> for SpanRef {
        fn from(st: StringRef) -> Self {
            st.0
        }
    }

    #[derive(Debug, Clone)]
    pub enum Atom {
        None,
        Ellipsis,
        Int(isize),
        Str(SpanRef),
        Bool(bool),
        Float(f64),
        Tuple(Vec<Spanned<Expr>>),
        Comment(SpanRef),
        Name(SpanRef),
    }

    impl Atom {
        pub fn as_name(&self) -> Option<SpanRef> {
            match self {
                Self::Name(t) => Some(t.clone()),
                _ => None,
            }
        }
    }

    impl PartialEq for Atom {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Bool(a), Self::Bool(k)) => a == k,
                (Self::Int(n), Self::Int(i)) => n == i,
                (Self::Str(s), Self::Str(t)) => s == t,
                (Self::Name(a), Self::Name(b)) => a == b,

                (Self::Ellipsis, Self::Ellipsis) | (Self::None, Self::None) => true,

                _ => unimplemented!(),
            }
        }
    }

    impl From<PyToken> for Atom {
        fn from(value: PyToken) -> Self {
            match value {
                PyToken::Ellipsis => Self::Ellipsis,
                PyToken::None => Self::None,
                PyToken::True => Self::Bool(true),
                PyToken::False => Self::Bool(false),
                PyToken::Digits(n) => Self::Int(n),
                PyToken::CommentRef(n) => Self::Comment(n),
                PyToken::StringRef(n) => Self::Str(n),
                PyToken::Ident(n) => Self::Name(n),
                _ => unreachable!("{:?}", value),
            }
        }
    }
}
