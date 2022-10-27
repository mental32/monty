//! The collection of various AST nodes.

use montyc_lexer::Span;

pub mod ann;
pub mod assign;
pub mod atom;
pub mod classdef;
pub mod expr;
pub mod funcdef;
pub mod ifstmt;
pub mod import;
pub mod module;
pub mod primary;
pub mod return_;
pub mod spanned;
pub mod statement;
pub mod while_;

use std::fmt;

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(montyc_lexer::SpanRef),
    None,
    Ellipsis,
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(n) => write!(f, "{}", n),
            Constant::Float(n) => write!(f, "{}", n),
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::String(s) => write!(f, "{:?}", s),
            Constant::None => write!(f, "None"),
            Constant::Ellipsis => write!(f, "<ellipsis: ...>"),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Pass;

impl AstObject for Pass {
    fn into_ast_node(&self) -> AstNode {
        AstNode::Pass(self.clone())
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        self
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        visitor.visit_pass(self, span)
    }
}

/// An explicit enumeration of all AST nodes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum AstNode {
    Import(import::Import),
    ClassDef(classdef::ClassDef),
    FuncDef(funcdef::FunctionDef),
    If(ifstmt::IfChain),
    Assign(assign::Assign),
    Int(atom::Atom),
    Str(atom::Atom),
    Comment(atom::Atom),
    Bool(atom::Atom),
    Float(atom::Atom),
    Tuple(atom::Atom),
    Name(atom::Atom),
    BinOp(expr::Expr),
    IfExpr(expr::Expr),
    Unary(expr::Expr),
    NamedExpr(expr::Expr),
    None(atom::Atom),
    Ellipsis(atom::Atom),
    Subscript(primary::Primary),
    Call(primary::Primary),
    Attr(primary::Primary),
    Ret(return_::Return),
    While(while_::While),
    Annotation(ann::Annotation),
    Pass(Pass),
}

// impl AstNode {
//     pub fn as_inner(&self) -> &dyn AstObject {
//         match self {
//             AstNode::Import(import) => import,
//             AstNode::ClassDef(classdef) => classdef,
//             AstNode::FuncDef(fndef) => fndef,
//             AstNode::If(ifch) => ifch,
//             AstNode::Assign(asn) => asn,
//             AstNode::Pass => self,
//             _ => todo!(),
//         }
//     }
// }

impl AstObject for AstNode {
    fn into_ast_node(&self) -> AstNode {
        self.clone()
    }

    //     fn span(&self) -> Option<Span> {
    //         None
    //     }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        unimplemented!()
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        match self {
            AstNode::Import(import) => import.visit_with(visitor, span),
            AstNode::ClassDef(classdef) => classdef.visit_with(visitor, span),
            AstNode::FuncDef(fndef) => fndef.visit_with(visitor, span),
            AstNode::If(ifch) => ifch.visit_with(visitor, span),
            AstNode::Assign(asn) => asn.visit_with(visitor, span),
            AstNode::Ret(ret) => ret.visit_with(visitor, span),
            AstNode::Str(st) => st.visit_with(visitor, span),
            AstNode::Pass(pass) => pass.visit_with(visitor, span),
            _ => todo!("{:?}", self),
        }
    }
}

// impl From<AstNode> for Box<dyn AstObject> {
//     fn from(node: AstNode) -> Self {
//         match node {
//             AstNode::Import(import) => Box::new(import),
//             AstNode::ClassDef(classdef) => Box::new(classdef),
//             AstNode::FuncDef(funcdef) => Box::new(funcdef),
//             AstNode::If(ifstmt) => Box::new(ifstmt),
//             AstNode::Pass => Box::new(Statement::Pass),
//             AstNode::Assign(asn) => Box::new(asn),
//             _ => todo!(),
//         }
//     }
// }

/// An opaque representation of an AST node.
pub trait AstObject {
    /// A copy of `Self` wrapped into an `AstNode` variant.
    fn into_ast_node(&self) -> AstNode;

    // /// The span of the AST object, if available.
    // fn span(&self) -> Option<Span>;

    /// The inner unspanned AST object.
    fn unspanned<'a>(&'a self) -> &'a dyn AstObject;

    /// Invoke the appropriate visitor method for this current AST node.
    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized;
}

impl<T> AstObject for spanned::Spanned<T>
where
    T: AstObject,
{
    fn into_ast_node(&self) -> AstNode {
        self.inner.into_ast_node()
    }

    fn unspanned<'a>(&'a self) -> &'a dyn AstObject {
        &self.inner
    }

    fn visit_with<U>(&self, visitor: &mut dyn AstVisitor<U>, span: Option<Span>) -> U
    where
        Self: Sized,
    {
        self.inner
            .visit_with(visitor, span.or(Some(self.span.clone())))
    }
}

/// A visitor trait used for walking an AST object.
pub trait AstVisitor<T = ()> {
    fn visit_any(&mut self, _: &dyn AstObject) -> T;

    fn visit_pass(&mut self, pass: &Pass, _span: Option<Span>) -> T {
        self.visit_any(pass)
    }

    fn visit_funcdef(&mut self, fndef: &funcdef::FunctionDef, _span: Option<Span>) -> T {
        self.visit_any(fndef)
    }

    fn visit_expr(&mut self, expr: &expr::Expr, _span: Option<Span>) -> T {
        self.visit_any(expr)
    }

    fn visit_int(&mut self, int: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(int)
    }

    fn visit_float(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_str(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_none(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_name(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_tuple(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_ellipsis(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_bool(&mut self, node: &atom::Atom, _span: Option<Span>) -> T {
        self.visit_any(node)
    }

    fn visit_import(&mut self, import: &import::Import, _span: Option<Span>) -> T {
        self.visit_any(import)
    }

    fn visit_classdef(&mut self, classdef: &classdef::ClassDef, _span: Option<Span>) -> T {
        self.visit_any(classdef)
    }

    fn visit_ifstmt(&mut self, ifch: &ifstmt::IfChain, _span: Option<Span>) -> T {
        self.visit_any(ifch)
    }

    fn visit_assign(&mut self, asn: &assign::Assign, _span: Option<Span>) -> T {
        self.visit_any(asn)
    }

    fn visit_return(&mut self, ret: &return_::Return, _span: Option<Span>) -> T {
        self.visit_any(ret)
    }

    fn visit_while(&mut self, while_: &while_::While, _span: Option<Span>) -> T {
        self.visit_any(while_)
    }

    fn visit_binop(&mut self, expr: &expr::Expr, _span: Option<Span>) -> T {
        self.visit_any(expr)
    }

    fn visit_unary(&mut self, unary: &expr::Expr, _span: Option<Span>) -> T {
        self.visit_any(unary)
    }

    fn visit_ternary(&mut self, ternary: &expr::Expr, _span: Option<Span>) -> T {
        self.visit_any(ternary)
    }

    fn visit_named_expr(&mut self, expr: &expr::Expr, _span: Option<Span>) -> T {
        self.visit_any(expr)
    }

    fn visit_call(&mut self, call: &primary::Primary, _span: Option<Span>) -> T {
        self.visit_any(call)
    }

    fn visit_subscript(&mut self, call: &primary::Primary, _span: Option<Span>) -> T {
        self.visit_any(call)
    }

    fn visit_attr(&mut self, attr: &primary::Primary, _span: Option<Span>) -> T {
        self.visit_any(attr)
    }

    fn visit_module(&mut self, module: &module::Module, _span: Option<Span>) -> T {
        self.visit_any(module)
    }

    fn visit_annotation(&mut self, ann: &ann::Annotation, _span: Option<Span>) -> T {
        self.visit_any(ann)
    }
}
