use logos::Span;

use super::*;

pub(crate) type Block = Vec<AstObject>;

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub(crate) test: Box<AstObject>,
    pub(crate) body: Block,
    pub(crate) orelse: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct AstObject {
    pub(crate) span: Span,
    pub inner: AstNode,
}

impl Default for AstObject {
    fn default() -> Self {
        Self {
            span: Default::default(),
            inner: AstNode::BareToken(PyToken::Invalid),
        }
    }
}

static_assertions::assert_eq_size!(AstNode, (usize, usize, usize, usize, usize, usize, usize));

#[derive(Debug, Clone)]
pub enum AstNode {
    BareToken(PyToken),

    /// Constants such as `True`, `False`, `None`, spanref's (string literals) and integer literals.
    Constant(Constant),

    /// A raw identifier with no extra context.
    RawName(SpanEntry),

    /// if-elif-else blocks.
    If(Box<IfStmt>),

    // A sequence of statements.
    Block(Block),

    /// An attribute access.
    Attribute {
        value: Box<AstObject>,
        attr: SpanEntry,
    },

    Power {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Call {
        func: Box<AstObject>,
    },

    Subscript {
        value: Box<AstObject>,
    },

    FloorDiv {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    MatMult {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Sub {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Add {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    RShift {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    LShift {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Div {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Mult {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Mod {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Xor {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Or {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    And {
        left: Box<AstObject>,
        right: Box<AstObject>,
    },

    Assign {
        name: Box<AstObject>,
        value: Box<AstObject>,
        kind: Option<Box<AstObject>>,
    },

    FuncDef {
        ret: SpanEntry,
        ident: Box<AstObject>,
        args: Option<Vec<(SpanEntry, SpanEntry)>>,
        body: Box<AstObject>,
    },

    Return(Box<AstObject>),

    Is(Box<AstObject>),
    IsNot(Box<AstObject>),

    In(Box<AstObject>),
    NotIn(Box<AstObject>),

    Eq(Box<AstObject>),
    NotEq(Box<AstObject>),

    Lt(Box<AstObject>),
    Lte(Box<AstObject>),

    Gt(Box<AstObject>),
    Gte(Box<AstObject>),

    UAdd(Box<AstObject>),
    USub(Box<AstObject>),
    UInvert(Box<AstObject>),

    Await(Box<AstObject>),
}

impl From<Vec<AstObject>> for AstObject {
    fn from(block: Vec<AstObject>) -> Self {
        let span = block.iter().next().map(|o| o.span.start).unwrap_or(0)
            ..block.iter().last().map(|o| o.span.end).unwrap_or(0);

        Self {
            span,
            inner: AstNode::Block(block),
        }
    }
}

impl From<PyToken> for AstNode {
    fn from(value: PyToken) -> Self {
        match value {
            PyToken::True
            | PyToken::False
            | PyToken::None
            | PyToken::Ellipsis
            | PyToken::Digits(_)
            | PyToken::SpanRef(_) => Self::Constant(Constant::from(value)),

            PyToken::Ident(n) => Self::RawName(n),

            value => Self::BareToken(value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    None,
    Ellipsis,
    Int(isize),
    Str(SpanEntry),
    Bool(bool),
    Float(f64),
}

impl From<PyToken> for Constant {
    fn from(value: PyToken) -> Self {
        match value {
            PyToken::Ellipsis => Self::Ellipsis,
            PyToken::None => Self::None,
            PyToken::True => Self::Bool(true),
            PyToken::False => Self::Bool(false),
            PyToken::Digits(n) => Self::Int(n),
            PyToken::SpanRef(n) => Self::Str(n),
            _ => unreachable!("{:?}", value),
        }
    }
}
