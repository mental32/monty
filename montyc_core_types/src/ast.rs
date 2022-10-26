use std::fmt;

derive_everything! {
    #[repr(transparent)]
    pub struct AstNodeId(pub u32);
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(crate::SpanRef),
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
