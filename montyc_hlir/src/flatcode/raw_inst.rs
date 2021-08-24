#![allow(missing_docs)]

use std::fmt::Display;

use montyc_core::SpanRef;
use montyc_parser::ast::{InfixOp, UnaryOp};

#[derive(Debug, Clone)]
pub enum Dunder {
    Unary(UnaryOp),
    Infix(InfixOp),
    GetItem,
    SetItem,
    DocComment,
}

impl Display for Dunder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Dunder::Unary(_) => todo!(),
            Dunder::Infix(infix) => write!(f, "__{}__", infix.as_ref()),
            Dunder::DocComment => write!(f, "__doc__"),
            Dunder::GetItem => write!(f, "__getitem__"),
            Dunder::SetItem => write!(f, "__setitem__"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(SpanRef),
    None,
    Ellipsis,
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Int(n) => write!(f, "{}", n),
            Const::Float(n) => write!(f, "{}", n),
            Const::Bool(b) => write!(f, "{}", b),
            Const::String(s) => write!(f, "{:?}", s),
            Const::None => write!(f, "None"),
            Const::Ellipsis => write!(f, "<ellipsis: ...>"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RawInst<V = usize, R = SpanRef> {
    /// Define a function like: `def {name}({params}) -> {returns}`
    Defn {
        name: R,
        params: Vec<(R, Option<V>)>,
        returns: Option<V>,
        sequence_id: usize,
    },

    Class {
        name: R,
    },

    /// Call a callable-like value with like `callable(arguments)`.
    Call {
        callable: V,
        arguments: Vec<V>,
    },

    /// A named variable assignment like `a = ...`
    SetVar {
        variable: R,
        value: V,
    },

    /// A named variable lookup.
    UseVar {
        variable: R,
    },

    GetAttribute {
        object: V,
        name: R,
    },

    GetDunder {
        object: V,
        dunder: Dunder,
    },

    SetAttribute {
        object: V,
        name: R,
        value: V,
    },

    SetDunder {
        object: V,
        dunder: Dunder,
        value: V,
    },

    // GetItem {
    //     object: V,
    //     index: V,
    // },

    // SetItem {
    //     object: V,
    //     index: V,
    //     value: V,
    // },
    Import {
        path: Box<[R]>,
        relative: usize,
    },

    Const(Const),

    Tuple(Box<[V]>),

    Nop,

    Undefined,

    /// if `test` is a truthy value jump to `truthy` instr otherwise jump to `falsey`.
    If {
        test: V,
        truthy: Option<V>,
        falsey: Option<V>,
    },

    /// An unconditional branch to a value ref.
    Br {
        to: V,
    },

    PhiJump {
        recv: V,
        value: V,
    },

    PhiRecv,

    /// return from the current call frame with the specified return `value`.
    Return {
        value: V,
    },
}

fn format_vec_of_values(seq: &[usize]) -> String {
    seq.iter()
        .map(|value| format!("%{}", value))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Display for RawInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawInst::Defn {
                name,
                params,
                returns,
                sequence_id,
            } => write!(
                f,
                "defn({:?}) {:?} [{}] {:?}",
                sequence_id,
                name,
                {
                    let p = params
                        .iter()
                        .map(|(name, ann)| format!("{:?}: {:?}", name, ann))
                        .collect::<Vec<_>>();
                    p.join(", ")
                },
                returns
            ),

            RawInst::Class { name } => write!(f, "class {:?}", name),
            RawInst::Call {
                callable,
                arguments,
            } => write!(
                f,
                "call %{:?} [{}]",
                callable,
                format_vec_of_values(arguments)
            ),

            RawInst::SetVar { variable, value } => write!(f, "set-var {:?} %{:?}", variable, value),

            RawInst::UseVar { variable } => write!(f, "use-var {:?}", variable),
            RawInst::GetAttribute { object, name } => {
                write!(f, "get-attribute %{:?} {:?}", object, name)
            }

            RawInst::GetDunder { object, dunder } => {
                write!(f, "get-dunder %{:?} {}", object, dunder)
            }

            RawInst::SetAttribute {
                object,
                name,
                value,
            } => write!(f, "set-attribute %{:?} {:?} %{:?}", object, name, value),

            RawInst::SetDunder {
                object,
                dunder,
                value,
            } => write!(f, "set-dunder %{:?} {:?} %{:?}", object, dunder, value),

            // RawInst::GetItem { object, index } => {
            //     write!(f, "get-item %{:?} %{:?}", object, index)
            // }

            // RawInst::SetItem {
            //     object,
            //     index,
            //     value,
            // } => write!(f, "set-item {:?} %{:?} %{:?}", object, index, value),
            RawInst::Import { path, relative: _ } => write!(f, "import {:?}", path),
            RawInst::Const(c) => write!(f, "const {}", c),
            RawInst::Nop => write!(f, "nop"),
            RawInst::Undefined => write!(f, "undef"),
            RawInst::If {
                test,
                truthy,
                falsey,
            } => write!(
                f,
                "if %{} {}, {}",
                test,
                truthy
                    .map(|v| format!("%{:?}", v))
                    .unwrap_or("nop".to_string()),
                falsey
                    .map(|v| format!("%{:?}", v))
                    .unwrap_or("nop".to_string())
            ),
            RawInst::Br { to } => write!(f, "branch %{:?}", to),
            RawInst::PhiJump { recv, value } => write!(f, "phi-jump %{:?} %{:?}", recv, value),
            RawInst::PhiRecv => write!(f, "phi-recv"),
            RawInst::Return { value } => write!(f, "return %{:?}", value),
            RawInst::Tuple(inner) => write!(f, "tuple [{}]", format_vec_of_values(inner)),
        }
    }
}
