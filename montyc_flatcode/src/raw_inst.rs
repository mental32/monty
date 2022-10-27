use std::fmt::Display;

use montyc_lexer::SpanRef;
use montyc_parser::prelude::*;

#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Dunder {
    /// unary dunders i.e. __neg__
    Unary(UnaryOp),
    /// infix dunders i.e. __add__
    Infix(InfixOp),
    /// __get_item__
    GetItem,
    /// __set_item__
    SetItem,
    /// __doc__
    DocComment,
    /// __bool__
    AsBool,
}

impl Display for Dunder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Dunder::Unary(_) => todo!(),
            Dunder::Infix(infix) => write!(f, "__{}__", infix.as_ref()),
            Dunder::DocComment => write!(f, "__doc__"),
            Dunder::GetItem => write!(f, "__getitem__"),
            Dunder::SetItem => write!(f, "__setitem__"),
            Dunder::AsBool => write!(f, "__bool__"),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
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

    BuildClass {
        sequence: usize,
        class: V,
    },

    RefAsStr {
        r: R,
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
        attr: V,
    },

    GetDunder {
        object: V,
        dunder: Dunder,
    },

    SetAttribute {
        object: V,
        attr: V,
        value: V,
    },

    SetDunder {
        object: V,
        dunder: Dunder,
        value: V,
    },

    Import {
        path: Box<[R]>,
        relative: usize,
    },

    Const(montyc_ast::Constant),

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

    JumpTarget,

    PhiRecv,

    /// return from the current call frame with the specified return `value`.
    Return {
        value: V,
    },

    SetAnnotation {
        name: R,
        annotation: V,
    },
}

pub trait InstVisitor<T, V = usize, R = SpanRef> {
    fn visit_defn(
        &mut self,
        name: R,
        params: &[(R, Option<V>)],
        returns: Option<V>,
        seq: usize,
    ) -> T;
    fn visit_class(&mut self, name: R) -> T;
    fn visit_build_class(&mut self, klass: V, seq: usize) -> T;
    fn visit_ref_as_str(&mut self, r: R) -> T;
    fn visit_call(&mut self, callable: V, arguments: &[V]) -> T;
    fn visit_set_var(&mut self, variable: R, value: V) -> T;
    fn visit_use_var(&mut self, variable: R) -> T;
    fn visit_get_attribute(&mut self, variable: R, value: V) -> T;
    fn visit_set_attribute(&mut self, variable: R, attr: V, value: V) -> T;
    fn visit_get_dunder(&mut self, object: V, dunder: &Dunder) -> T;
    fn visit_set_dunder(&mut self, object: V, dunder: &Dunder, value: V) -> T;
    fn visit_import(&mut self, path: &[R], relative: usize) -> T;
    fn visit_const(&mut self, cst: &montyc_ast::Constant) -> T;
    fn visit_tuple(&mut self, tple: &[V]) -> T;
    fn visit_nop(&mut self) -> T;
    fn visit_undef(&mut self) -> T;
    fn visit_if(&mut self, test: V, truthy: Option<V>, falsey: Option<V>) -> T;
    fn visit_br(&mut self, to: V) -> T;
    fn visit_phi_jump(&mut self, recv: V, value: V) -> T;
    fn visit_jump_target(&mut self) -> T;
    fn visit_phi_recv(&mut self) -> T;
    fn visit_return(&mut self, value: V) -> T;
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
            RawInst::SetAnnotation { name, annotation } => {
                write!(f, "set-annotation {:?} {:?}", name, annotation)
            }
            RawInst::RefAsStr { r } => write!(f, "ref-as-str {:?}", r),

            RawInst::BuildClass { sequence, class } => {
                write!(f, "build-class seq({}) %{:?}", sequence, class)
            }

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
            RawInst::GetAttribute { object, attr } => {
                write!(f, "get-attribute %{:?} {:?}", object, attr)
            }

            RawInst::GetDunder { object, dunder } => {
                write!(f, "get-dunder %{:?} {}", object, dunder)
            }

            RawInst::SetAttribute {
                object,
                attr,
                value,
            } => write!(f, "set-attribute %{:?} {:?} %{:?}", object, attr, value),

            RawInst::SetDunder {
                object,
                dunder,
                value,
            } => write!(f, "set-dunder %{:?} {:?} %{:?}", object, dunder, value),

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
            RawInst::JumpTarget => write!(f, "jump-target"),
        }
    }
}
