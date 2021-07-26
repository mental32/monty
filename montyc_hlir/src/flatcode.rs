//! A post-processed AST
#![allow(warnings)]

use montyc_core::{patma, SpanRef};
use montyc_parser::{
    ast::{
        Assign, Atom, ClassDef, Expr, FunctionDef, IfChain, Import, InfixOp, Primary, Return, While,
    },
    AstObject, AstVisitor,
};

#[derive(Debug)]
enum RawInst<ValueRef = usize, StringRef = SpanRef> {
    /// Define a function like: `def {name}({params}) -> {returns}`
    Defn {
        name: StringRef,
        params: Vec<(StringRef, Option<ValueRef>)>,
        returns: Option<ValueRef>,
    },

    Class {
        name: StringRef,
    },

    /// Call a callable-like value with like `callable(arguments)`.
    Call {
        callable: ValueRef,
        arguments: Vec<ValueRef>,
    },

    /// A named variable assignment like `a = ...`
    SetVar {
        variable: StringRef,
        value: ValueRef,
    },

    /// A named variable lookup.
    UseVar {
        variable: StringRef,
    },

    GetAttribute {
        object: ValueRef,
        name: StringRef,
    },

    GetDunder {
        object: ValueRef,
        dunder: InfixOp,
    },

    SetAttribute {
        object: ValueRef,
        name: StringRef,
        value: ValueRef,
    },

    IConst(i64),
    FConst(f64),
    BConst(bool),
    SConst(SpanRef),

    None,

    Nop,

    Undefined,

    /// if `test` is a truthy value jump to `truthy` instr otherwise jump to `falsey`.
    If {
        test: ValueRef,
        truthy: ValueRef,
        falsey: ValueRef,
    },

    /// return from the current call frame with the specified return `value`.
    Return {
        value: ValueRef,
    },
}

/// An (instruction -> value) pair.
#[derive(Debug)]
struct FlatInst<ValueRef = usize, StringRef = SpanRef> {
    op: RawInst<ValueRef, StringRef>,
    value: ValueRef,
}

struct FlatCode {
    sequence_index: usize,
    sequences: Vec<Vec<FlatInst>>,
}

impl FlatCode {
    fn inst(&mut self, raw_inst: RawInst) -> usize {
        match self.sequences.get_mut(self.sequence_index) {
            Some(seq) => {
                seq.push(FlatInst {
                    op: raw_inst,
                    value: seq.len(),
                });

                seq.len().saturating_sub(1)
            }

            None => unreachable!(),
        }
    }
}

impl AstVisitor<usize> for FlatCode {
    fn visit_any(&mut self, o: &dyn AstObject) -> usize {
        todo!("{:#?}", o.into_ast_node());
    }

    fn visit_funcdef(&mut self, fndef: &FunctionDef) -> usize {
        let params = match (&fndef.reciever, &fndef.args) {
            (None, None) => vec![],
            (None, Some(args)) => args
                .iter()
                .map(|(arg, ann)| (*arg, ann.as_ref().map(|ann| ann.visit_with(self))))
                .collect(),

            (Some(recv), None) => vec![(recv.inner.as_name().unwrap(), None)],
            (Some(recv), Some(args)) => {
                let mut params = vec![(recv.inner.as_name().unwrap(), None)];

                params.extend(
                    args.iter()
                        .map(|(arg, ann)| (*arg, ann.as_ref().map(|ann| ann.visit_with(self)))),
                );
                params
            }
        };

        let returns = fndef.returns.as_ref().map(|r| r.visit_with(self));

        self.inst(RawInst::Defn {
            name: fndef.name.inner.as_name().unwrap(),
            params,
            returns,
        })
    }

    fn visit_expr(&mut self, expr: &Expr) -> usize {
        expr.visit_with(self)
    }

    fn visit_int(&mut self, int: &Atom) -> usize {
        let n = patma!(*n => Atom::Int(n) in int).unwrap();
        self.inst(RawInst::IConst(n))
    }

    fn visit_float(&mut self, node: &Atom) -> usize {
        let n = patma!(*n => Atom::Float(n) in node).unwrap();
        self.inst(RawInst::FConst(n))
    }

    fn visit_str(&mut self, node: &Atom) -> usize {
        let n = patma!(*n => Atom::Str(n) in node).unwrap();
        self.inst(RawInst::SConst(n))
    }

    fn visit_none(&mut self, node: &Atom) -> usize {
        self.inst(RawInst::None)
    }

    fn visit_name(&mut self, node: &Atom) -> usize {
        let variable = patma!(name.clone() => Atom::Name(name) in node).unwrap();
        self.inst(RawInst::UseVar { variable })
    }

    fn visit_tuple(&mut self, node: &Atom) -> usize {
        self.visit_any(node)
    }

    fn visit_ellipsis(&mut self, node: &Atom) -> usize {
        self.inst(RawInst::Undefined)
    }

    fn visit_bool(&mut self, node: &Atom) -> usize {
        let v = patma!(v => Atom::Bool(v) in node).unwrap();
        self.inst(RawInst::BConst(*v))
    }

    fn visit_import(&mut self, import: &Import) -> usize {
        todo!();
    }

    fn visit_classdef(&mut self, classdef: &ClassDef) -> usize {
        todo!();
    }

    fn visit_ifstmt(&mut self, ifch: &IfChain) -> usize {
        todo!()
    }

    fn visit_pass(&mut self) -> usize {
        self.inst(RawInst::Nop)
    }

    fn visit_assign(&mut self, asn: &Assign) -> usize {
        match &asn.name.inner {
            Primary::Atomic(atom) => {
                let variable = patma!(name => Atom::Name(name) in atom.inner).unwrap();
                let value = asn.value.visit_with(self);

                self.inst(RawInst::SetVar { variable, value })
            }

            Primary::Await(_) | Primary::Call { .. } => unreachable!(),

            Primary::Subscript { .. } => todo!(),
            Primary::Attribute { left, attr } => {
                let name = patma!(name => Atom::Name(name) in attr.inner).unwrap();

                let object = left.visit_with(self);
                let value = asn.value.visit_with(self);

                self.inst(RawInst::SetAttribute {
                    object,
                    name,
                    value,
                })
            }
        }
    }

    fn visit_return(&mut self, ret: &Return) -> usize {
        let rv = match &ret.value {
            Some(expr) => expr.visit_with(self),
            None => self.inst(RawInst::None),
        };

        self.inst(RawInst::Return { value: rv })
    }

    fn visit_while(&mut self, while_: &While) -> usize {
        todo!()
    }

    fn visit_binop(&mut self, expr: &Expr) -> usize {
        let (left, op, right) =
            patma!((left, op, right) => Expr::BinOp { left, op, right } in expr).unwrap();

        let left_value = left.visit_with(self);
        let right_value = right.visit_with(self);

        let dunder = self.inst(RawInst::GetDunder {
            object: left_value,
            dunder: op.clone(),
        });

        self.inst(RawInst::Call {
            callable: dunder,
            arguments: vec![left_value, right_value],
        })
    }

    fn visit_unary(&mut self, unary: &Expr) -> usize {
        todo!()
    }

    fn visit_ternary(&mut self, ternary: &Expr) -> usize {
        todo!()
    }

    fn visit_named_expr(&mut self, expr: &Expr) -> usize {
        let (target, value) =
            patma!((target, value) => Expr::Named { target, value } in expr).unwrap();

        let variable = patma!(name => Atom::Name(name) in target.inner).unwrap();
        let value = value.visit_with(self);

        self.inst(RawInst::SetVar { variable, value });

        value
    }

    fn visit_call(&mut self, call: &Primary) -> usize {
        let (func, args) = patma!((func, args) => Primary::Call { func, args } in call).unwrap();

        let callable = func.visit_with(self);
        let arguments = match args {
            Some(args) => args.iter().map(|arg| arg.visit_with(self)).collect(),
            None => vec![],
        };

        self.inst(RawInst::Call {
            callable,
            arguments,
        })
    }

    fn visit_subscript(&mut self, call: &Primary) -> usize {
        todo!()
    }
}
