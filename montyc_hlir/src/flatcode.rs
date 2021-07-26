//! A post-processed, flattened, AST, with extra semantics sprinkled in.
#![allow(warnings)]

use montyc_core::{patma, SpanRef};
use montyc_parser::{
    ast::{
        Assign, Atom, ClassDef, Expr, FunctionDef, IfChain, Import, InfixOp, Primary, Return,
        UnaryOp, While,
    },
    AstNode, AstObject, AstVisitor,
};

const INVALID_VALUE: usize = std::usize::MAX;

#[derive(Debug, Clone)]
enum Dunder {
    Unary(UnaryOp),
    Infix(InfixOp),
    Comment,
}

#[derive(Debug, Clone)]
enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(SpanRef),
    None,
    Ellipsis,
}

#[derive(Debug, Clone)]
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
        dunder: Dunder,
    },

    SetAttribute {
        object: ValueRef,
        name: StringRef,
        value: ValueRef,
    },

    SetDunder {
        object: ValueRef,
        dunder: Dunder,
        value: ValueRef,
    },

    GetItem {
        object: ValueRef,
        index: ValueRef,
    },

    SetItem {
        object: ValueRef,
        index: ValueRef,
        value: ValueRef,
    },

    Import(StringRef),

    Const(Const),

    Nop,

    Undefined,

    /// if `test` is a truthy value jump to `truthy` instr otherwise jump to `falsey`.
    If {
        test: ValueRef,
        truthy: Option<ValueRef>,
        falsey: Option<ValueRef>,
    },

    /// An unconditional branch to a value ref.
    Br {
        to: ValueRef,
    },

    PhiJump {
        recv: ValueRef,
        value: ValueRef,
    },

    PhiRecv,

    /// return from the current call frame with the specified return `value`.
    Return {
        value: ValueRef,
    },
}

/// An (instruction -> value) pair.
#[derive(Debug, Clone)]
struct FlatInst<ValueRef = usize, StringRef = SpanRef> {
    op: RawInst<ValueRef, StringRef>,
    value: ValueRef,
}

#[derive(Debug)]
pub struct FlatCode {
    sequence_index: usize,
    sequences: Vec<Vec<FlatInst>>,
}

impl FlatCode {
    pub fn new() -> Self {
        Self {
            sequence_index: 0,
            sequences: vec![vec![]],
        }
    }
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

    fn visit_module(&mut self, module: &montyc_parser::ast::Module) -> usize {
        for stmt in &module.body {
            stmt.visit_with(self);
        }

        INVALID_VALUE
    }

    fn visit_expr(&mut self, expr: &Expr) -> usize {
        expr.visit_with(self)
    }

    fn visit_int(&mut self, int: &Atom) -> usize {
        let n = patma!(*n, Atom::Int(n) in int).unwrap();
        self.inst(RawInst::Const(Const::Int(n)))
    }

    fn visit_float(&mut self, node: &Atom) -> usize {
        let n = patma!(*n, Atom::Float(n) in node).unwrap();
        self.inst(RawInst::Const(Const::Float(n)))
    }

    fn visit_str(&mut self, node: &Atom) -> usize {
        let n = patma!(*n, Atom::Str(n) in node).unwrap();
        self.inst(RawInst::Const(Const::String(n)))
    }

    fn visit_none(&mut self, node: &Atom) -> usize {
        self.inst(RawInst::Const(Const::None))
    }

    fn visit_name(&mut self, node: &Atom) -> usize {
        let variable = patma!(name.clone(), Atom::Name(name) in node).unwrap();
        self.inst(RawInst::UseVar { variable })
    }

    fn visit_tuple(&mut self, node: &Atom) -> usize {
        self.visit_any(node)
    }

    fn visit_ellipsis(&mut self, node: &Atom) -> usize {
        self.inst(RawInst::Undefined)
    }

    fn visit_bool(&mut self, node: &Atom) -> usize {
        let v = patma!(*v, Atom::Bool(v) in node).unwrap();
        self.inst(RawInst::Const(Const::Bool(v)))
    }

    fn visit_import(&mut self, import: &Import) -> usize {
        match import {
            Import::Names(names) => {
                for name in names {
                    let name = name.inner.as_name().unwrap();
                    let value = self.inst(RawInst::Import(name));
                    self.inst(RawInst::SetVar {
                        variable: name,
                        value,
                    });
                }

                INVALID_VALUE
            }

            Import::From {
                module,
                names,
                level,
            } => {
                let name = module.inner.as_name().unwrap();
                let module = self.inst(RawInst::Import(name));

                for name in names {
                    let name = name.inner.as_name().unwrap();
                    let attr = self.inst(RawInst::GetAttribute {
                        object: module,
                        name,
                    });

                    self.inst(RawInst::SetVar {
                        variable: name,
                        value: attr,
                    });
                }

                INVALID_VALUE
            }
        }
    }

    fn visit_classdef(&mut self, classdef: &ClassDef) -> usize {
        let name = classdef.name.inner.as_name().unwrap();
        let class = self.inst(RawInst::Class { name });

        let mut body = classdef.body.iter().peekable();

        if let Some(node) = body.peek() {
            let node = node.inner.into_ast_node();

            if let AstNode::Str(string) = node {
                let string = string.visit_with(self);

                self.inst(RawInst::SetDunder {
                    object: class,
                    dunder: Dunder::Comment,
                    value: string,
                });

                let _ = body.next();
            }
        }

        for node in body {
            match node.inner.into_ast_node() {
                AstNode::FuncDef(def) => {
                    let function = def.visit_with(self);
                    self.inst(RawInst::SetAttribute {
                        object: class,
                        name: def.name.inner.as_name().unwrap(),
                        value: function,
                    });
                }

                AstNode::Assign(assign) => {
                    let assignment = assign.visit_with(self);
                    self.inst(RawInst::SetAttribute {
                        object: class,
                        name: assign.name.inner.as_name().unwrap(),
                        value: assignment,
                    });
                }

                _ => {
                    node.visit_with(self);
                }
            }
        }
        class
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

    fn visit_ifstmt(&mut self, ifch: &IfChain) -> usize {
        let branch_indices: Vec<_> = ifch
            .branches
            .iter()
            .map(|branch| {
                let test = branch.inner.test.visit_with(self);
                self.inst(RawInst::If {
                    test,
                    truthy: Some(INVALID_VALUE),
                    falsey: None,
                })
            })
            .collect();

        let true_const = self.inst(RawInst::Const(Const::Bool(true)));

        let or_else_branch = ifch.orelse.as_ref().map(
            (|x| {
                self.inst(RawInst::If {
                    test: true_const,
                    truthy: Some(INVALID_VALUE),
                    falsey: None,
                })
            }),
        );

        let body_indices: Vec<_> = ifch
            .branches
            .iter()
            .map(|branch| {
                let body_entry = self.inst(RawInst::Nop);

                for node in branch.inner.body.iter() {
                    node.visit_with(self);
                }

                self.inst(RawInst::If {
                    test: true_const,
                    truthy: Some(INVALID_VALUE),
                    falsey: Some(INVALID_VALUE),
                });

                body_entry
            })
            .collect();

        {
            let seq = self.sequences.get_mut(self.sequence_index).unwrap();

            for (branch, body) in branch_indices.iter().zip(body_indices.iter()) {
                let inst = seq.get_mut(*branch).unwrap();
                if let RawInst::If {
                    test,
                    truthy,
                    falsey,
                } = &mut inst.op
                {
                    truthy.replace(*body);
                } else {
                    unreachable!();
                }
            }
        }

        if let Some(or_else_branch) = or_else_branch {
            let or_else_start = self.inst(RawInst::Nop);

            for node in ifch.orelse.as_ref().unwrap() {
                node.visit_with(self);
            }

            let inst = self
                .sequences
                .get_mut(self.sequence_index)
                .unwrap()
                .get_mut(or_else_branch)
                .unwrap();
            if let RawInst::If {
                test,
                truthy,
                falsey,
            } = &mut inst.op
            {
                truthy.replace(or_else_branch);
            } else {
                unreachable!();
            }
        }

        INVALID_VALUE
    }

    fn visit_pass(&mut self) -> usize {
        self.inst(RawInst::Nop)
    }

    fn visit_assign(&mut self, asn: &Assign) -> usize {
        match &asn.name.inner {
            Primary::Atomic(atom) => {
                let variable = patma!(name, Atom::Name(name) in atom.inner).unwrap();
                let value = asn.value.visit_with(self);

                self.inst(RawInst::SetVar { variable, value })
            }

            Primary::Await(_) | Primary::Call { .. } => unreachable!(),

            Primary::Subscript { .. } => todo!(),
            Primary::Attribute { left, attr } => {
                let name = patma!(name, Atom::Name(name) in attr.inner).unwrap();

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
            None => self.inst(RawInst::Const(Const::None)),
        };

        self.inst(RawInst::Return { value: rv })
    }

    fn visit_while(&mut self, while_: &While) -> usize {
        let start = self.inst(RawInst::Nop);
        let test = while_.test.inner.visit_with(self);

        let jump = self.inst(RawInst::If {
            test,
            truthy: None,
            falsey: None,
        });

        for node in &while_.body {
            node.visit_with(self);
        }

        self.inst(RawInst::Br { to: start });
        let or_else = self.inst(RawInst::Nop);

        let mut seq = self.sequences.get_mut(self.sequence_index).unwrap();

        seq[jump].op = RawInst::If {
            test,
            truthy: None,
            falsey: Some(or_else),
        };

        INVALID_VALUE
    }

    fn visit_binop(&mut self, expr: &Expr) -> usize {
        let (left, op, right) =
            patma!((left, op, right), Expr::BinOp { left, op, right } in expr).unwrap();

        let left_value = left.visit_with(self);
        let right_value = right.visit_with(self);

        let dunder = self.inst(RawInst::GetDunder {
            object: left_value,
            dunder: Dunder::Infix(op.clone()),
        });

        self.inst(RawInst::Call {
            callable: dunder,
            arguments: vec![left_value, right_value],
        })
    }

    fn visit_unary(&mut self, unary: &Expr) -> usize {
        let (op, value) = patma!((op, value), Expr::Unary {op, value} in unary).unwrap();

        let value = value.visit_with(self);

        let dunder = self.inst(RawInst::GetDunder {
            object: value,
            dunder: Dunder::Unary(op.clone()),
        });

        self.inst(RawInst::Call {
            callable: dunder,
            arguments: vec![value],
        })
    }

    fn visit_ternary(&mut self, ternary: &Expr) -> usize {
        let (test, body, orelse) =
            patma!((test, body, orelse), Expr::If {test, body, orelse} in ternary).unwrap();

        let test = test.visit_with(self);
        let test_jump = self.inst(RawInst::Nop);

        let true_value = body.visit_with(self);
        let after_value_jump_true = self.inst(RawInst::Br { to: INVALID_VALUE });

        let false_header = self.inst(RawInst::Nop);
        let false_value = orelse.visit_with(self);
        let after_value_jump_false = self.inst(RawInst::Br { to: INVALID_VALUE });

        let phi_recv = self.inst(RawInst::PhiRecv);

        let mut seq = self.sequences.get_mut(self.sequence_index).unwrap();

        seq[test_jump].op = RawInst::If {
            test,
            truthy: None,
            falsey: Some(false_header),
        };

        seq[after_value_jump_true].op = RawInst::PhiJump {
            recv: phi_recv,
            value: true_value,
        };

        seq[after_value_jump_false].op = RawInst::PhiJump {
            recv: phi_recv,
            value: false_value,
        };

        phi_recv
    }

    fn visit_named_expr(&mut self, expr: &Expr) -> usize {
        let (target, value) =
            patma!((target, value), Expr::Named { target, value } in expr).unwrap();

        let variable = patma!(name, Atom::Name(name) in target.inner).unwrap();
        let value = value.visit_with(self);

        self.inst(RawInst::SetVar { variable, value });

        value
    }

    fn visit_call(&mut self, call: &Primary) -> usize {
        let (func, args) = patma!((func, args), Primary::Call { func, args } in call).unwrap();

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

    fn visit_subscript(&mut self, subscr: &Primary) -> usize {
        let (value, index) =
            patma!((value, index), Primary::Subscript { value, index } in subscr).unwrap();

        let object = value.visit_with(self);
        let index = index.visit_with(self);

        let get_item = self.inst(RawInst::GetItem { object, index });

        get_item
    }
}
