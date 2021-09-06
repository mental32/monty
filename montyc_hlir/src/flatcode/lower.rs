use montyc_core::{patma, Span};

use montyc_parser::{
    ast::{Assign, ClassDef, FunctionDef, IfChain, Primary, Return, While},
    ast::{Atom, Expr, Import},
    AstNode, AstObject, AstVisitor,
};

use crate::flatcode::SequenceType;

use super::{
    raw_inst::{Const, Dunder, RawInst},
    FlatCode, INVALID_VALUE,
};

fn visit_const(this: &mut FlatCode, node: &Atom, _span: Option<Span>) -> usize {
    let const_v = match node {
        Atom::None => Const::None,
        Atom::Ellipsis => Const::Ellipsis,
        Atom::Int(n) => Const::Int(*n),
        Atom::Str(s) => Const::String(*s),
        Atom::Bool(b) => Const::Bool(*b),
        Atom::Float(f) => Const::Float(*f),
        _ => unreachable!(),
    };

    this.inst(RawInst::Const(const_v))
}

impl AstVisitor<usize> for FlatCode {
    fn visit_module(&mut self, module: &montyc_parser::ast::Module, _: Option<Span>) -> usize {
        for stmt in &module.body {
            stmt.visit_with(self);
        }

        INVALID_VALUE
    }

    fn visit_any(&mut self, o: &dyn AstObject) -> usize {
        todo!("{:#?}", o.into_ast_node());
    }

    fn visit_expr(&mut self, expr: &Expr, _: Option<Span>) -> usize {
        expr.visit_with(self)
    }

    // -- const shims

    fn visit_int(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_float(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_str(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_none(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_ellipsis(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_bool(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    // -- other visitors

    fn visit_pass(&mut self) -> usize {
        self.inst(RawInst::Nop)
    }

    fn visit_name(&mut self, node: &Atom, _span: Option<Span>) -> usize {
        let variable = patma!(name.clone(), Atom::Name(name) in node).unwrap();
        self.inst(RawInst::UseVar { variable })
    }

    fn visit_tuple(&mut self, node: &Atom, _span: Option<Span>) -> usize {
        let inner = patma!(name.clone(), Atom::Tuple(name) in node).unwrap();
        let inner: Vec<_> = inner.iter().map(|elem| elem.visit_with(self)).collect();

        self.inst(RawInst::Tuple(inner.into_boxed_slice()))
    }

    fn visit_import(&mut self, import: &Import, _: Option<Span>) -> usize {
        match import {
            Import::Names(names) => {
                for name in names {
                    let path = name
                        .inner
                        .components()
                        .into_iter()
                        .map(|at| at.as_name().unwrap())
                        .collect::<Vec<_>>()
                        .into_boxed_slice();

                    let root = path.get(0).unwrap().clone();

                    let value = self.inst(RawInst::Import { path, relative: 0 });

                    self.inst(RawInst::SetVar {
                        variable: root,
                        value,
                    });
                }
            }

            Import::From {
                module,
                names,
                level,
            } => {
                let path = module
                    .inner
                    .components()
                    .into_iter()
                    .map(|at| at.as_name().unwrap())
                    .collect();

                let module = self.inst(RawInst::Import {
                    path,
                    relative: *level,
                });

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
            }
        }

        INVALID_VALUE
    }

    fn visit_classdef(&mut self, classdef: &ClassDef, _: Option<Span>) -> usize {
        let name = classdef.name.inner.as_name().unwrap();
        let class = self.inst(RawInst::Class { name });

        self.inst(RawInst::SetVar {
            variable: name,
            value: class,
        });

        let mut body = classdef.body.iter().peekable();

        if let Some(node) = body.peek() {
            let node = node.inner.into_ast_node();

            if let AstNode::Str(string) = node {
                let string = string.visit_with(self);

                self.inst(RawInst::SetDunder {
                    object: class,
                    dunder: Dunder::DocComment,
                    value: string,
                });

                let _ = body.next();
            }
        }

        for node in body {
            match node.inner.into_ast_node() {
                AstNode::FuncDef(def) => {
                    let function = def.visit_with(self);

                    self.sequences[self.sequence_index]
                        .inst
                        .last_mut()
                        .unwrap()
                        .op = RawInst::SetAttribute {
                        object: class,
                        name: def.name.inner.as_name().unwrap(),
                        value: function,
                    };
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

    fn visit_funcdef(&mut self, fndef: &FunctionDef, _: Option<Span>) -> usize {
        let _decorators: Vec<_> = fndef
            .decorator_list
            .iter()
            .rev()
            .map(|dec| dec.visit_with(self))
            .collect();

        let params = match (&fndef.reciever, &fndef.args) {
            (None, None) => vec![],
            (Some(recv), None) => vec![(recv.inner.as_name().unwrap(), None)],

            (recv, Some(params)) => {
                let mut parameters = Vec::with_capacity(params.len() + 1);

                if let Some(reciever) = recv {
                    parameters.push((reciever.inner.as_name().unwrap(), None));
                }

                for (name, annotation) in params.iter() {
                    let ann = annotation.as_ref().map(|ann| ann.visit_with(self));
                    parameters.push((name.clone(), ann));
                }

                parameters
            }
        };

        let returns = fndef.returns.as_ref().map(|r| r.visit_with(self));

        let sequence_id =
            self.with_new_sequence(fndef.body.len(), SequenceType::Function, |this| {
                for node in fndef.body.iter() {
                    node.visit_with(this);
                }
            });

        let name = fndef.name.inner.as_name().unwrap();
        let func = self.inst(RawInst::Defn {
            name: name,
            params: params,
            returns,
            sequence_id,
        });

        self.inst(RawInst::SetVar {
            variable: name,
            value: func,
        });

        // for dec in decorators {
        //     func = self.inst(RawInst::Call {
        //         callable: dec,
        //         arguments: vec![func],
        //     });
        // }

        func
    }

    fn visit_ifstmt(&mut self, ifch: &IfChain, _: Option<Span>) -> usize {
        let branch_indices: Vec<_> = ifch
            .branches
            .iter()
            .enumerate()
            .map(|(ix, branch)| {
                let test = branch.inner.test.visit_with(self);

                let bool_dunder = self.inst(RawInst::GetDunder {
                    object: test,
                    dunder: Dunder::AsBool,
                });

                let test = self.inst(RawInst::Call {
                    callable: bool_dunder,
                    arguments: vec![test],
                });

                let br = self.inst(RawInst::If {
                    test,
                    truthy: Some(INVALID_VALUE),
                    falsey: None,
                });

                if ix != ifch.branches.len() - 1 {
                    self.inst(RawInst::JumpTarget);
                }

                br
            })
            .collect();

        let or_else_branch = ifch.orelse.as_ref().map(|_| {
            let true_const = self.inst(RawInst::Const(Const::Bool(true)));

            let br = self.inst(RawInst::If {
                test: true_const,
                truthy: Some(INVALID_VALUE),
                falsey: None,
            });

            br
        });

        let body_indices: Vec<_> = ifch
            .branches
            .iter()
            .map(|branch| {
                let body_entry = self.inst(RawInst::JumpTarget);

                for node in branch.inner.body.iter() {
                    node.visit_with(self);
                }

                let backedge = self.inst(RawInst::Br { to: INVALID_VALUE });

                (body_entry, backedge)
            })
            .collect();

        if let Some(or_else_branch) = or_else_branch {
            self.inst(RawInst::Nop);

            for node in ifch.orelse.as_ref().unwrap() {
                node.visit_with(self);
            }

            let inst = self
                .sequences
                .get_mut(self.sequence_index)
                .unwrap()
                .inst
                .get_mut(or_else_branch)
                .unwrap();

            if let RawInst::If { truthy, .. } = &mut inst.op {
                truthy.replace(or_else_branch);
            } else {
                unreachable!();
            }
        }

        let after_if_ch = self.inst(RawInst::JumpTarget);

        {
            let seq = self.sequences.get_mut(self.sequence_index).unwrap();

            for (branch, (body, body_tail)) in branch_indices.iter().zip(body_indices.iter()) {
                let inst = seq.inst.get_mut(*branch).unwrap();

                if let RawInst::If { truthy, .. } = &mut inst.op {
                    truthy.replace(*body);
                } else {
                    unreachable!();
                }

                let inst = seq.inst.get_mut(*body_tail).unwrap();

                if let RawInst::Br { to } = &mut inst.op {
                    (*to) = after_if_ch;
                } else {
                    unreachable!();
                }
            }
        }

        INVALID_VALUE
    }

    fn visit_assign(&mut self, asn: &Assign, _: Option<Span>) -> usize {
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

    fn visit_return(&mut self, ret: &Return, _: Option<Span>) -> usize {
        let rv = match &ret.value {
            Ok(expr) => expr.visit_with(self),
            Err(_) => self.inst(RawInst::Const(Const::None)),
        };

        self.inst(RawInst::Return { value: rv })
    }

    fn visit_while(&mut self, while_: &While, _: Option<Span>) -> usize {
        let start = self.inst(RawInst::JumpTarget);
        let test = while_.test.inner.visit_with(self);

        let jump = self.inst(RawInst::If {
            test,
            truthy: None,
            falsey: None,
        });

        self.inst(RawInst::JumpTarget);

        for node in &while_.body {
            node.visit_with(self);
        }

        self.inst(RawInst::Br { to: start });
        let or_else = self.inst(RawInst::JumpTarget);

        let seq = self.sequences.get_mut(self.sequence_index).unwrap();

        seq.inst[jump].op = RawInst::If {
            test,
            truthy: None,
            falsey: Some(or_else),
        };

        INVALID_VALUE
    }

    fn visit_binop(&mut self, expr: &Expr, _: Option<Span>) -> usize {
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

    fn visit_unary(&mut self, unary: &Expr, _: Option<Span>) -> usize {
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

    fn visit_ternary(&mut self, ternary: &Expr, _: Option<Span>) -> usize {
        let (test, body, orelse) =
            patma!((test, body, orelse), Expr::If {test, body, orelse} in ternary).unwrap();

        // generate the ternary test
        let test = test.visit_with(self);

        let bool_dunder = self.inst(RawInst::GetDunder {
            object: test,
            dunder: Dunder::AsBool,
        });

        let test = self.inst(RawInst::Call {
            callable: bool_dunder,
            arguments: vec![test],
        });

        // emit a dummy nop that will be overwritten later with an If.
        let test_jump = self.inst(RawInst::Nop);
        self.inst(RawInst::JumpTarget);

        let true_value = body.visit_with(self);
        let after_value_jump_true = self.inst(RawInst::Br { to: INVALID_VALUE });

        // jump target by the jump after the test.
        let false_header = self.inst(RawInst::JumpTarget);

        let false_value = orelse.visit_with(self);
        let after_value_jump_false = self.inst(RawInst::Br { to: INVALID_VALUE });

        // At this point the sequence layout is:
        //
        //                                                                  +-----------------------------------+
        //                                                                 /                                     \
        //   <test> -> <test_jump> -[true]-> <true_value> -> <true_jump> -/   +-> <false_value> -> <false_jump> -+-> PhiRecv(φ)
        //                         \                                         /
        //                          ---------------[false]------------------/
        //
        // Now we emit a "PhiRecv" which is like an LLVM `phi` instruction
        // but is only used as a jump target for "PhiJump" instructions.
        //
        let phi_recv = self.inst(RawInst::PhiRecv);

        // Time to patch the dummy instructions / jump targets.
        let seq = self.sequences.get_mut(self.sequence_index).unwrap();

        // The test branch nop's into the true code but branches into the false code.
        seq.inst[test_jump].op = RawInst::If {
            test,
            truthy: None,
            falsey: Some(false_header),
        };

        // forward the true value into the reciever.
        seq.inst[after_value_jump_true].op = RawInst::PhiJump {
            recv: phi_recv,
            value: true_value,
        };

        // forward the false value into the reciever.
        seq.inst[after_value_jump_false].op = RawInst::PhiJump {
            recv: phi_recv,
            value: false_value,
        };

        phi_recv
    }

    fn visit_named_expr(&mut self, expr: &Expr, _: Option<Span>) -> usize {
        let (target, value) =
            patma!((target, value), Expr::Named { target, value } in expr).unwrap();

        let variable = patma!(name, Atom::Name(name) in target.inner).unwrap();
        let value = value.visit_with(self);

        self.inst(RawInst::SetVar { variable, value });

        value
    }

    fn visit_call(&mut self, call: &Primary, _: Option<Span>) -> usize {
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

    fn visit_subscript(&mut self, subscr: &Primary, _: Option<Span>) -> usize {
        let (value, index) =
            patma!((value, index), Primary::Subscript { value, index } in subscr).unwrap();

        let object = value.visit_with(self);
        let index = index.visit_with(self);

        let get_item = self.inst(RawInst::GetDunder {
            object,
            dunder: Dunder::GetItem,
        });

        self.inst(RawInst::Call {
            callable: get_item,
            arguments: vec![object, index],
        })
    }
}
