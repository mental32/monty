use montyc_core::ast::Constant;
use montyc_core::{patma, Span};

use montyc_parser::ast::{
    Annotation, Assign, Atom, ClassDef, Expr, FunctionDef, IfChain, Import, Primary, Return, While,
};
use montyc_parser::{AstNode, AstObject, AstVisitor};

use crate::SequenceType;

use super::raw_inst::{Dunder, RawInst};
use super::{FlatCode, INVALID_VALUE};

fn visit_const(this: &mut FlatCode, node: &Atom, span: Option<Span>) -> usize {
    let const_v = match node {
        Atom::None => Constant::None,
        Atom::Ellipsis => Constant::Ellipsis,
        Atom::Int(n) => Constant::Int(*n),
        Atom::Str(s) => Constant::String(*s),
        Atom::Bool(b) => Constant::Bool(*b),
        Atom::Float(f) => Constant::Float(*f),
        _ => unreachable!(),
    };

    let const_v = this.inst(RawInst::Const(const_v));

    if let Some(span) = span {
        this.set_span_for_values([const_v], span);
    }

    const_v
}

impl AstVisitor<usize> for FlatCode {
    fn visit_any(&mut self, o: &dyn AstObject) -> usize {
        match o.into_ast_node() {
            AstNode::Comment(_) => self.visit_pass(),
            node => todo!("{:#?}", node),
        }
    }

    fn visit_funcdef(&mut self, fndef: &FunctionDef, span: Option<Span>) -> usize {
        let decorators: Vec<usize> = fndef
            .decorator_list
            .iter()
            .rev()
            .map(|dec| dec.visit_with(self, None))
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
                    let ann = annotation.as_ref().map(|ann| ann.visit_with(self, None));
                    parameters.push((name.clone(), ann));
                }

                parameters
            }
        };

        let returns = fndef.returns.as_ref().map(|r| r.visit_with(self, None));

        let sequence_id = self.with_new_sequence(
            fndef.body.len(),
            SequenceType::Function,
            (self.mref(), span.unwrap()),
            |this| {
                for node in fndef.body.iter() {
                    node.visit_with(this, None);
                }
            },
        );

        self.sequences[sequence_id]
            .ast
            .replace(fndef.into_ast_node());

        let name = fndef.name.inner.as_name().unwrap();
        let mut func = self.inst(RawInst::Defn {
            name: name,
            params: params,
            returns,
            sequence_id,
        });

        self.inst(RawInst::SetVar {
            variable: name,
            value: func,
        });

        for dec in decorators {
            func = self.inst(RawInst::Call {
                callable: dec,
                arguments: vec![func],
            });
        }

        func
    }

    fn visit_expr(&mut self, expr: &Expr, span: Option<Span>) -> usize {
        let value = expr.visit_with(self, None);

        if let Some(span) = span {
            if self.sequences[self.sequence_index].inst[value]
                .attrs
                .span
                .is_none()
            {
                self.set_span_for_values([value], span);
            }
        }

        value
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

    fn visit_name(&mut self, node: &Atom, span: Option<Span>) -> usize {
        let variable = patma!(name.clone(), Atom::Name(name) in node).unwrap();
        let value = self.inst(RawInst::UseVar { variable });

        if let Some(span) = span {
            self.set_span_for_values([value], span);
        }

        value
    }

    fn visit_tuple(&mut self, node: &Atom, span: Option<Span>) -> usize {
        let inner = patma!(name.clone(), Atom::Tuple(name) in node).unwrap();
        let inner: Vec<_> = inner
            .iter()
            .map(|elem| elem.visit_with(self, None))
            .collect();

        let value = self.inst(RawInst::Tuple(inner.into_boxed_slice()));

        if let Some(span) = span {
            self.set_span_for_values([value], span);
        }

        value
    }

    // -- other visitors

    fn visit_ellipsis(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_bool(&mut self, node: &Atom, span: Option<Span>) -> usize {
        visit_const(self, node, span)
    }

    fn visit_import(&mut self, import: &Import, span: Option<Span>) -> usize {
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

                    let set_var = self.inst(RawInst::SetVar {
                        variable: root,
                        value,
                    });

                    if let Some(span) = span.clone() {
                        self.set_span_for_values([value, set_var], span);
                    }
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
                    let attr = self.inst(RawInst::RefAsStr { r: name });

                    let attr = self.inst(RawInst::GetAttribute {
                        object: module,
                        attr,
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

    fn visit_classdef(&mut self, classdef: &ClassDef, span: Option<Span>) -> usize {
        let name = classdef.name.inner.as_name().unwrap();
        let class = self.inst(RawInst::Class { name });

        let set_var = self.inst(RawInst::SetVar {
            variable: name,
            value: class,
        });

        if let Some(span) = span.clone() {
            self.set_span_for_values([set_var], span);
        }

        let mut body = classdef.body.iter().peekable();

        if let Some(node) = body.peek() {
            let span = node.span.clone();
            let node = node.inner.into_ast_node();

            if let AstNode::Str(string) = node {
                let string = string.visit_with(self, None);

                let dunder = self.inst(RawInst::SetDunder {
                    object: class,
                    dunder: Dunder::DocComment,
                    value: string,
                });

                self.set_span_for_values([dunder], span);

                let _ = body.next();
            }
        }

        let span = (self.mref(), span.unwrap_or_default());
        let class_body = self.with_new_sequence(
            classdef.body.len(),
            SequenceType::Class,
            span,
            move |this| {
                let _start = this.inst(RawInst::Nop);

                for node in body {
                    let span = node.span.clone();

                    match node.inner.into_ast_node() {
                        AstNode::FuncDef(def) => {
                            let function = def.visit_with(this, None);

                            let setter =
                                this.sequences[this.sequence_index].inst.last_mut().unwrap();

                            setter.op = RawInst::SetVar {
                                variable: def.name.inner.as_name().unwrap(),
                                value: function,
                            };

                            this.set_span_for_values([function], span);
                        }

                        AstNode::Assign(assign) => {
                            let assignment = assign.visit_with(this, None);
                            let varname = assign.name.inner.as_name().unwrap();

                            let setter = this.inst(RawInst::SetVar {
                                variable: varname,
                                value: assignment,
                            });

                            this.set_span_for_values([assignment, setter], span);
                        }

                        _ => {
                            let val = node.visit_with(this, None);

                            this.set_span_for_values([val], span);
                        }
                    }
                }
            },
        );

        self.inst(RawInst::BuildClass {
            sequence: class_body,
            class,
        });

        class
    }

    fn visit_ifstmt(&mut self, ifch: &IfChain, _: Option<Span>) -> usize {
        let branch_indices: Vec<_> = ifch
            .branches
            .iter()
            .enumerate()
            .map(|(_, branch)| {
                let entry = if matches!(self.last_inst(), Some(crate::FlatInst { .. })) {
                    self.sequences[self.sequence_index].inst.len() - 1
                } else {
                    self.inst(RawInst::JumpTarget)
                };

                let test = branch.inner.test.visit_with(self, None);

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
                    falsey: Some(INVALID_VALUE),
                });

                (br, entry)
            })
            .collect();

        let body_indices: Vec<_> = ifch
            .branches
            .iter()
            .map(|branch| {
                let body_entry = self.inst(RawInst::JumpTarget);

                for node in branch.inner.body.iter() {
                    node.visit_with(self, None);
                }

                let backedge = self.inst(RawInst::Br { to: INVALID_VALUE });

                (body_entry, backedge)
            })
            .collect();

        let or_else_br_tail = if let Some(_) = ifch.orelse.as_ref() {
            let or_else_body = self.inst(RawInst::JumpTarget);

            let seq = self.sequences.get_mut(self.sequence_index).unwrap();

            // set the falsey destination of the previous branch to point to
            if let Some(RawInst::If { falsey, .. }) = branch_indices
                .last()
                .and_then(|(inst_ix, _)| seq.inst.get_mut(*inst_ix))
                .map(|inst| &mut inst.op)
            {
                falsey.replace(or_else_body);
            }

            for node in ifch.orelse.as_ref().unwrap() {
                node.visit_with(self, None);
            }

            self.inst(RawInst::Br { to: INVALID_VALUE })
        } else {
            INVALID_VALUE
        };

        let after_if_ch = self.inst(RawInst::JumpTarget);

        {
            let seq = self.sequences.get_mut(self.sequence_index).unwrap();

            for ((branch_ix, (branch, branch_entry)), (body, body_tail)) in
                branch_indices.iter().enumerate().zip(body_indices.iter())
            {
                // set the falsey destination of the previous branch to point to
                if let Some(RawInst::If { falsey, .. }) = branch_ix
                    .checked_sub(1)
                    .and_then(|ix| branch_indices.get(ix))
                    .and_then(|(inst_ix, _)| seq.inst.get_mut(*inst_ix))
                    .map(|inst| &mut inst.op)
                {
                    falsey.replace(*branch_entry);
                }

                let inst = seq.inst.get_mut(*branch).unwrap();

                if let RawInst::If { truthy, falsey, .. } = &mut inst.op {
                    truthy.replace(*body);
                    falsey.replace(after_if_ch);
                } else {
                    unreachable!();
                }

                let inst = seq.inst.get_mut(*body_tail).unwrap();

                if let RawInst::Br { to } = &mut inst.op {
                    *to = after_if_ch;
                } else {
                    unreachable!();
                }
            }

            if or_else_br_tail != INVALID_VALUE {
                let inst = self
                    .sequences
                    .get_mut(self.sequence_index)
                    .unwrap()
                    .inst
                    .get_mut(or_else_br_tail)
                    .unwrap();

                if let RawInst::Br { to, .. } = &mut inst.op {
                    *to = after_if_ch;
                } else {
                    unreachable!();
                }
            }
        }

        INVALID_VALUE
    }

    fn visit_pass(&mut self) -> usize {
        self.inst(RawInst::Nop)
    }

    fn visit_assign(&mut self, asn: &Assign, span: Option<Span>) -> usize {
        match &asn.name.inner {
            Primary::Atomic(atom) => {
                let variable = patma!(name, Atom::Name(name) in atom.inner).unwrap();
                let value = asn.value.visit_with(self, None);

                self.inst(RawInst::SetVar { variable, value })
            }

            Primary::Await(_) | Primary::Call { .. } => unreachable!(),

            Primary::Subscript { .. } => todo!(),

            Primary::Attribute { left, attr } => {
                let object = left.visit_with(self, None);

                let r = attr.inner.as_name().unwrap();
                let attr_span = attr.span.clone();
                let attr = self.inst(RawInst::RefAsStr { r });

                self.set_span_for_values([attr], attr_span);

                let value = asn.value.visit_with(self, None);

                let ret = self.inst(RawInst::SetAttribute {
                    object,
                    attr,
                    value,
                });

                if let Some(span) = span {
                    self.set_span_for_values([ret], span);
                }

                ret
            }
        }
    }

    fn visit_return(&mut self, ret: &Return, span: Option<Span>) -> usize {
        let rv = match &ret.value {
            Ok(expr) => expr.visit_with(self, None),
            Err(_) => self.inst(RawInst::Const(Constant::None)),
        };

        let ret = self.inst(RawInst::Return { value: rv });

        if let Some(span) = span {
            self.set_span_for_values([ret], span);
        }

        ret
    }

    fn visit_while(&mut self, while_: &While, _: Option<Span>) -> usize {
        let to_start = self.inst(RawInst::JumpTarget);
        let start = self.inst(RawInst::JumpTarget);

        self.sequences.get_mut(self.sequence_index).unwrap().inst[to_start].op =
            RawInst::Br { to: start };

        let test = while_.test.inner.visit_with(self, None);

        let jump = self.inst(RawInst::If {
            test,
            truthy: None,
            falsey: None,
        });

        let when_true = self.inst(RawInst::JumpTarget);

        for node in &while_.body {
            node.visit_with(self, None);
        }

        self.inst(RawInst::Br { to: start });
        let or_else = self.inst(RawInst::JumpTarget);

        let seq = self.sequences.get_mut(self.sequence_index).unwrap();

        seq.inst[jump].op = RawInst::If {
            test,
            truthy: Some(when_true),
            falsey: Some(or_else),
        };

        INVALID_VALUE
    }

    fn visit_binop(&mut self, expr: &Expr, _: Option<Span>) -> usize {
        let (left, op, right) =
            patma!((left, op, right), Expr::BinOp { left, op, right } in expr).unwrap();

        let left_value = left.visit_with(self, None);
        let right_value = right.visit_with(self, None);

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

        let value = value.visit_with(self, None);

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
        let test = test.visit_with(self, None);

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

        let true_value = body.visit_with(self, None);
        let after_value_jump_true = self.inst(RawInst::Br { to: INVALID_VALUE });

        // jump target by the jump after the test.
        let false_header = self.inst(RawInst::JumpTarget);

        let false_value = orelse.visit_with(self, None);
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

    fn visit_named_expr(&mut self, expr: &Expr, span: Option<Span>) -> usize {
        let (target, value) =
            patma!((target, value), Expr::Named { target, value } in expr).unwrap();

        let variable = patma!(name, Atom::Name(name) in target.inner).unwrap();
        let value = value.visit_with(self, None);

        let rv = self.inst(RawInst::SetVar { variable, value });

        if let Some(span) = span {
            self.set_span_for_values([rv], span);
        }

        value
    }

    fn visit_call(&mut self, call: &Primary, span: Option<Span>) -> usize {
        let (func, args) = patma!((func, args), Primary::Call { func, args } in call).unwrap();

        let callable = func.visit_with(self, None);
        let arguments = match args {
            Some(args) => args.iter().map(|arg| arg.visit_with(self, None)).collect(),
            None => vec![],
        };

        let rv = self.inst(RawInst::Call {
            callable,
            arguments,
        });

        if let Some(span) = span {
            self.set_span_for_values([rv], span);
        }

        rv
    }

    fn visit_subscript(&mut self, subscr: &Primary, _: Option<Span>) -> usize {
        let (value, index) =
            patma!((value, index), Primary::Subscript { value, index } in subscr).unwrap();

        let object = value.visit_with(self, None);
        let index = index.visit_with(self, None);

        let get_item = self.inst(RawInst::GetDunder {
            object,
            dunder: Dunder::GetItem,
        });

        self.inst(RawInst::Call {
            callable: get_item,
            arguments: vec![object, index],
        })
    }

    fn visit_attr(&mut self, attr: &Primary, _span: Option<Span>) -> usize {
        let (base, attr) = patma!((left, attr), Primary::Attribute { left, attr } in attr).unwrap();

        let object = base.visit_with(self, None);

        let r = attr.inner.as_name().unwrap();
        let attr = self.inst(RawInst::RefAsStr { r });

        self.inst(RawInst::GetAttribute { object, attr })
    }

    fn visit_module(&mut self, module: &montyc_parser::ast::Module, _: Option<Span>) -> usize {
        for stmt in &module.body {
            stmt.visit_with(self, None);
        }

        INVALID_VALUE
    }

    fn visit_annotation(
        &mut self,
        ann: &montyc_parser::ast::Annotation,
        _span: Option<Span>,
    ) -> usize {
        let Annotation { name, kind } = ann;

        let name = match name.inner.as_name() {
            Some(sref) => sref,
            None => todo!(),
        };

        let annotation = kind.visit_with(self, None);

        self.inst(RawInst::SetAnnotation { name, annotation })
    }
}
