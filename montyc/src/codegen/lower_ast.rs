use std::{collections::HashSet, rc::Rc};

use cranelift_codegen::ir::{
    condcodes::IntCC, ExtFuncData, ExternalName, GlobalValueData, StackSlotData, StackSlotKind,
    TrapCode,
};

use crate::{
    ast::{
        atom::{Atom, StringRef},
        expr::{Expr, InfixOp},
        primary::Primary,
        stmt::Statement,
    },
    prelude::*,
};

use super::context::{CodegenContext, CodegenLowerArg};

impl<'long, 'short, 'fx> LowerWith<CodegenLowerArg<'long, 'short, 'fx>, cranelift_codegen::ir::Value> for Atom {
    fn lower_with(&self, (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>) -> cranelift_codegen::ir::Value {
        use cranelift_codegen::ir::InstBuilder;

        match self {
            Atom::None => todo!(),
            Atom::Ellipsis => todo!(),
            Atom::Int(n) => builder.ins().iconst(
                cranelift_codegen::ir::types::I64,
                cranelift_codegen::ir::immediates::Imm64::new(*n as i64),
            ),
            Atom::Str(n) => {
                let gv = ctx
                    .codegen_backend
                    .strings
                    .get(&StringRef(*n))
                    .and_then(|data| {
                        builder
                            .func
                            .global_values
                            .iter()
                            .find_map(|(gv, gvd)| match gvd {
                                GlobalValueData::Symbol {
                                    name: ExternalName::User { index, .. },
                                    ..
                                } if *index == data.as_u32() => Some(gv),
                                _ => None,
                            })
                    })
                    .unwrap();

                builder
                    .ins()
                    .global_value(cranelift_codegen::ir::types::I64, gv)
            }

            Atom::Bool(b) => builder
                .ins()
                .iconst(ctx.codegen_backend.types[&TypeMap::INTEGER], *b as i64),

            Atom::Float(_) => todo!(),
            Atom::Comment(_) => unreachable!(),
            Atom::Name(n) => {
                let ss = ctx.vars.get(&n).unwrap();
                let (ty, _) = ctx.func.vars.get(n).map(|r| r.value().clone()).unwrap();

                let ty = ctx.codegen_backend.types[&ty];

                builder.ins().stack_load(ty, ss.clone(), 0)
            }
        }
    }
}

impl<'long, 'short, 'fx> LowerWith<CodegenLowerArg<'long, 'short, 'fx>, Option<bool>> for Statement {
    fn lower_with(&self, (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>) -> Option<bool> {
        use cranelift_codegen::ir::InstBuilder;

        match self {
            Statement::Expression(e) => {
                let _ = e.lower_with((ctx.clone(), builder));
            }

            Statement::FnDef(_) => todo!(),

            Statement::Ret(r) => {
                if let Some(e) = &r.value {
                    let mut value = e.inner.lower_with((ctx.clone(), builder));

                    let mref = ctx.func.scope.module_ref();

                    let v_ty = ctx
                        .codegen_backend
                        .global_context
                        .database
                        .type_of(&(Rc::clone(e) as Rc<_>), Some(&mref))
                        .unwrap();

                    if v_ty != ctx.func.kind.inner.ret {
                        value = ctx
                            .codegen_backend
                            .global_context
                            .type_map
                            .coerce(v_ty, ctx.func.kind.inner.ret)
                            .unwrap()((ctx.clone(), builder), value);
                    }

                    builder.ins().return_(&[value]);
                } else {
                    builder.ins().return_(&[]);
                };

                return Some(false); // this sets "implicit return" to false in codegen.
            }

            Statement::Asn(asn) => {
                let ss = ctx
                    .vars
                    .get(&asn.name.name().expect("atom name"))
                    .expect("unset var")
                    .clone();
                let mref = ctx.func.scope.module_ref();

                let value = if let Some(ann) = &asn.kind {
                    let value_ty = ctx
                        .codegen_backend
                        .global_context
                        .database
                        .type_of(&(Rc::clone(&asn.value) as Rc<_>), Some(&mref))
                        .unwrap();
                    let kind_ty = ctx
                        .codegen_backend
                        .global_context
                        .database
                        .type_of(&(Rc::clone(&ann) as Rc<_>), Some(&mref))
                        .unwrap();

                    let rule = ctx
                        .codegen_backend
                        .global_context
                        .type_map
                        .coerce(value_ty, kind_ty)
                        .unwrap();

                    let value = asn.value.inner.lower_with((ctx.clone(), builder));

                    rule((ctx.clone(), builder), value)
                } else {
                    asn.value.inner.lower_with((ctx.clone(), builder))
                };

                builder.ins().stack_store(value, ss, 0);
            }

            Statement::Import(_) => todo!(),
            Statement::Class(_) => todo!(),
            Statement::Pass => {
                builder.ins().nop();
            }

            Statement::If(ifstmt) => {
                let global_escape_block = builder.create_block();

                let branch_blocks: Vec<_> = ifstmt
                    .branches
                    .iter()
                    .map(|_| {
                        (
                            builder.create_block(),
                            builder.create_block(),
                            builder.create_block(),
                        ) // (head, body, escape)
                    })
                    .collect();

                let mut implicit_return = Some(true);

                let mut block_escapes = (0..branch_blocks.len()).collect::<HashSet<_>>();

                for (branch_blocks_idx, ifstmt) in ifstmt.branches.iter().enumerate() {
                    let (head_block, body_block, local_escape_block) =
                        branch_blocks[branch_blocks_idx];

                    builder.ins().jump(head_block, &[]);

                    builder.switch_to_block(head_block);

                    let cc = ifstmt.inner.test.inner.lower_with((ctx.clone(), builder));

                    builder.ins().brnz(cc, body_block, &[]);
                    builder.ins().jump(local_escape_block, &[]);

                    {
                        builder.switch_to_block(body_block);

                        for part in ifstmt.inner.body.iter() {
                            if let Some(false) = part.inner.lower_with((ctx.clone(), builder)) {
                                implicit_return.replace(false);
                                block_escapes.remove(&branch_blocks_idx);
                            }
                        }

                        if !builder.is_filled() {
                            builder.ins().jump(global_escape_block, &[]);
                        }
                    }

                    builder.switch_to_block(local_escape_block);
                }

                let mut orelse_escapes = true;
                let orelse = builder.create_block();

                builder.ins().jump(orelse, &[]);

                builder.switch_to_block(orelse);

                if let Some(orelse) = &ifstmt.orelse {
                    for stmt in orelse {
                        if let Some(false) = stmt.inner.lower_with((ctx.clone(), builder)) {
                            implicit_return.replace(false);
                            orelse_escapes = false;
                        }
                    }
                }

                if !builder.is_filled() {
                    builder.ins().jump(global_escape_block, &[]);
                }

                builder.switch_to_block(global_escape_block);

                if block_escapes.is_empty() && !orelse_escapes {
                    builder.ins().trap(TrapCode::UnreachableCodeReached);
                }

                return implicit_return;
            }

            Statement::While(w) => {
                let (head, body, escape) = {
                    (
                        builder.create_block(),
                        builder.create_block(),
                        builder.create_block(),
                    )
                };

                builder.ins().jump(head, &[]);

                builder.switch_to_block(head);

                let cc = w.test.inner.lower_with((ctx.clone(), builder));

                builder.ins().brnz(cc, body, &[]);
                builder.ins().jump(escape, &[]);

                builder.switch_to_block(body);

                let mut ret = None;

                for part in w.body.iter() {
                    ret = part.inner.lower_with((ctx.clone(), builder));
                }

                if !builder.is_filled() {
                    builder.ins().jump(head, &[]);
                    builder.switch_to_block(escape);
                } else {
                    builder.switch_to_block(escape);
                    builder.ins().jump(head, &[]);
                }

                return ret;
            }
        }

        None
    }
}

impl<'long, 'short, 'fx> LowerWith<CodegenLowerArg<'long, 'short, 'fx>, cranelift_codegen::ir::Value> for Primary {
    fn lower_with(&self, (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>) -> cranelift_codegen::ir::Value {
        use cranelift_codegen::ir::InstBuilder;

        #[allow(warnings)]
        match self {
            Primary::Atomic(at) => at.inner.lower_with((ctx.clone(), builder)),
            Primary::Subscript { value, index } => todo!(),
            Primary::Call { func, args } => {
                let func_name = match &func.inner {
                    Primary::Atomic(atom) => match atom.as_ref().inner {
                        Atom::Name(n) => n,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                let mref = ctx.func.scope.module_ref();

                let func_t = ctx
                    .codegen_backend
                    .global_context
                    .database
                    .type_of(&(Rc::clone(&func) as Rc<dyn AstObject>), Some(&mref))
                    .unwrap();
                let func_t = ctx
                    .codegen_backend
                    .global_context
                    .type_map
                    .get_tagged::<FunctionType>(func_t)
                    .unwrap()
                    .unwrap();

                let module = ctx
                    .codegen_backend
                    .names
                    .get(&ctx.func.scope.module_ref())
                    .unwrap();

                let (target_name, target_fid) = module.functions.get(&func_name).unwrap();

                let args = match args {
                    Some(args) => args
                        .iter()
                        .cloned()
                        .zip(func_t.inner.args.iter().cloned())
                        .map(|(arg, param_t)| {
                            let arg_t = ctx
                                .codegen_backend
                                .global_context
                                .database
                                .type_of(&(Rc::clone(&arg) as Rc<dyn AstObject>), Some(&mref))
                                .unwrap();

                            let mut value = arg.inner.lower_with((ctx.clone(), builder));

                            if arg_t != param_t {
                                value = ctx
                                    .codegen_backend
                                    .global_context
                                    .type_map
                                    .coerce(arg_t, param_t)
                                    .unwrap()(
                                        (ctx.clone(), builder), value
                                )
                            }

                            value
                        })
                        .collect(),

                    None => vec![],
                };

                let func_ref = if let Some(signature) =
                    ctx.codegen_backend.external_functions.get(target_fid)
                {
                    let sigref = builder.import_signature(signature.clone());

                    builder.import_function(ExtFuncData {
                        name: target_name.clone(),
                        signature: sigref,
                        colocated: false,
                    })
                } else {
                    cranelift_module::Module::declare_func_in_func(
                        &*ctx.codegen_backend.object_module.borrow(),
                        *target_fid,
                        &mut builder.func,
                    )
                };

                let result = builder.ins().call(func_ref, args.as_slice());

                let mut builder = builder;

                builder
                    .inst_results(result)
                    .first()
                    .cloned()
                    .unwrap_or_else(|| builder.ins().iconst(cranelift_codegen::ir::types::I64, 0))
            }

            Primary::Attribute { left, attr } => todo!(),
            Primary::Await(_) => todo!(),
        }
    }
}

impl<'long, 'short, 'fx> LowerWith<CodegenLowerArg<'long, 'short, 'fx>, cranelift_codegen::ir::Value> for Expr {
    fn lower_with(&self, (ctx, builder): CodegenLowerArg<'long, 'short, 'fx>) -> cranelift_codegen::ir::Value {
        use cranelift_codegen::ir::InstBuilder;

        match self {
            Expr::If { test, body, orelse } => {
                let body_ty = ctx
                    .codegen_backend
                    .global_context
                    .database
                    .type_of(&(Rc::clone(body) as Rc<_>), None)
                    .unwrap();
                let orelse_ty = ctx
                    .codegen_backend
                    .global_context
                    .database
                    .type_of(&(Rc::clone(orelse) as Rc<_>), None)
                    .unwrap();

                let ty = if body_ty != orelse_ty {
                    // create a union[left, right]
                    ctx.codegen_backend
                        .global_context
                        .type_map
                        .entry(TypeDescriptor::Generic(crate::typing::Generic::Union {
                            inner: vec![body_ty, orelse_ty],
                        }))
                } else {
                    body_ty
                };

                assert!(ctx.codegen_backend.types.contains_key(&ty));

                let escape_block = builder.create_block();
                let body_block = builder.create_block();
                let orelse_block = builder.create_block();

                let size = ctx
                    .codegen_backend
                    .global_context
                    .database
                    .size_of(
                        ctx.func.scope.module_ref(),
                        Rc::clone(&test) as Rc<_>,
                        &ctx.codegen_backend.global_context,
                    )
                    .unwrap()
                    .get();

                let ss = builder
                    .create_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        size as u32,
                    ));

                let cc = test.inner.lower_with((ctx.clone(), builder));

                builder.ins().brnz(cc, body_block, &[]);
                builder.ins().jump(orelse_block, &[]);

                builder.switch_to_block(body_block);
                let body_v = body.inner.lower_with((ctx.clone(), builder));

                builder.ins().stack_store(body_v, ss, 0);
                builder.ins().jump(escape_block, &[]);

                builder.switch_to_block(orelse_block);
                let orelse_v = orelse.inner.lower_with((ctx.clone(), builder));

                builder.ins().stack_store(orelse_v, ss, 0);
                builder.ins().jump(escape_block, &[]);

                builder.switch_to_block(escape_block);

                let ty = builder.func.dfg.value_type(body_v);

                builder.ins().stack_load(ty, ss, 0)
            }

            Expr::BinOp { left, op, right } => {
                let mref = ctx.func.scope.module_ref();

                let left_ty = ctx
                    .codegen_backend
                    .global_context
                    .database
                    .type_of(&(Rc::clone(left) as Rc<_>), Some(&mref))
                    .unwrap();

                let right_ty = ctx
                    .codegen_backend
                    .global_context
                    .database
                    .type_of(&(Rc::clone(left) as Rc<_>), Some(&mref))
                    .unwrap();

                if left_ty.is_builtin() && right_ty.is_builtin() {
                    let lvalue = left.inner.lower_with((ctx.clone(), builder));
                    let rvalue = right.inner.lower_with((ctx.clone(), builder));

                    match op {
                        InfixOp::Add => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                builder.ins().iadd(lvalue, rvalue)
                            }
                            _ => unreachable!(),
                        },

                        InfixOp::Sub => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                let ss = builder.create_stack_slot(
                                    cranelift_codegen::ir::stackslot::StackSlotData::new(
                                        StackSlotKind::ExplicitSlot,
                                        8,
                                    ),
                                );

                                let v0 = builder.ins().isub(lvalue, rvalue);
                                builder.ins().stack_store(v0, ss, 0);

                                let underflow_trap = builder.create_block();
                                let exit_block = builder.create_block();

                                builder.ins().br_icmp(
                                    IntCC::SignedGreaterThan,
                                    v0,
                                    lvalue,
                                    underflow_trap,
                                    &[],
                                );
                                builder.ins().jump(exit_block, &[]);

                                builder.switch_to_block(underflow_trap);

                                builder
                                    .ins()
                                    .trap(TrapCode::IntegerOverflow);

                                builder.switch_to_block(exit_block);
                                let v0 = builder.ins().stack_load(
                                    cranelift_codegen::ir::types::I64,
                                    ss,
                                    0,
                                );

                                v0
                            }
                            _ => unreachable!(),
                        },

                        InfixOp::Power => todo!(),
                        InfixOp::Invert => todo!(),
                        InfixOp::FloorDiv => todo!(),
                        InfixOp::MatMult => todo!(),
                        InfixOp::Mod => todo!(),
                        InfixOp::Div => todo!(),

                        InfixOp::Mult => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                builder.ins().imul(lvalue, rvalue)
                            }
                            _ => unreachable!(),
                        },

                        InfixOp::LeftShift => todo!(),
                        InfixOp::RightShift => todo!(),
                        InfixOp::NotEq => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) =>
                                builder
                                .ins()
                                .icmp(IntCC::NotEqual, rvalue, lvalue),

                            _ => unreachable!(),
                        },

                        InfixOp::Eq => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) =>
                                builder
                                .ins()
                                .icmp(IntCC::Equal, rvalue, lvalue),

                            _ => unreachable!(),
                        },

                        InfixOp::And => todo!(),
                        InfixOp::Or => todo!(),
                    }
                } else {
                    todo!()
                }
            }

            Expr::Unary { op, value } => todo!(),
            Expr::Named { target, value } => todo!(),
            Expr::Primary(p) => p.inner.lower_with((ctx.clone(), builder)),
        }
    }
}
