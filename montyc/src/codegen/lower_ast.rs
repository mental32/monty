#![allow(warnings)]

use std::{convert::TryFrom, rc::Rc};

use cranelift_codegen::ir::{self, condcodes::IntCC, ExtFuncData, TrapCode};
use cranelift_module::Module;

use crate::{
    ast::{
        assign::Assign,
        atom::{Atom, StringRef},
        class::ClassDef,
        expr::{Expr, InfixOp},
        primary::Primary,
        stmt::Statement,
    },
    codegen::{context::RValueAlloc, pointer::Pointer, storage::Storage, tvalue::TypedValue},
    prelude::*,
    typing::{Generic, TaggedType},
};

use super::{context::CodegenLowerArg, tvalue::TypePair, LowerCodegen, Value};

impl LowerCodegen for Atom {
    fn lower<'g>(&self, (ctx, builder): CodegenLowerArg<'g, '_, '_>) -> Option<Value<'g>> {
        use cranelift_codegen::ir::InstBuilder;

        match self {
            Atom::Tuple(elements) => {
                let mut inner = Vec::with_capacity(elements.len());
                let mref = ctx.func.scope.module_ref();

                for elem in elements.iter() {
                    let elem_t = ctx
                        .codegen_backend
                        .global_context
                        .database
                        .type_of(&(Rc::clone(elem) as Rc<_>), Some(&mref))
                        .unwrap();

                    inner.push(elem_t);
                }

                let kind = {
                    ctx.codegen_backend
                        .global_context
                        .type_map
                        .entry(TypeDescriptor::Generic(Generic::Struct { inner }))
                };

                let inner = if let Some(Ok(TaggedType {
                    inner: Generic::Struct { inner },
                    ..
                })) = ctx
                    .codegen_backend
                    .global_context
                    .type_map
                    .get_tagged::<Generic>(kind)
                {
                    inner
                } else {
                    unreachable!()
                };

                let elements = elements.clone();
                let (_, layout) = ctx.size_and_layout_of(TypePair(kind, None)).unwrap();

                let alloca = box move |(ctx, builder): CodegenLowerArg<'_, '_, '_>,
                                       storage: &Storage| {
                    {
                        let mut sbuf = storage.as_mut_struct((ctx.clone(), builder));

                        for (field, elem) in elements.iter().enumerate() {
                            let value = match elem.inner.lower((ctx.clone(), builder)).unwrap() {
                                Ok(v) => v,
                                Err(_) => todo!(),
                            };

                            sbuf.write(field, value.into_raw(builder), (ctx.clone(), builder));
                        }
                    }

                    let pointer = storage.as_ptr_value();

                    Some(pointer)
                };

                Some(Err((layout, alloca)))
            }

            Atom::None => todo!(),
            Atom::Ellipsis => todo!(),
            Atom::Int(n) => {
                let value = builder.ins().iconst(
                    ir::types::I64,
                    ir::immediates::Imm64::new(i64::try_from(*n).unwrap()),
                );

                Some(Ok(TypedValue::by_val(
                    value,
                    TypePair(TypeMap::INTEGER, Some(ir::types::I64)),
                )))
            }
            Atom::Str(n) => {
                let str_global_value = ctx
                    .get_str_data(StringRef(*n), &builder.func)
                    .expect("string literal should already be defined in the module.");

                let ptr = {
                    let ptr =
                        Pointer::new(builder.ins().global_value(ir::types::I64, str_global_value));

                    TypedValue::by_ref(ptr, TypePair(TypeMap::STRING, Some(ir::types::I64)))
                };

                Some(Ok(ptr))
            }

            Atom::Bool(b) => {
                let ir_ty = ctx.codegen_backend.scalar_type_of(TypeMap::BOOL);

                let value = builder.ins().bconst(ir_ty, *b);

                Some(Ok(TypedValue::by_val(
                    value,
                    TypePair(TypeMap::BOOL, Some(ir_ty)),
                )))
            }

            Atom::Float(_) => todo!(),
            Atom::Comment(_) => unreachable!(),
            Atom::Name((n, _)) => {
                if ctx.vars.contains_key(&n) {
                    ctx.with_var_alloc(*n, |storage| Some(Ok(storage.as_ptr_value())))
                } else {
                    let def = self
                        .resolve_name_to_definition(
                            &ctx.func.scope,
                            &ctx.codegen_backend.global_context,
                        )
                        .expect(&format!("{:?}", self))
                        .unspanned();

                    if let Some(klass) =
                        crate::isinstance!(def.as_ref(), crate::ast::class::ClassDef)
                    {
                        let result = ctx
                            .codegen_backend
                            .global_context
                            .database
                            .query()
                            .filter(|object| !(object.is_named(*n) && crate::isinstance!(object, ClassDef).or_else(|| crate::isinstance!(object, Statement, Statement::Class(k) => k)).is_some()))
                            .into_iter()
                            .next()
                            .unwrap();

                        let type_id_original = ctx
                            .codegen_backend
                            .global_context
                            .type_map
                            .values()
                            .find_map(|(_, desc)| match desc {
                                TypeDescriptor::Class(c) if c.name == *n => Some(c.kind),
                                _ => None,
                            })
                            .unwrap();

                        let type_id = builder.ins().iconst(
                            ctx.codegen_backend.scalar_type_of(TypeMap::INTEGER),
                            type_id_original.as_usize() as i64,
                        );

                        Some(Ok(TypedValue::by_val(
                            type_id,
                            TypePair(
                                TypeMap::TYPE,
                                Some(ctx.codegen_backend.scalar_type_of(TypeMap::INTEGER)),
                            ),
                        )))
                    } else if let Some(asn) = crate::isinstance!(def.as_ref(), Assign).or_else(
                        || crate::isinstance!(def.as_ref(), Statement, Statement::Asn(a) => a),
                    ) {
                        let name = asn.name.inner.name().unwrap();
                        let mref = ctx.func.scope.module_ref();

                        let global = ctx.codegen_backend.globals.get(&(mref, name)).unwrap();

                        let value = ctx
                            .codegen_backend
                            .object_module
                            .borrow_mut()
                            .declare_data_in_func(global.data_id, &mut builder.func);
                        let value = builder.ins().global_value(ir::types::I64, value);

                        let ptr = Pointer::new(value);

                        Some(Ok(TypedValue::by_ref(
                            ptr,
                            TypePair(
                                global.type_id,
                                Some(ctx.codegen_backend.scalar_type_of(global.type_id)),
                            ),
                        )))
                    } else {
                        unimplemented!()
                    }
                }
            }
        }
    }
}

impl LowerCodegen for Primary {
    fn lower<'g>(&self, (ctx, builder): CodegenLowerArg<'g, '_, '_>) -> Option<Value<'g>> {
        use ir::InstBuilder;

        log::trace!("codegen:primary {:?}", self);

        match self {
            Primary::Atomic(atom) => return atom.inner.lower((ctx, builder)),

            Primary::Subscript { value, index } => {
                let value_t = ctx.type_of(value).unwrap();

                let value = match value.inner.lower((ctx.clone(), builder)) {
                    None => unreachable!(),
                    Some(Ok(_)) => todo!("subscript on scalar value."),
                    Some(Err((layout, f))) => {
                        let storage = Storage::new_stack_slot(
                            (ctx.clone(), builder),
                            TypePair(value_t, None),
                        );

                        let alloc_id = ctx.allocator.borrow_mut().alloc(storage);

                        let value_t_d = ctx
                            .codegen_backend
                            .global_context
                            .type_map
                            .get(value_t)
                            .unwrap();

                        let storage = ctx.allocator.borrow().get(alloc_id);

                        let _value = f((ctx.clone(), builder), &*storage).unwrap();

                        if let Expr::Primary(Spanned {
                            inner: Primary::Atomic(atom),
                            ..
                        }) = &index.inner
                        {
                            let n = match atom.inner {
                                Atom::Ellipsis
                                | Atom::Str(_)
                                | Atom::Tuple(_)
                                | Atom::Comment(_)
                                | Atom::Name(_)
                                | Atom::Float(_)
                                | Atom::None => {
                                    unreachable!();
                                }

                                Atom::Int(n) => n,
                                Atom::Bool(b) => b.then_some(1).unwrap_or(0),
                            };

                            let sbuf = storage.as_mut_struct((ctx.clone(), builder));

                            let idx = if n < 0 {
                                sbuf.field_count() - usize::try_from(-n).unwrap()
                            } else {
                                usize::try_from(n).unwrap()
                            };

                            let value = sbuf.read(idx, (ctx.clone(), builder)).unwrap();
                            let value_t = builder.func.dfg.value_type(value);

                            let type_id = match value_t_d.value() {
                                TypeDescriptor::Generic(Generic::Struct { inner }) => inner[idx],
                                _ => unreachable!(),
                            };

                            return Some(Ok(TypedValue::by_val(
                                value,
                                TypePair(type_id, Some(value_t)),
                            )));
                        }

                        todo!();
                    }
                };
            }

            Primary::Call { func, args } => {
                let (func_name, _) = match &func.inner {
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

                let mut args = match args {
                    Some(args) => args
                        .iter()
                        .zip(func_t.inner.args.iter().cloned())
                        .map(|(arg, param_t)| {
                            let value = match arg.inner.lower((ctx.clone(), builder)).unwrap() {
                                Ok(v) => v,
                                Err(_) => todo!(),
                            };

                            ctx.maybe_coerce(value, param_t, builder)
                        })
                        .collect(),

                    None => vec![],
                };

                let module = ctx
                    .codegen_backend
                    .names
                    .get(&ctx.func.scope.module_ref())
                    .unwrap();

                match module.functions.get(&func_name) {
                    Some((target_name, target_fid)) => {
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

                        let args: Vec<_> =
                            args.drain(..).map(|arg| arg.into_raw(builder)).collect();

                        let result = builder.ins().call(func_ref, args.as_slice());

                        let value = builder.inst_results(result).first().cloned().unwrap();

                        let ty = builder.func.dfg.value_type(value);

                        Some(Ok(TypedValue::by_val(
                            value,
                            TypePair(func_t.inner.ret, Some(ty)),
                        )))
                    }

                    None => {
                        if let Some((name, (f, builtin_func_handler))) = ctx
                            .codegen_backend
                            .global_context
                            .builtin_functions
                            .iter()
                            .find(|(n, (f, _))| {
                                **n == func_t.inner.name.0
                                    && ctx
                                        .codegen_backend
                                        .global_context
                                        .type_map
                                        .unify_func(f.kind.type_id, &func_t.inner)
                            })
                        {
                            (builtin_func_handler.0)((ctx.clone(), builder), &args).map(Ok)
                        } else {
                            todo!("uhhhh");
                        }
                    }
                }
            }

            Primary::Attribute { left, attr } => todo!(),
            Primary::Await(_) => unreachable!(),
        }
    }
}

impl LowerCodegen for Expr {
    fn lower<'g>(&self, (ctx, builder): CodegenLowerArg<'g, '_, '_>) -> Option<Value<'g>> {
        use ir::InstBuilder;

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

                let (ty, is_union) =
                    if body_ty != orelse_ty {
                        // create a union[left, right]
                        let ty = ctx.codegen_backend.global_context.type_map.entry(
                            TypeDescriptor::Generic(crate::typing::Generic::Union {
                                inner: vec![body_ty, orelse_ty],
                            }),
                        );

                        (ty, true)
                    } else {
                        (body_ty, false)
                    };

                if !is_union {
                    assert!(ctx.codegen_backend.types.contains_key(&ty));
                }

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

                let test = Rc::clone(test);
                let orelse = Rc::clone(orelse);
                let body = Rc::clone(body);

                let (_, layout) = ctx
                    .codegen_backend
                    .global_context
                    .type_map
                    .size_and_layout(ty)
                    .unwrap();

                if is_union {
                    let alloca: Box<RValueAlloc<'_>> = box move |(ctx, builder), storage| {
                        let escape_block = builder.create_block();
                        let body_block = builder.create_block();
                        let orelse_block = builder.create_block();

                        let cc = match test.inner.lower((ctx.clone(), builder)).unwrap() {
                            Ok(v) => v.into_raw(builder),
                            Err(_) => todo!(),
                        };

                        builder.ins().brnz(cc, body_block, &[]);
                        builder.ins().jump(orelse_block, &[]);

                        let sbuf = storage.as_mut_struct((ctx.clone(), builder));

                        builder.switch_to_block(body_block);
                        let body_v = match body.inner.lower((ctx.clone(), builder)).unwrap() {
                            Ok(v) => v,
                            Err(_) => todo!(),
                        };

                        let body_ty = ctx.type_of(&body).unwrap();
                        let body_ty = builder
                            .ins()
                            .iconst(ir::types::I64, body_ty.as_usize() as i64);

                        sbuf.write(0, body_ty, (ctx.clone(), builder));
                        sbuf.write(1, body_v.into_raw(builder), (ctx.clone(), builder));

                        builder.ins().jump(escape_block, &[]);

                        builder.switch_to_block(orelse_block);
                        let orelse_v = match orelse.inner.lower((ctx.clone(), builder)).unwrap() {
                            Ok(v) => v,
                            Err(_) => todo!(),
                        };

                        let orelse_ty = ctx.type_of(&orelse).unwrap();
                        let orelse_ty = builder
                            .ins()
                            .iconst(ir::types::I64, orelse_ty.as_usize() as i64);

                        sbuf.write(0, orelse_ty, (ctx.clone(), builder));
                        sbuf.write(1, orelse_v.into_raw(builder), (ctx.clone(), builder));

                        builder.ins().jump(escape_block, &[]);

                        builder.switch_to_block(escape_block);

                        Some(storage.as_ptr_value())
                    };

                    Some(Err((layout, alloca)))
                } else {
                    let escape_block = builder.create_block();
                    builder.append_block_param(
                        escape_block,
                        ctx.codegen_backend.scalar_type_of(body_ty),
                    );

                    let body_block = builder.create_block();
                    let orelse_block = builder.create_block();

                    let cc = match test.inner.lower((ctx.clone(), builder)).unwrap() {
                        Ok(v) => v.into_raw(builder),
                        Err(_) => todo!(),
                    };

                    builder.ins().brnz(cc, body_block, &[]);
                    builder.ins().jump(orelse_block, &[]);

                    builder.switch_to_block(body_block);
                    let body_v = match body.inner.lower((ctx.clone(), builder)).unwrap() {
                        Ok(v) => v,
                        Err(_) => todo!(),
                    };

                    let body_v_raw = body_v.clone().into_raw(builder);

                    builder.ins().jump(escape_block, &[body_v_raw]);

                    builder.switch_to_block(orelse_block);
                    let orelse_v = match orelse.inner.lower((ctx.clone(), builder)).unwrap() {
                        Ok(v) => v.into_raw(builder),
                        Err(_) => todo!(),
                    };

                    builder.ins().jump(escape_block, &[orelse_v]);

                    builder.switch_to_block(escape_block);

                    let ty = builder.func.dfg.value_type(body_v_raw);

                    let value = builder.block_params(escape_block)[0];

                    Some(Ok(TypedValue::by_val(value, TypePair(body_ty, Some(ty)))))
                }
            }

            Expr::BinOp { left, op, right } => {
                let left_t = ctx.type_of(left).unwrap();
                let right_t = ctx.type_of(right).unwrap();

                if left_t.is_builtin() && right_t.is_builtin() {
                    let left_v = match left.inner.lower((ctx.clone(), builder)).unwrap() {
                        Ok(v) => v,
                        Err(_) => todo!(),
                    };

                    let right_v = match right.inner.lower((ctx.clone(), builder)).unwrap() {
                        Ok(v) => v,
                        Err(_) => todo!(),
                    };

                    match op {
                        InfixOp::Add => match (left_t, right_t) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                let lvalue = left_v.clone().deref_into_raw(builder);
                                let rvalue = right_v.clone().deref_into_raw(builder);

                                let result = builder.ins().iadd(lvalue, rvalue);

                                let cc = builder.ins().ifcmp(result, lvalue);

                                builder.ins().trapif(
                                    IntCC::SignedLessThan,
                                    cc,
                                    TrapCode::IntegerOverflow,
                                );

                                let value = TypedValue::by_val(
                                    result,
                                    TypePair(TypeMap::INTEGER, Some(ir::types::I64)),
                                );

                                Some(Ok(value))
                            }

                            _ => todo!(),
                        },

                        InfixOp::Sub => match (left_t, right_t) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                let lvalue = left_v.clone().deref_into_raw(builder);
                                let rvalue = right_v.clone().deref_into_raw(builder);

                                let result = builder.ins().isub(lvalue, rvalue);

                                let cc = builder.ins().ifcmp(result, lvalue);

                                builder.ins().trapif(
                                    IntCC::SignedGreaterThan,
                                    cc,
                                    TrapCode::IntegerOverflow,
                                );

                                let value = TypedValue::by_val(
                                    result,
                                    TypePair(TypeMap::INTEGER, Some(ir::types::I64)),
                                );

                                Some(Ok(value))
                            }

                            _ => todo!(),
                        },

                        InfixOp::Mult => match (left_t, right_t) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                let lvalue = left_v.clone().deref_into_raw(builder);
                                let rvalue = right_v.clone().deref_into_raw(builder);

                                let result = builder.ins().imul(lvalue, rvalue);

                                let cc = builder.ins().ifcmp(result, lvalue);

                                builder.ins().trapif(
                                    IntCC::SignedLessThan,
                                    cc,
                                    TrapCode::IntegerOverflow,
                                );

                                let value = TypedValue::by_val(
                                    result,
                                    TypePair(TypeMap::INTEGER, Some(ir::types::I64)),
                                );

                                Some(Ok(value))
                            }

                            _ => todo!(),
                        },

                        InfixOp::Eq => match (left_t, right_t) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                let lvalue = left_v.clone().deref_into_raw(builder);
                                let rvalue = right_v.clone().deref_into_raw(builder);

                                let result = builder.ins().icmp(IntCC::Equal, lvalue, rvalue);

                                Some(Ok(TypedValue::by_val(
                                    result,
                                    TypePair(TypeMap::BOOL, Some(ir::types::B1)),
                                )))
                            }

                            (TypeMap::BOOL, TypeMap::BOOL) => {
                                let lvalue = left_v.clone().deref_into_raw(builder);
                                let lvalue = builder.ins().bint(ir::types::I64, lvalue);

                                let rvalue = right_v.clone().deref_into_raw(builder);
                                let rvalue = builder.ins().bint(ir::types::I64, rvalue);

                                let result = builder.ins().icmp(IntCC::Equal, lvalue, rvalue);

                                Some(Ok(TypedValue::by_val(
                                    result,
                                    TypePair(TypeMap::BOOL, Some(ir::types::B1)),
                                )))
                            }

                            _ => todo!(),
                        },

                        InfixOp::NotEq => match (left_t, right_t) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                let lvalue = left_v.clone().deref_into_raw(builder);
                                let rvalue = right_v.clone().deref_into_raw(builder);

                                let result = builder.ins().icmp(IntCC::NotEqual, lvalue, rvalue);

                                Some(Ok(TypedValue::by_val(
                                    result,
                                    TypePair(TypeMap::BOOL, Some(ir::types::B1)),
                                )))
                            }

                            _ => todo!(),
                        },

                        _ => todo!("{:?}", op),
                    }
                } else {
                    todo!();
                }
            }

            Expr::Unary { op, value } => todo!(),
            Expr::Named { target, value } => todo!(),
            Expr::Primary(primary) => return primary.inner.lower((ctx, builder)),
        }
    }
}

impl LowerCodegen for Statement {
    fn lower<'g>(&self, (ctx, builder): CodegenLowerArg<'g, '_, '_>) -> Option<Value<'g>> {
        use ir::InstBuilder;

        match self {
            Self::FnDef(_) | Self::Import(_) | Self::Class(_) => unreachable!(),

            Statement::Expression(expr) => LowerCodegen::lower(expr, (ctx, builder)),

            Statement::Ret(ret) => {
                if let Some(expr) = &ret.value {
                    let value = {
                        let value = match expr.inner.lower((ctx.clone(), builder)).unwrap() {
                            Ok(v) => v,
                            Err(_) => todo!(),
                        };

                        let refined = ctx.maybe_coerce(value, ctx.func.kind.inner.ret, builder);

                        refined.deref_into_raw(builder)
                    };

                    builder.ins().return_(&[value]);
                } else {
                    builder.ins().return_(&[]);
                }

                None
            }

            Statement::Asn(assign) => {
                let mut value = match assign.value.inner.lower((ctx.clone(), builder)).unwrap() {
                    Ok(value) => value,
                    Err((layout, f)) => {
                        let value_t = ctx.type_of(&assign.value).unwrap();

                        return ctx
                            .with_var_alloc(assign.name.name().unwrap(), |storage| {
                                f((ctx.clone(), builder), storage)
                            })
                            .map(Ok);
                    }
                };

                if let Some(ann) = &assign.kind {
                    let expected = ctx.type_of(ann).unwrap();

                    value = ctx.maybe_coerce(value, expected, builder);
                }

                ctx.with_var_alloc(assign.name.name().unwrap(), move |storage| {
                    storage.write(value, builder)
                });

                None
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

                for (branch_blocks_idx, ifstmt) in ifstmt.branches.iter().enumerate() {
                    let (head_block, body_block, local_escape_block) =
                        branch_blocks[branch_blocks_idx];

                    builder.ins().jump(head_block, &[]);

                    builder.switch_to_block(head_block);

                    let cc = match ifstmt
                        .inner
                        .test
                        .inner
                        .lower((ctx.clone(), builder))
                        .unwrap()
                    {
                        Ok(v) => v.into_raw(builder),
                        Err(_) => todo!(),
                    };

                    builder.ins().brnz(cc, body_block, &[]);
                    builder.ins().jump(local_escape_block, &[]);

                    {
                        builder.switch_to_block(body_block);

                        for part in ifstmt.inner.body.iter() {
                            let _ = part.inner.lower((ctx.clone(), builder));
                        }

                        if !builder.is_filled() {
                            builder.ins().jump(global_escape_block, &[]);
                        }
                    }

                    builder.switch_to_block(local_escape_block);
                }

                let orelse = builder.create_block();

                builder.ins().jump(orelse, &[]);

                builder.switch_to_block(orelse);

                if let Some(orelse) = &ifstmt.orelse {
                    for stmt in orelse {
                        let _ = stmt.inner.lower((ctx.clone(), builder));
                    }
                }

                if !builder.is_filled() {
                    builder.ins().jump(global_escape_block, &[]);
                }

                builder.switch_to_block(global_escape_block);

                None
            }

            Statement::While(while_) => {
                let (head, body, escape) = {
                    (
                        builder.create_block(),
                        builder.create_block(),
                        builder.create_block(),
                    )
                };

                builder.ins().jump(head, &[]);

                builder.switch_to_block(head);

                let cc = match while_.test.inner.lower((ctx.clone(), builder)).unwrap() {
                    Ok(v) => v.into_raw(builder),
                    Err(_) => todo!(),
                };

                builder.ins().brnz(cc, body, &[]);
                builder.ins().jump(escape, &[]);

                builder.switch_to_block(body);

                for part in while_.body.iter() {
                    let _ = part.inner.lower((ctx.clone(), builder));
                }

                if !builder.is_filled() {
                    builder.ins().jump(head, &[]);
                    builder.switch_to_block(escape);
                } else {
                    builder.switch_to_block(escape);
                    builder.ins().jump(head, &[]);
                }

                None
            }

            Statement::Pass => {
                builder.ins().nop();
                None
            }
        }
    }
}
