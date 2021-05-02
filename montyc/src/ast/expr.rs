use std::{path::PathBuf, rc::Rc};

use crate::{
    context::codegen::{CodegenContext, CodegenLowerArg},
    prelude::*,
};

use super::{atom::Atom, primary::Primary, AstObject, ObjectIter, Spanned};

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Invert,
    Not,
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOp {
    Add,
    Sub,
    Power,
    Invert,
    FloorDiv,
    MatMult,
    Mod,
    Div,
    Mult,
    LeftShift,
    RightShift,
    NotEq,
    Eq,
    And,
    Or,
}

impl AstObject for InfixOp {
    fn span(&self) -> Option<logos::Span> {
        todo!()
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        todo!()
    }

    fn walk(&self) -> Option<ObjectIter> {
        todo!()
    }
}

impl LookupTarget for InfixOp {
    fn is_named(&self, _target: SpanEntry) -> bool {
        todo!()
    }

    fn name(&self) -> SpanEntry {
        todo!()
    }
}

impl TypedObject for InfixOp {
    fn infer_type<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        todo!()
    }

    fn typecheck<'a>(&self, _ctx: &LocalContext<'a>) -> crate::Result<()> {
        todo!()
    }
}

impl InfixOp {
    pub fn sigil(&self) -> &str {
        match self {
            InfixOp::Add => "+",
            InfixOp::Sub => "-",
            InfixOp::Power => "^",
            InfixOp::Invert => "~",
            InfixOp::FloorDiv => "//",
            InfixOp::MatMult => "@",
            InfixOp::Mod => "%",
            InfixOp::Div => "/",
            InfixOp::Mult => "*",
            InfixOp::LeftShift => "<<",
            InfixOp::RightShift => ">>",
            InfixOp::NotEq => "!=",
            InfixOp::Eq => "==",
            InfixOp::And => "and",
            InfixOp::Or => "or",
        }
    }
}

impl AsRef<str> for InfixOp {
    fn as_ref(&self) -> &str {
        match self {
            InfixOp::Add => "add",
            InfixOp::Sub => "sub",
            InfixOp::Power => "pow",
            InfixOp::Invert => todo!(),
            InfixOp::FloorDiv => "floordiv",
            InfixOp::MatMult => "matmul",
            InfixOp::Mod => "mod",
            InfixOp::Div => "div",
            InfixOp::Mult => "mul",
            InfixOp::LeftShift => "lshift",
            InfixOp::RightShift => "rshift",
            InfixOp::NotEq => "ne",
            InfixOp::Eq => "eq",
            InfixOp::And => "and",
            InfixOp::Or => "or",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    If {
        test: Rc<Spanned<Expr>>,
        body: Rc<Spanned<Expr>>,
        orelse: Rc<Spanned<Expr>>,
    },

    BinOp {
        left: Rc<Spanned<Expr>>,
        op: InfixOp,
        right: Rc<Spanned<Expr>>,
    },

    Unary {
        op: UnaryOp,
        value: Rc<Spanned<Expr>>,
    },

    Named {
        target: Spanned<Atom>,
        value: Rc<Spanned<Expr>>,
    },

    Primary(Spanned<Primary>),
}

impl Parseable for Expr {
    const PARSER: ParserT<Self> = crate::parser::comb::expression_unspanned;
}

impl AstObject for Expr {
    fn span(&self) -> Option<Span> {
        if let Self::Primary(p) = self {
            p.span()
        } else {
            None
        }
    }

    fn walk<'a>(&'a self) -> Option<ObjectIter> {
        let it = match &self {
            Expr::If { test, body, orelse } => vec![
                test.clone() as Rc<dyn AstObject>,
                body.clone() as Rc<dyn AstObject>,
                orelse.clone() as Rc<dyn AstObject>,
            ],

            Expr::BinOp { left, op: _, right } => vec![
                left.clone() as Rc<dyn AstObject>,
                right.clone() as Rc<dyn AstObject>,
            ],

            Expr::Unary { op: _, value } => vec![value.clone() as Rc<dyn AstObject>],

            Expr::Named { target, value } => vec![
                Rc::new(target.clone()) as Rc<dyn AstObject>,
                value.clone() as Rc<dyn AstObject>,
            ],

            Expr::Primary(primary) => return primary.walk(),
        };

        Some(Box::new(it.into_iter()))
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }
}

impl TypedObject for Expr {
    fn infer_type(&self, ctx: &LocalContext<'_>) -> crate::Result<LocalTypeId> {
        match self {
            Expr::If {
                test: _,
                body,
                orelse: _,
            } => body.infer_type(ctx),

            Expr::BinOp { left, op, right } => {
                let left_ty = ctx.with(Rc::clone(left) as Rc<_>, |ctx, this| this.infer_type(&ctx))?;
                let right_ty = ctx.with(Rc::clone(right) as Rc<_>, |ctx, this| this.infer_type(&ctx))?;

                ctx.cache_type(left, left_ty);
                ctx.cache_type(right, right_ty);

                let ltr_name_str = format!("__{}__", op.as_ref());
                let rtl_name_str = format!("__r{}__", op.as_ref());

                let name_ref = ModuleRef(PathBuf::from(format!("__monty:magical_names")));

                let m = ctx.global_context.resolver.sources.get(&name_ref).unwrap();

                let ltr_name = ctx.global_context.span_ref.borrow().find(&ltr_name_str, &m);
                let rtl_name = ctx.global_context.span_ref.borrow().find(&rtl_name_str, &m);

                let ltr = FunctionType {
                    reciever: Some(left_ty),
                    name: ltr_name,
                    args: vec![right_ty],
                    ret: TypeMap::UNKNOWN,
                    decl: None,
                    module_ref: name_ref.clone(),
                };

                let rtl = FunctionType {
                    reciever: Some(right_ty),
                    name: rtl_name,
                    args: vec![left_ty],
                    ret: TypeMap::UNKNOWN,
                    decl: None,
                    module_ref: name_ref,
                };

                let left_class = ctx.try_get_class_of_type(left_ty).unwrap();
                let right_class = ctx.try_get_class_of_type(right_ty).unwrap();

                // unify(ltr) or unify(rtl)

                let method = left_class
                    .try_unify_method(ctx, &ltr)
                    .or(right_class.try_unify_method(ctx, &rtl));

                method.ok_or(MontyError::BadBinaryOp {
                    span: ctx.this.as_ref().unwrap().span().unwrap(),
                    left: left_ty,
                    right: right_ty,
                    op: op.clone(),
                })
            }

            Expr::Unary { op: _, value: _ } => todo!(),

            Expr::Named {
                target: _,
                value: _,
            } => todo!(),

            Expr::Primary(p) => p.infer_type(ctx),
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        match self {
            Expr::If { test, body, orelse } => {
                let test_type = test.infer_type(ctx)?;

                if test_type != TypeMap::BOOL {
                    ctx.exit_with_error(MontyError::BadConditionalType {
                        actual: test_type,
                        span: test.span.clone(),
                    });
                }

                let immediate_body_type = body.infer_type(ctx)?;
                let adjacent_branch_type = orelse.infer_type(ctx)?;

                if immediate_body_type != adjacent_branch_type {
                    ctx.exit_with_error(MontyError::IncompatibleTypes {
                        left_span: body.span.clone(),
                        right_span: orelse.span.clone(),
                        left: immediate_body_type,
                        right: adjacent_branch_type,
                    });
                }

                Ok(())
            }

            Expr::BinOp {
                left: _,
                op: _,
                right: _,
            } => {
                let ty = self.infer_type(ctx)?;
                ctx.global_context.type_map.cache.insert(
                    (
                        ctx.module_ref.clone(),
                        ctx.this.clone().unwrap().span().unwrap(),
                    ),
                    ty,
                );
                Ok(())
            }

            Expr::Unary { op: _, value: _ } => todo!(),
            Expr::Named {
                target: _,
                value: _,
            } => todo!(),
            Expr::Primary(p) => p.typecheck(ctx),
        }
    }
}

impl LookupTarget for Expr {
    fn is_named(&self, _target: crate::parser::SpanEntry) -> bool {
        false
    }

    fn name(&self) -> crate::parser::SpanEntry {
        None
    }
}

impl<'a> Lower<Layout<&'a dyn AstObject>> for &'a Expr {
    fn lower(&self) -> Layout<&'a dyn AstObject> {
        log::trace!("lower:expr {:?}", self);

        let mut layout = Layout::<&dyn AstObject>::new();

        match self {
            Expr::If { test, body, orelse } => {
                let body = &body.inner;
                let test = &test.inner;
                let orelse = &orelse.inner;

                let (t_start, t_end) = layout.with_sublayout_from(test);

                layout.succeed(layout.start, t_start);

                let (b_start, b_end) = layout.with_sublayout_from(body);
                let (o_start, o_end) = layout.with_sublayout_from(orelse);

                layout.succeed(t_end, b_start);
                layout.succeed(t_end, o_start);

                layout.succeed(b_end, layout.end);
                layout.succeed(o_end, layout.end);
            }

            Expr::BinOp { left, op, right } => {
                let left = &left.inner;
                let right = &right.inner;

                let (l_start, l_end) = layout.with_sublayout_from(left);

                layout.succeed(layout.start, l_start);

                let (r_start, r_end) = layout.with_sublayout_from(right);

                layout.succeed(l_end, r_start);

                let infix = layout.insert_into_new_block(op);

                layout.succeed(r_end, infix);

                layout.succeed(infix, layout.end);
            }

            Expr::Unary { op: _, value: _ } => todo!(),
            Expr::Named {
                target: _,
                value: _,
            } => todo!(),
            Expr::Primary(primary) => {
                let block = layout.insert_into_new_block(primary);
                layout.succeed(block, layout.end);
                layout.succeed(layout.start, block);
            }
        }

        layout
    }
}

impl<'a, 'b> LowerWith<CodegenLowerArg<'a, 'b>, cranelift_codegen::ir::Value> for Expr {
    fn lower_with(&self, ctx: CodegenLowerArg<'a, 'b>) -> cranelift_codegen::ir::Value {
        use cranelift_codegen::ir::InstBuilder;

        let CodegenContext {
            codegen_backend,
            builder,
            vars,
            func,
        } = ctx.clone();

        #[allow(warnings)]
        match self {
            Expr::If { test, body, orelse } => todo!(),

            Expr::BinOp { left, op, right } => {
                let left_ty = ctx
                    .codegen_backend
                    .global_context
                    .resolver
                    .type_map
                    .cache
                    .get(&(
                        func.scope.inner.module_ref.clone().unwrap(),
                        left.span.clone(),
                    ))
                    .unwrap()
                    .value()
                    .clone();

                let right_ty = ctx
                    .codegen_backend
                    .global_context
                    .resolver
                    .type_map
                    .cache
                    .get(&(
                        func.scope.inner.module_ref.clone().unwrap(),
                        right.span.clone(),
                    ))
                    .unwrap()
                    .value()
                    .clone();

                if left_ty.is_builtin() && right_ty.is_builtin() {
                    let lvalue = left.inner.lower_with(ctx.clone());
                    let rvalue = right.inner.lower_with(ctx);

                    match op {
                        InfixOp::Add => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                builder.borrow_mut().ins().iadd(lvalue, rvalue)
                            }
                            _ => unreachable!(),
                        },

                        InfixOp::Sub => match (left_ty, right_ty) {
                            (TypeMap::INTEGER, TypeMap::INTEGER) => {
                                builder.borrow_mut().ins().isub(lvalue, rvalue)
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
                                builder.borrow_mut().ins().imul(lvalue, rvalue)
                            }
                            _ => unreachable!(),
                        },

                        InfixOp::LeftShift => todo!(),
                        InfixOp::RightShift => todo!(),
                        InfixOp::NotEq => todo!(),
                        InfixOp::Eq => todo!(),
                        InfixOp::And => todo!(),
                        InfixOp::Or => todo!(),
                    }
                } else {
                    todo!()
                }
            }

            Expr::Unary { op, value } => todo!(),
            Expr::Named { target, value } => todo!(),
            Expr::Primary(p) => p.inner.lower_with(CodegenContext {
                codegen_backend,
                builder: Rc::clone(&builder),
                vars: &vars,
                func,
            }),
        }
    }
}
