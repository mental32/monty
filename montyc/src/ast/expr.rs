use std::rc::Rc;

use crate::prelude::*;

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
    fn is_named(&self, _target: SpanRef) -> bool {
        todo!()
    }

    fn name(&self) -> Option<SpanRef> {
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
    const PARSER: ParserT<Self> = crate::parser::comb::expr::expression_unspanned;
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
                orelse,
            } => {
                let left = ctx.with(Rc::clone(body), |ctx, body| body.infer_type(&ctx))?;
                let right = ctx.with(Rc::clone(orelse), |ctx, orelse| orelse.infer_type(&ctx))?;

                let ty = if left != right {
                    // create a union[left, right]
                    ctx.global_context.type_map.entry(TypeDescriptor::Generic(
                        crate::typing::Generic::Union {
                            inner: vec![left, right],
                        },
                    ))
                } else {
                    left
                };

                Ok(ty)
            }

            Expr::BinOp { left, op, right } => {
                let left_ty = ctx.with(Rc::clone(left), |ctx, this| this.infer_type(&ctx))?;
                let right_ty = ctx.with(Rc::clone(right), |ctx, this| this.infer_type(&ctx))?;

                let ltr_name = ctx
                    .global_context
                    .magical_name_of(&format!("__{}__", op.as_ref()))
                    .unwrap();

                let rtl_name = ctx
                    .global_context
                    .magical_name_of(&format!("__{}__", op.as_ref()))
                    .unwrap();

                let ltr = FunctionType {
                    reciever: Some(left_ty),
                    name: ltr_name,
                    args: vec![right_ty],
                    ret: TypeMap::UNKNOWN,
                    module_ref: "__monty:magical_names".into(),
                };

                let rtl = FunctionType {
                    reciever: Some(right_ty),
                    name: rtl_name,
                    args: vec![left_ty],
                    ret: TypeMap::UNKNOWN,
                    module_ref: "__monty:magical_names".into(),
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
            Expr::If { test, .. } => {
                let test_type = ctx.with(Rc::clone(&test), |ctx, test| test.infer_type(&ctx))?;

                if !ctx
                    .global_context
                    .type_map
                    .type_eq(test_type, TypeMap::BOOL)
                {
                    ctx.exit_with_error(MontyError::BadConditionalType {
                        actual: test_type,
                        span: test.span.clone(),
                    });
                }

                let _ = self.infer_type(ctx)?;

                Ok(())
            }

            Expr::BinOp {
                left: _,
                op: _,
                right: _,
            } => {
                let ty = self.infer_type(ctx)?;
                ctx.cache_type(ctx.this.as_ref().unwrap(), ty);
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
    fn is_named(&self, _target: SpanRef) -> bool {
        false
    }

    fn name(&self) -> Option<crate::parser::SpanRef> {
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
