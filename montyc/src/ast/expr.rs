use std::{path::PathBuf, rc::Rc};

use crate::{
    context::{LocalContext, ModuleRef},
    parser::{Parseable, ParserT, Span},
    scope::LookupTarget,
    typing::{CompilerError, FunctionType, TypeDescriptor, TypeMap, TypedObject},
    MontyError,
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
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<crate::typing::LocalTypeId> {
        match self {
            Expr::If {
                test: _,
                body,
                orelse: _,
            } => body.infer_type(ctx),

            Expr::BinOp { left, op, right } => {
                let left_ty = left.infer_type(ctx).unwrap_or_compiler_error(ctx);
                let right_ty = right.infer_type(ctx).unwrap_or_compiler_error(ctx);

                let left_name = ctx
                    .global_context
                    .type_map
                    .borrow()
                    .get(left_ty)
                    .clone()
                    .unwrap()
                    .clone();
                let right_name = ctx
                    .global_context
                    .type_map
                    .borrow()
                    .get(right_ty)
                    .clone()
                    .unwrap()
                    .clone();

                let ltr_name_str = format!("__{}__", op.as_ref());
                let rtl_name_str = format!("__r{}__", op.as_ref());

                let name_ref = ModuleRef(PathBuf::from(format!("__monty:magical_names")));

                let s = ctx.global_context.resolver.sources.borrow();
                let m = s.get(&name_ref).unwrap();

                let ltr_name = ctx.global_context.span_ref.borrow().find(&ltr_name_str, &m);
                let rtl_name = ctx.global_context.span_ref.borrow().find(&rtl_name_str, &m);

                let ltr = FunctionType {
                    reciever: Some(left_ty),
                    name: ltr_name,
                    args: vec![right_ty],
                    ret: TypeMap::UNKNOWN,
                    decl: None,
                    resolver: ctx.global_context.resolver.clone(),
                    module_ref: name_ref.clone(),
                };

                let rtl = FunctionType {
                    reciever: Some(right_ty),
                    name: rtl_name,
                    args: vec![left_ty],
                    ret: TypeMap::UNKNOWN,
                    decl: None,
                    resolver: ctx.global_context.resolver.clone(),
                    module_ref: name_ref,
                };

                let type_map = ctx.global_context.type_map.borrow();

                let left_class = match type_map.get(left_ty).unwrap() {
                    crate::typing::TypeDescriptor::Simple(_) => {
                        ctx.global_context.builtins.get(&left_ty).unwrap().0.clone()
                    }
                    crate::typing::TypeDescriptor::Function(_) => todo!(),
                    crate::typing::TypeDescriptor::Class(klass) => ctx
                        .global_context
                        .get_class_from_module(klass.mref.clone(), klass.name)
                        .unwrap(),
                };

                let right_class = match type_map.get(right_ty).unwrap() {
                    crate::typing::TypeDescriptor::Simple(_) => ctx
                        .global_context
                        .builtins
                        .get(&right_ty)
                        .unwrap()
                        .0
                        .clone(),
                    crate::typing::TypeDescriptor::Function(_) => todo!(),
                    crate::typing::TypeDescriptor::Class(klass) => ctx
                        .global_context
                        .get_class_from_module(klass.mref.clone(), klass.name)
                        .unwrap(),
                };

                // unify(ltr)

                for (name, kind) in left_class.properties.iter() {
                    if *name == ltr.name.unwrap() {
                        if type_map.unify_func(kind.clone(), &ltr) {
                            let ret = match type_map.get(kind.clone()) {
                                Some(TypeDescriptor::Function(f)) => f.ret,
                                _ => todo!(),
                            };

                            return Some(ret);
                        }
                    }
                }

                // unify(rtl)

                for (name, kind) in right_class.properties.iter() {
                    if *name == rtl.name.unwrap() {
                        if type_map.unify_func(kind.clone(), &rtl) {
                            let ret = match type_map.get(kind.clone()) {
                                Some(TypeDescriptor::Function(f)) => f.ret,
                                _ => todo!(),
                            };

                            return Some(ret);
                        }
                    }
                }

                ctx.error(MontyError::BadBinaryOp {
                    span: ctx.this.as_ref().unwrap().span().unwrap(),
                    left: left_ty,
                    right: right_ty,
                    op: op.clone(),
                    ctx,
                });
            }

            Expr::Unary { op: _, value: _ } => todo!(),

            Expr::Named {
                target: _,
                value: _,
            } => todo!(),

            Expr::Primary(p) => p.infer_type(ctx),
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) {
        match self {
            Expr::If { test, body, orelse } => {}
            Expr::BinOp { left, op, right } => {}
            Expr::Unary { op, value } => {}
            Expr::Named { target, value } => {}
            Expr::Primary(p) => p.typecheck(ctx),
        }
    }
}

impl LookupTarget for Expr {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        false
    }

    fn name(&self) -> crate::parser::SpanEntry {
        None
    }
}
