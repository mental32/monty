use std::rc::Rc;

use super::{
    atom::Atom, expr::Expr, funcdef::FunctionDef, stmt::Statement, AstObject, ObjectIter, Spanned,
};
use crate::{context::codegen::CodegenLowerArg, prelude::*, scope::LookupOrder};

#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    Atomic(Spanned<Atom>),

    /// `<value:primary>[<index?>]`
    Subscript {
        value: Rc<Spanned<Primary>>,
        index: Option<Rc<Spanned<Primary>>>,
    },

    /// `<func:primary>(<args?>)`
    Call {
        func: Rc<Spanned<Primary>>,
        args: Option<Vec<Rc<Spanned<Expr>>>>,
    },

    /// `<primary> DOT(.) <atom>`
    Attribute {
        left: Rc<Spanned<Primary>>,
        attr: Spanned<Atom>,
    },

    /// `(await +)+<primary>`
    Await(Rc<Spanned<Primary>>),
}

impl Primary {
    /// break a dotted name down into its named components
    pub fn components(&self) -> Vec<Atom> {
        let mut names = vec![];

        match self {
            Primary::Atomic(Spanned { inner, .. }) => {
                names.push(inner.clone());
            }

            Primary::Attribute { left, attr } => {
                names.extend(left.inner.components());
                names.push(attr.inner.clone());
            }

            _ => unreachable!(),
        }

        names
    }
}

impl AstObject for Primary {
    fn walk<'a>(&'a self) -> Option<ObjectIter> {
        let it = match self {
            Primary::Atomic(_) => return None,
            Primary::Await(inner) => return inner.walk(),

            Primary::Subscript { value, index } => {
                let mut v = vec![value.clone() as Rc<dyn AstObject>];

                if index.is_some() {
                    v.push(Rc::new(index.clone()) as Rc<dyn AstObject>)
                }

                v
            }

            Primary::Call { func, args } => {
                let mut v = vec![func.clone() as Rc<dyn AstObject>];

                if let Some(args) = args {
                    v.extend(args.iter().map(|arg| arg.clone() as Rc<dyn AstObject>));
                }

                v
            }

            Primary::Attribute { left, attr } => {
                vec![
                    left.clone() as Rc<dyn AstObject>,
                    Rc::new(attr.clone()) as Rc<dyn AstObject>,
                ]
            }
        };

        Some(Box::new(it.into_iter()))
    }

    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }
}

impl TypedObject for Primary {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        log::trace!("infer_type: {:?}", self);

        match self {
            Primary::Atomic(at) => at.infer_type(ctx),
            Primary::Subscript { value: _, index: _ } => todo!(),
            Primary::Call { func, args: _ } => {
                let func_t = func.infer_type(ctx).unwrap_or_compiler_error(ctx);
                let func_t = &ctx
                    .global_context
                    .type_map
                    .get_tagged::<FunctionType>(func_t)
                    .unwrap()
                    .unwrap();

                Ok(func_t.inner.ret)
            }

            Primary::Attribute { left: _, attr: _ } => todo!(),
            Primary::Await(_) => todo!("`await` doesn't exist here."),
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:primary {:?}", self);

        match self {
            Primary::Atomic(at) => at.typecheck(ctx),
            Primary::Subscript { value: _, index: _ } => todo!(),

            Primary::Call { func, args } => {
                let func_t = func.infer_type(ctx).unwrap_or_compiler_error(ctx);

                let callsite = args
                    .as_ref()
                    .map(|args| {
                        args.iter()
                            .map(|arg| {
                                let mut ctx = ctx.clone();
                                ctx.this = Some(arg.clone());
                                arg.infer_type(&ctx).unwrap_or_compiler_error(&ctx)
                            })
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();

                let type_map = &ctx.global_context.type_map;

                if let Err((expected, actual, idx)) = type_map.unify_call(func_t, callsite.iter()) {
                    let def_node = 'outer: loop {
                        let results = ctx
                            .scope
                            .lookup_def(func.name(), &ctx.global_context, LookupOrder::Unspecified)
                            .unwrap_or_compiler_error(ctx);

                        for obj in results {
                            if let Some(f) = obj.as_ref().downcast_ref::<Spanned<FunctionDef>>() {
                                break 'outer f.clone();
                            } else if let Some(Statement::FnDef(f)) =
                                obj.as_ref().downcast_ref::<Statement>()
                            {
                                break 'outer Spanned {
                                    span: f.name.span.start
                                        ..f.body
                                            .last()
                                            .and_then(|l| l.span())
                                            .unwrap_or(f.name.span.clone())
                                            .end,
                                    inner: f.clone(),
                                };
                            }
                        }

                        todo!("no func result.");
                    };

                    let def_node = Rc::new(def_node);

                    ctx.exit_with_error(MontyError::BadArgumentType {
                        expected,
                        actual,
                        arg_node: args.as_ref().unwrap().get(idx).cloned().unwrap(),
                        def_node,
                    });
                } else {
                    Ok(())
                }
            }

            Primary::Attribute { left: _, attr: _ } => todo!(),
            Primary::Await(_) => todo!(),
        }
    }
}

impl LookupTarget for Primary {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        matches!(self, Self::Atomic(Spanned { inner: Atom::Name(n), .. }) if n.clone() == target)
    }

    fn name(&self) -> crate::parser::SpanEntry {
        match self {
            Self::Atomic(at) => at.name(),
            _ => None,
        }
    }
}

impl<'a, 'b> LowerWith<CodegenLowerArg<'a, 'b>, cranelift_codegen::ir::Value> for Primary {
    fn lower_with(&self, ctx: CodegenLowerArg<'a, 'b>) -> cranelift_codegen::ir::Value {

        #[allow(warnings)]
        match self {
            Primary::Atomic(at) => at.inner.lower_with(ctx),
            Primary::Subscript { value, index } => todo!(),
            Primary::Call { func, args } => todo!(),
            Primary::Attribute { left, attr } => todo!(),
            Primary::Await(_) => todo!(),
        }
    }
}
