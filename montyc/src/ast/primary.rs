use std::rc::Rc;

use super::{
    atom::Atom, expr::Expr, funcdef::FunctionDef, stmt::Statement, AstObject, ObjectIter, Spanned,
};
use crate::{context::codegen::{CodegenLowerArg}, func::DataRef, prelude::*, scope::LookupOrder};

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
        log::trace!("infer_type:expr {:?}", self);

        match self {
            Primary::Atomic(at) => at.infer_type(ctx),
            Primary::Subscript { value: _, index: _ } => todo!(),
            Primary::Call { func, args: _ } => {
                log::trace!("infer_type:call {:?}", func);

                let func_t = func.infer_type(ctx).unwrap_or_compiler_error(ctx);
                let func_t = match ctx
                    .global_context
                    .type_map
                    .get_tagged::<FunctionType>(func_t)
                    .unwrap()
                {
                    Ok(f) => f,
                    Err(_) => ctx.exit_with_error(MontyError::NotCallable {
                        kind: func_t,
                        callsite: ctx.this.clone().unwrap().span().unwrap(),
                    }),
                };

                if let ScopeRoot::Func(host) = ctx.scope.root() {
                    let func = if let Primary::Atomic(Spanned {
                        inner: Atom::Name(n),
                        ..
                    }) = &func.inner
                    {
                        n.clone()
                    } else {
                        unreachable!();
                    };

                    let other = ctx
                        .scope
                        .lookup_def(
                            func.clone(),
                            &ctx.global_context,
                            LookupOrder::ControlFlowSensitive(ctx.this.clone().unwrap()),
                        )
                        .unwrap_or_compiler_error(ctx)
                        .iter()
                        .find_map(|o| crate::isinstance!(o.as_ref(), FunctionDef).or_else(|| crate::isinstance!(o.as_ref(), Statement, Statement::FnDef(f) => f)).map(|f| f.name()))
                        .unwrap();

                    host.refs.borrow_mut().push(DataRef::FunctionRef {
                        span_entry: other.unwrap(),
                        module_ref: ctx.module_ref.clone(),
                    });
                }

                Ok(func_t.inner.ret)
            }

            Primary::Attribute { left: _, attr: _ } => todo!(),
            Primary::Await(_) => todo!("`await` doesn't exist here."),
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck:primary {:?}", self);

        match self {
            Primary::Atomic(at) => {
                let mut ctx = ctx.clone();
                ctx.this = Some(Rc::new(at.clone()));
                at.typecheck(&ctx)
            }

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

                if let Err((expected, actual, idx)) = ctx
                    .global_context
                    .type_map
                    .unify_call(func_t, callsite.iter())
                {
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
        use cranelift_codegen::ir::InstBuilder;

        #[allow(warnings)]
        match self {
            Primary::Atomic(at) => at.inner.lower_with(ctx),
            Primary::Subscript { value, index } => todo!(),
            Primary::Call { func, args } => {
                let func = if let Primary::Atomic(Spanned {
                    inner: Atom::Name(n),
                    ..
                }) = &func.inner
                {
                    n.clone()
                } else {
                    unreachable!();
                };

                let module = ctx.codegen_backend.names.get(&ctx.func.scope.module_ref()).unwrap();

                let (target_name, target_fid) = module.functions.get(func.as_ref().unwrap()).unwrap();

                let args = match args {
                    Some(args) => {
                        args.iter().map(|arg| arg.inner.lower_with(ctx.clone())).collect()
                    },

                    None => vec![],
                };

                let mut builder = ctx.builder.borrow_mut();

                let func_ref = cranelift_module::Module::declare_func_in_func(&ctx.codegen_backend.object_module, *target_fid, &mut builder.func);

                let result = builder.ins().call(func_ref, args.as_slice());

                builder.inst_results(result).first().unwrap().clone()
            }

            Primary::Attribute { left, attr } => todo!(),
            Primary::Await(_) => todo!(),
        }
    }
}
