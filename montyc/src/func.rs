use std::{cell::RefCell, marker::PhantomData, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{
    ast::{
        atom::Atom, expr::Expr, funcdef::FunctionDef, primary::Primary, retrn::Return,
        stmt::Statement,
    },
    database::DefId,
    prelude::*,
    scope::ScopeRoot,
    typing::TaggedType,
};

#[derive(Debug)]
pub enum DataRef {
    StringConstant {
        span_entry: NonZeroUsize,
        module_ref: ModuleRef,
    },

    FunctionRef {
        span_entry: NonZeroUsize,
        module_ref: ModuleRef,
    },
}

#[derive(Debug)]
pub struct Function {
    def: DefId,
    pub scope: LocalScope<FunctionDef>,
    pub kind: TaggedType<FunctionType>,
    pub vars: DashMap<SpanRef, (LocalTypeId, Span)>,
    pub refs: RefCell<Vec<DataRef>>,
}

impl Function {
    pub fn def(&self, gctx: &GlobalContext) -> Option<Rc<dyn AstObject>> {
        gctx.database.as_weak_object(self.def)
    }

    pub fn name(&self, gctx: &GlobalContext) -> SpanRef {
        self.def(gctx)
            .unwrap()
            .unspanned()
            .as_ref()
            .as_function()
            .unwrap()
            .name()
            .unwrap()
    }

    pub fn name_as_string(&self, gctx: &GlobalContext) -> Option<String> {
        let def = self.name(gctx);

        gctx.resolver
            .resolve(self.scope.inner.module_ref.clone().unwrap(), def)
    }

    pub fn new(def: &Rc<dyn AstObject>, ctx: &LocalContext) -> crate::Result<Rc<Self>> {
        let def = ctx
            .global_context
            .database
            .entry(Rc::clone(def), &ctx.module_ref);
        let id = ctx.global_context.database.id_of(&def).unwrap();
        let def = ctx
            .global_context
            .database
            .as_weak_object(id)
            .unwrap()
            .unspanned();
        let func = crate::isinstance!(def.as_ref(), Statement, Statement::FnDef(f) => f).unwrap();

        let mut scope = OpaqueScope::from(Rc::clone(&def) as Rc<_>);

        scope.module_ref = Some(ctx.module_ref.clone());
        scope.parent = Some(ctx.scope.clone());

        let scope = LocalScope {
            _t: PhantomData,
            inner: scope,
        };

        let kind = {
            let type_id = def.infer_type(ctx).unwrap_or_compiler_error(ctx);

            ctx.global_context
                .type_map
                .get_tagged(type_id)
                .unwrap()
                .unwrap()
        };

        let vars = DashMap::default();

        if let Some(args) = &func.args {
            for (name, ann) in args.iter() {
                let ty = ctx.with(Rc::clone(&ann), |ctx, ann| ann.infer_type(&ctx))?;

                vars.insert(name.clone(), (ty, ann.span.clone()));
            }
        }

        let func = Self {
            def: id,
            scope,
            kind,
            vars,
            refs: Default::default(),
        };

        let mut func = Rc::new(func);

        let mut root = ScopeRoot::Func(Rc::clone(&func));

        // SAFETY: The only other reference to `func` is in the scope root.
        //         which doesn't get dereferenced in the call to `std::mem::swap`.
        unsafe {
            let func = Rc::get_mut_unchecked(&mut func);
            std::mem::swap(&mut func.scope.inner.root, &mut root);
        }

        Ok(func)
    }

    pub fn is_externaly_defined(
        &self,
        global_context: &GlobalContext,
        callcov: Option<&str>,
    ) -> bool {
        let def = self.def(global_context).unwrap().unspanned();

        let def: &FunctionDef = def.as_function().unwrap();

        let source = global_context
            .modules
            .get(&self.scope.module_ref())
            .as_ref()
            .unwrap()
            .source
            .clone();

        // checks if the function has a decorator "extern"
        // "extern" may be called with zero or exactly one arguments
        // and the argument if present must be a string literal that matches `callcov`
        let has_extern_tag = def
            .decorator_list
            .as_slice()
            .iter()
            .any(|dec| match &dec.inner {
                Primary::Atomic(atom) => atom
                    .reveal(&source)
                    .map(|st| st == "extern")
                    .unwrap_or(false),

                Primary::Call { func, args } => {
                    source
                        .get(func.span.clone())
                        .map(|st| st == "extern")
                        .unwrap_or(false)
                        && args
                        .as_ref()
                            .map(|args| {
                                args.len() == 1
                                    && args
                                        .get(0)
                                        .map(|arg| {
                                            matches!(
                                                &arg.as_ref().inner,
                                                Expr::Primary(Spanned { inner: Primary::Atomic(atom), .. }) if matches!(atom.as_ref(), Spanned { inner: Atom::Str(n), ..} if matches!(source.get(global_context.span_ref.borrow().get(*n).unwrap()), Some(st) if callcov.map(|cc| cc == st).unwrap_or(true))
                                            ))
                                        })
                                        .unwrap_or(false)
                            })
                            .unwrap_or(true)
                }
                _ => false,
            });

        let body_is_ellipsis = match def.body.as_slice() {
            [] => unreachable!(),

            [obj]
            | [_, obj] => {
                crate::isinstance!(obj.as_ref(), Statement, Statement::Expression(Expr::Primary(Spanned { inner: Primary::Atomic(atom), .. })) => matches!(atom.as_ref(), Spanned { inner: Atom::Ellipsis, ..})).unwrap_or(false)
            },

            _ => false,
        };

        has_extern_tag && body_is_ellipsis
    }
}

impl TypedObject for Function {
    fn infer_type<'a>(&self, _: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        Ok(self.kind.type_id)
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!(
            "typecheck:function {}",
            ctx.as_formattable(&self.kind.inner)
        );

        if self.is_externaly_defined(ctx.global_context, None) {
            return Ok(());
        }

        assert_matches!(self.scope.root(), ScopeRoot::Func(_));

        let mut implicit_return = true;

        for scoped_object in self.scope.iter() {
            let node = scoped_object.object.clone();

            assert_matches!(scoped_object.scope.root(), ScopeRoot::Func(_));

            if node.as_ref().downcast_ref::<Spanned<Return>>().is_some()
                || node.as_ref().downcast_ref::<Return>().is_some()
                || node
                    .as_ref()
                    .downcast_ref::<Spanned<Statement>>()
                    .map(|Spanned { inner, .. }| matches!(inner, Statement::Ret(_)))
                    .unwrap_or(false)
            {
                implicit_return = false;
            }

            scoped_object.with_context(ctx.global_context, |local_context, object| {
                object.typecheck(&local_context)
            })?;
        }

        if implicit_return
            && !ctx
                .global_context
                .type_map
                .type_eq(self.kind.inner.ret, TypeMap::NONE_TYPE)
        {
            let def_node = match self.scope.root() {
                ScopeRoot::Func(f) => f.def(ctx.global_context).unwrap(),
                _ => unreachable!(),
            };

            ctx.exit_with_error(MontyError::MissingReturn {
                expected: TypeMap::NONE_TYPE,
                actual: self.kind.inner.ret,
                def_span: def_node.span().unwrap(),
                ret_span: def_node
                    .as_ref()
                    .as_function()
                    .cloned()
                    .unwrap()
                    .returns
                    .map(|ret| ret.span.clone())
                    .unwrap_or(def_node.span().unwrap()),
            });
        }

        Ok(())
    }
}

// impl Lower<Layout<Rc<dyn AstObject>>> for &Function {
//     fn lower(&self) -> Layout<Rc<dyn AstObject>> {
//         self.def.inner.lower()
//     }
// }
