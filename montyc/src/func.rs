use std::{cell::RefCell, collections::HashMap, marker::PhantomData, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{ast::{
        atom::{Atom, StringRef},
        expr::Expr,
        funcdef::FunctionDef,
        primary::Primary,
        retrn::Return,
        stmt::Statement,
    }, database::DefId, prelude::*, scope::{ScopeRoot, ScopedObject}, typing::TaggedType};

pub fn is_externaly_defined(
    def: &FunctionDef,
    global_context: &GlobalContext,
    mref: &ModuleRef,
    callcov: Option<&str>,
) -> bool {
    let source = global_context
        .modules
        .get(mref)
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

#[derive(Debug)]
pub enum DataRef {
    StringConstant(StringRef),

    FunctionRef {
        span_entry: NonZeroUsize,
        module_ref: ModuleRef,
    },
}

pub struct Function {
    pub def_id: DefId,
    has_extern_tag: bool,

    pub scope: Rc<LocalScope<FunctionDef>>,
    pub kind: TaggedType<FunctionType>,
    pub vars: DashMap<(SpanRef, SpanRef), (LocalTypeId, Span)>,
    pub refs: RefCell<Vec<DataRef>>,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("kind", &self.kind)
            .finish()
    }
}

impl Function {
    pub fn def(&self, gctx: &GlobalContext) -> Option<Rc<dyn AstObject>> {
        gctx.database.as_weak_object(self.def_id)
    }

    pub fn args(
        &self,
        gctx: &GlobalContext,
    ) -> impl Iterator<Item = ((SpanRef, SpanRef), LocalTypeId)> {
        if let Some(FunctionDef { args, .. }) =
            self.def(gctx).unwrap().unspanned().as_ref().as_function()
        {
            let args = match args {
                Some(args) => {
                    let typed_args = args.iter().cloned().map(|(n, ann)| {
                        let type_id = gctx
                            .database
                            .type_of(&(Rc::clone(&ann) as Rc<_>), None)
                            .unwrap();

                        (n, type_id)
                    });

                    typed_args.collect()
                }

                None => vec![],
            };

            args.into_iter()
        } else {
            unreachable!("not a function");
        }
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
        assert!(
            ctx.this
                .as_ref()
                .map(|this| Rc::as_ptr(this) as *const () == Rc::as_ptr(def) as *const ())
                .unwrap_or(false),
            "Rc::as_ptr(def) != Rc::as_ptr(ctx.this): `ctx.this` must be the same object as `def`"
        );

        let def_id = {
            let entry = ctx
                .global_context
                .database
                .entry(Rc::clone(def), &ctx.module_ref);

            ctx.global_context.database.id_of(&entry).unwrap()
        };

        let def = def.unspanned();
        let funcdef =
            crate::isinstance!(def.as_ref(), Statement, Statement::FnDef(f) => f).unwrap();

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

        if let Some(args) = &funcdef.args {
            for (name, ann) in args.iter() {

                let (ty, _) = {
                    let ty = match crate::utils::try_parse_union_literal(ctx, &ann.inner, false)? {
                        Some(tys) => (ctx.global_context.type_map.tagged_union(tys), true),
                        None => {
                            let mut ctx = ctx.clone();
                            ctx.this = Some(Rc::clone(ann) as Rc<_>);
                            let ty = ann.infer_type(&ctx).unwrap_or_compiler_error(&ctx);
                            (ty, false)
                        }
                    };
    
                    ctx.cache_type(&(Rc::clone(ann) as Rc<dyn AstObject>), ty.0);
    
                    ty
                };

                let ty = ty.canonicalize(&ctx.global_context.type_map);

                vars.insert(name.clone(), (ty, ann.span.clone()));
            }
        }

        let mut func = Self {
            has_extern_tag: false,
            def_id,
            scope: Rc::new(scope),
            kind,
            vars,
            refs: Default::default(),
        };

        func.has_extern_tag =
            is_externaly_defined(funcdef, &ctx.global_context, &ctx.scope.module_ref(), None);

        let func = Rc::new(func);

        let mut root = ScopeRoot::Func(Rc::clone(&func));

        {
            let empty_slot = &mut *func.scope.inner.root.borrow_mut();
            std::mem::swap(empty_slot, &mut root);
        }

        Ok(func)
    }

    pub fn is_externaly_defined(&self) -> bool {
        self.has_extern_tag
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

        if self.is_externaly_defined() {
            return Ok(());
        }

        assert_matches!(self.scope.root(), ScopeRoot::Func(_));


        let (def_span, def_node) = {
            let def = ctx.this.clone().expect("`ctx.this` is not set.");
            let span = def.span().expect("definition must be spanned.");

            (span, def.unspanned())
        };

        let funcdef = def_node
            .as_function()
            .expect("Function::def must be a FunctionDef node!");

        ctx.global_context.branches.borrow_mut().insert(self.def_id, HashMap::new());

        for scoped_object in self.scope.iter() {
            let scoped_object = ScopedObject {
                object: scoped_object.object,
                scope: Rc::clone(&self.scope) as Rc<_>
            };

            scoped_object.with_context(ctx.global_context, |mut local_context, object| {
                local_context.current_branch = Some(self.def_id);
                object.typecheck(&local_context)
            })?;
        }

        if !ctx
            .global_context
            .type_map
            .type_eq(self.kind.inner.ret, TypeMap::NONE_TYPE)
        {
            // This function returns something (i.e. not `None`)
            // check the "tailing" blocks of the layout and make sure they're all Return statements.

            let layout = {
                let mut layout = funcdef.lower();

                layout.reduce_forwarding_edges();

                layout
            };

            let tails = layout
                .blocks
                .get(&layout.end)
                .expect("Function layout does not have an exit block.")
                .preds
                .iter()
                .filter_map(|id| layout.blocks.get(&id))
                .filter_map(|block| block.nodes.last());

            for tail in tails {
                let last = tail.as_ref();

                if crate::isinstance!(last, Return).is_none()
                    && crate::isinstance!(last, Statement, Statement::Ret(_) => ()).is_none()
                {
                    ctx.exit_with_error(MontyError::MissingReturn {
                        expected: self.kind.inner.ret,
                        def_span: last.span().unwrap(),
                        ret_span: funcdef
                            .returns
                            .clone()
                            .map(|ret| ret.span.clone())
                            .unwrap_or(def_span),
                    });
                }
            }
        }

        Ok(())
    }
}
