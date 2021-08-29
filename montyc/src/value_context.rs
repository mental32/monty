use std::iter::FromIterator;

use ahash::AHashSet;

use montyc_core::{ModuleRef, MontyError, MontyResult, TypeError, TypeId};
use montyc_hlir::{
    typing::{PythonType, TypingContext},
    value_store::{GlobalValueStore, ValueGraphIx},
    Const, Dunder, FlatCode, FlatSeq, RawInst, Value,
};

use crate::{
    def_stack::{DefScope, ScopeChain},
    prelude::GlobalContext,
};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub code: &'this FlatCode,
    pub value_store: &'gcx mut GlobalValueStore,
    pub gcx: &'gcx GlobalContext,
    pub value: &'this Value,
    pub value_idx: ValueGraphIx,
}

fn typecheck_seq(
    mut seq: FlatSeq,
    return_type: TypeId,
    tcx: &mut TypingContext,
    def_stack: &mut ScopeChain,
    value_store: &mut GlobalValueStore,
    rt: &montyc_hlir::interpreter::Runtime,
    mref: ModuleRef,
    value_ix: ValueGraphIx,
    spanner: SpanInterner,
) -> (FlatSeq, Vec<TypeId>) {
    let mut seq_value_types = vec![TypingContext::Unknown; seq.inst().len()];
    let mut inst_iter = seq.inst().to_owned().into_iter().enumerate();
    let mut refs = AHashSet::default();

    for (ix, inst) in inst_iter {
        log::trace!("[ValueContext::typecheck] {:?}", inst);

        match &inst.op {
            RawInst::Privileged(_) => unreachable!("privileged instructions are already present in this sequence (they shouldn't be.)"),
            RawInst::Class { .. } | RawInst::Defn { .. } => todo!(),

            RawInst::UseVar { variable } => {
                let (var_t, var_def) = def_stack.lookup(variable.group()).next().unwrap();

                seq_value_types[ix] = var_t;

                let inst = match var_def {
                    DefKind::Local | DefKind::Parameters => {
                        RawInst::Privileged(PrivInst::UseLocal { var: variable }),
                    },

                    DefKind::Global | DefKind::Builtin => {
                        let mref = if matches!(var_def, DefKind::Global) { mref } else { ModueRef(1) };
                        let val = if let Some(Value::Module { properties, .. }) = value_store.get(mref) {
                            let hash = rt.hash({ spanner.str_to_spanref(variable.clone()) });
                            let (_, val) = properties.get(hash).unwrap();

                            val
                        } else {
                            unreachable!()
                        };

                        let _ = refs.insert(val);

                        PrivInst::UseVar { val )
                    }

                    DefKind::Empty => unreachable!(),
                }

                seq.inst_mut()[ix] = RawInst::Privileged(inst);
            }

            RawInst::Call {
                callable,
                arguments,
            } => {
                let callable_type_id = seq_value_types[*callable];
                assert_ne!(callable_type_id, TypingContext::Unknown);

                let args = arguments
                    .iter()
                    .map(|i| {
                        let ty = seq_value_types[*i];
                        assert_ne!(callable_type_id, TypingContext::Unknown);
                        ty
                    })
                    .collect::<Vec<_>>();

                if let Some(PythonType::Callable { ret, .. }) =
                    tcx.unify_func(callable_type_id, &args, TypingContext::Unknown)
                {
                    seq_value_types[ix] = ret;
                } else {
                    todo!(
                        "{:?}",
                        tcx.contextualize(callable_type_id)
                            .unwrap()
                            .as_python_type()
                    );
                }
            }

            RawInst::SetVar { variable, value } => {
                let type_id = seq_value_types[*value];
                assert_ne!(type_id, TypingContext::Unknown);

                match def_stack.locals.insert(variable.group(), type_id) {
                    None => (),
                    Some(t) => assert_eq!(t, type_id),
                }
            }

            RawInst::GetAttribute { .. } => todo!(),
            RawInst::SetAttribute { .. } => todo!(),

            RawInst::GetDunder { object, dunder } => {
                let type_id = seq_value_types[*object];
                assert_ne!(type_id, TypingContext::Unknown);

                if tcx.is_tuple(type_id) {
                    let ty = tcx.contextualize(type_id).unwrap();
                    let py_kind = ty.as_python_type();

                    let members = match py_kind {
                        PythonType::Tuple { members } => members.clone().unwrap_or_default(),
                        _ => unreachable!(),
                    };

                    seq_value_types[ix] = match dunder {
                        Dunder::Unary(_) | Dunder::Infix(_) | Dunder::DocComment => {
                            todo!()
                        }

                        Dunder::GetItem => {
                            let ret = match members.as_slice() {
                                [] => tcx.union_type(&members),
                                [first, rest @ ..] => {
                                    if rest.iter().all(|ty| *ty == *first) {
                                        *first
                                    } else {
                                        tcx.union_type(&members)
                                    }
                                }
                            };

                            tcx.callable(Some(vec![TypingContext::TSelf, TypingContext::Int]), ret)
                        }

                        Dunder::SetItem => todo!(),
                    };

                    continue;
                }

                let type_class_value = value_store.get(type_id).unwrap();

                let (_, dunder_index) = match type_class_value {
                    Value::Class { properties, .. } => {
                        let hash = rt.hash(format!("{}", dunder));
                        properties.get(hash).unwrap();
                    }

                    _ => unreachable!(),
                };

                seq.inst_mut()[ix] = {
                    let val = dunder_index;
                    let _ = refs.insert(val);

                    RawInst::Privileged(PrivInst::RefVal { val })
                };

                let dunder_type = value_store.metadata(dunder_index).type_id.unwrap();

                seq_value_types[ix] = dunder_type;
            }

            RawInst::SetDunder { .. } | RawInst::Import { .. } => todo!(),

            RawInst::Const(const_) => {
                seq_value_types[ix] = match const_ {
                    Const::Int(_) => TypingContext::Int,
                    Const::Float(_) => TypingContext::Float,
                    Const::Bool(_) => TypingContext::Bool,
                    Const::String(_) => TypingContext::Str,
                    Const::None => TypingContext::None,
                    Const::Ellipsis => TypingContext::None,
                };
            }

            RawInst::Tuple(elements) => {
                seq_value_types[ix] =
                    tcx.tuple(elements.iter().map(|elem| seq_value_types[*elem]).collect());

                let (tuple_class, _) = <_ as montyc_hlir::value_store::GVKey>::resolve(
                    &TypingContext::UntypedTuple,
                    &*value_store,
                )
                .unwrap();

                value_store
                    .type_data
                    .insert(seq_value_types[ix], tuple_class);
            }

            RawInst::PhiJump { recv, value } => {
                let recv = *recv;

                let value_t = seq_value_types[*value];
                assert_ne!(value_t, TypingContext::Unknown);

                let phi_t = seq_value_types[recv];

                if phi_t == TypingContext::Unknown {
                    seq_value_types[recv] = value_t;
                } else if phi_t != value_t {
                    seq_value_types[recv] = tcx.make_union(phi_t, value_t);
                }
            }

            RawInst::Return { value } => {
                let type_id = seq_value_types[*value];
                assert_ne!(type_id, TypingContext::Unknown);

                assert_eq!(
                    type_id,
                    return_type,
                    "{:?}",
                    tcx.contextualize(type_id).unwrap().as_python_type()
                );

                seq_value_types[ix] = TypingContext::Never;
            }

            RawInst::PhiRecv
            | RawInst::Undefined
            | RawInst::Nop
            | RawInst::If { .. }
            | RawInst::Br { .. } => continue,
        }
    }

    seq_value_types
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
    pub(crate) fn typecheck(self) -> MontyResult<TypeId> {
        match self.value {
            Value::Function {
                ret_t,
                args_t,
                source,
                name,
                ..
            } => {
                log::debug!("[ValueContext::typecheck] Typechecking function {:?}", name);

                self.value_store
                    .metadata(self.value_idx)
                    .type_id
                    .replace(TypingContext::UntypedFunc);

                let return_type = {
                    let value = self.value_store.value_graph.node_weight(*ret_t).unwrap();

                    if let Value::Class { .. } = value {
                        self.value_store
                            .metadata(*ret_t)
                            .type_id
                            .expect("class object does not have a type_id.")
                    } else {
                        panic!(
                            "error: only expected class values as return annotations got {:#?}",
                            value
                        );
                    }
                };

                let mut params = Vec::with_capacity(
                    args_t
                        .as_ref()
                        .map(|(recv, args)| recv.is_some() as usize + args.len())
                        .unwrap_or(0),
                );

                let arg_t: Option<Vec<TypeId>> = if let Some((recv, args)) = args_t {
                    let mut seen =
                        ahash::AHashSet::with_capacity(recv.is_some() as usize + args.len());

                    if let Some(recv) = recv {
                        seen.insert(recv.clone().group());
                        params.push((recv.group(), TypingContext::TSelf));
                    }

                    let mut is_dynamically_typed = false;

                    // Verify that the parameter names are all unique.
                    for (arg, kind) in args.iter() {
                        // true iff the spanref group was already present in the set.
                        if !seen.insert(arg.group()) {
                            return Err(MontyError::TypeError {
                                module: self.mref.clone(),
                                error: TypeError::DuplicateParameters,
                            });
                        }

                        if !is_dynamically_typed {
                            is_dynamically_typed = kind.is_none();
                        }

                        match kind.clone() {
                            Some(kind_value_ix) => {
                                let kind =
                                    self.value_store.metadata(kind_value_ix).type_id.unwrap();

                                params.push((arg.group(), kind))
                            }

                            None => continue,
                        }
                    }

                    // Ignore any function definitions with non-annotated arguments.
                    if is_dynamically_typed {
                        log::debug!(
                            "[ValueContext::typecheck] Marking function as dynamically typed: {:?}",
                            name
                        );

                        return Ok(TypingContext::UntypedFunc);
                    } else {
                        Some(params.iter().map(|(_, t)| *t).collect())
                    }
                } else {
                    None
                };

                let func_type = self
                    .gcx
                    .typing_context
                    .borrow_mut()
                    .callable(arg_t, return_type);

                self.value_store
                    .metadata(self.value_idx)
                    .type_id
                    .replace(func_type);

                // Don't typecheck the bodies of ellipsis-stubbed functions as they are
                // essentially either "extern" declarations or nops.
                let source = source.and_then(|(s_mref, seq)| {
                    (s_mref == self.mref && !self.code.is_sequence_ellipsis_stubbed(seq))
                        .then(|| seq)
                });

                let s = match source {
                    Some(seq) => seq,
                    None => return Ok(func_type),
                };

                let mut def_stack = {
                    if self.mref != ModuleRef(1) {
                        let builtin_rib =
                            self.value_store.metadata(ModuleRef(1)).rib.clone().unwrap();

                        ScopeChain::new(builtin_rib)
                    } else {
                        ScopeChain::new(Default::default())
                    }
                };

                {
                    let module_rib = self.value_store.metadata(self.mref).rib.clone().unwrap();

                    def_stack.globals.extend(module_rib);
                    def_stack.parameters.extend(params);
                }

                let seq = self.code.sequences()[s].clone();
                let (code, code_value_types, refs) = typecheck_seq(
                    seq,
                    return_type,
                    &mut *self.gcx.typing_context.borrow_mut(),
                    &mut def_stack,
                    self.value_store,
                    &*self.gcx.const_runtime.borrow(),
                    cx.mref,
                    cx.value_idx
                    cx.gcx.span_interner.clone(),
                );

                let meta = self.value_store.metadata(self.value_idx);

                meta.type_id.replace(func_type);
                meta.rib
                    .replace(<_ as FromIterator<(u32, TypeId)>>::from_iter(
                        def_stack
                            .parameters
                            .into_iter()
                            .chain(def_stack.locals.into_iter()),
                    ));

                meta.function.replace(montyc_hlir::Function {
                    type_id: func_type,
                    code,
                    code_value_types,
                    refs
                });

                Ok(func_type)
            }

            Value::Class { name, properties } => {
                log::trace!(
                    "[Value::typecheck(ValueContext)] Typechecking Value::Class {{ name: {:?} }}",
                    name
                );

                let class_value_id = self.value_idx;

                let type_id = {
                    if self.mref == ModuleRef(1) {
                        // builtins

                        let type_id = match name.as_str() {
                            "int" => TypingContext::Int,
                            "str" => TypingContext::Str,
                            "bool" => TypingContext::Bool,
                            "type" => TypingContext::Type,
                            "float" => TypingContext::Float,
                            "object" => TypingContext::Object,
                            "tuple" => TypingContext::UntypedTuple,
                            name => todo!("custom builtin class is not supported yet {:?}", name),
                        };

                        log::trace!("[Value::typecheck(ValueContext)] Setting value = {:?} as class of {:?}", class_value_id, type_id);

                        self.value_store.type_data.insert(type_id, class_value_id);

                        type_id
                    } else {
                        // any other module.
                        todo!();
                    }
                };

                self.value_store
                    .metadata(class_value_id)
                    .type_id
                    .replace(type_id);

                let mut rib = DefScope::default();

                for (key_idx, value_idx) in properties.iter_by_alloc_asc(&self.value_store) {
                    let value = &self
                        .value_store
                        .value_graph
                        .node_weight(value_idx)
                        .unwrap()
                        .clone();

                    let value_type = ValueContext {
                        mref: self.mref,
                        value_store: self.value_store,
                        gcx: self.gcx,
                        code: self.code,
                        value,
                        value_idx,
                    }
                    .typecheck()?;

                    if let Some(key) =
                        self.value_store
                            .value_graph
                            .node_weight(key_idx)
                            .map(|weight| match weight {
                                Value::String(st) => {
                                    montyc_hlir::HostGlue::str_to_spanref(self.gcx, st)
                                }
                                _ => unreachable!(),
                            })
                    {
                        rib.insert(key.group(), value_type);

                        self.value_store
                            .metadata(class_value_id)
                            .rib
                            .replace(rib.clone());
                    }
                }

                Ok(type_id)
            }

            Value::Dict { object: _, data: _ } => todo!(),

            Value::Module { .. } => Ok(TypingContext::Module),
            Value::String(_) => Ok(TypingContext::Str),
            Value::Integer(_) => Ok(TypingContext::Int),
            Value::Object(_) => Ok(TypingContext::Object),
        }
    }
}
