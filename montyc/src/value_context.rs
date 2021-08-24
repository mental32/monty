use montyc_core::{ModuleRef, MontyError, MontyResult, TypeError, TypeId};
use montyc_hlir::{
    typing::{PythonType, TypingContext},
    Const, Dunder, FlatCode, ObjectGraph, ObjectGraphIndex, RawInst, Value,
};

use crate::{
    def_stack::{DefKind, DefScope, DefStack},
    prelude::GlobalContext,
};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub code: &'this FlatCode,
    pub object_graph: &'gcx ObjectGraph,
    pub gcx: &'gcx GlobalContext,
    pub value: &'this Value,
    pub value_idx: ObjectGraphIndex,
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
    pub(crate) fn typecheck(&self) -> MontyResult<TypeId> {
        let cx = self;
        let object_graph = cx.object_graph;

        match cx.value {
            Value::Function {
                ret_t,
                args_t,
                source,
                name,
                ..
            } => {
                let value_id = cx.gcx.value_store.borrow_mut().insert_function(
                    cx.value_idx,
                    cx.mref,
                    None,
                    None,
                    object_graph,
                );

                log::debug!("[ValueContext::typecheck] Typechecking function {:?}", name);

                let mut store = cx.gcx.value_store.borrow_mut();

                store.set_type_of(value_id, Some(TypingContext::UntypedFunc));

                let return_type = {
                    let value = object_graph.node_weight(*ret_t).unwrap();

                    let value_id = store
                        .get_value_from_alloc(object_graph.alloc_id_of(*ret_t).unwrap())
                        .unwrap();

                    if let Value::Class { .. } = value {
                        store
                            .type_of(value_id)
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
                                module: cx.mref.clone(),
                                error: TypeError::DuplicateParameters,
                            });
                        }

                        if !is_dynamically_typed {
                            is_dynamically_typed = kind.is_none();
                        }

                        match kind.clone() {
                            Some(kind_value) => {
                                let kind_alloc_id = object_graph.alloc_id_of(kind_value).unwrap();
                                let kind_value_id =
                                    store.get_value_from_alloc(kind_alloc_id).unwrap();

                                let kind = store.type_of(kind_value_id).unwrap();

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

                let func_type = cx
                    .gcx
                    .typing_context
                    .borrow_mut()
                    .callable(arg_t, return_type);

                store.set_type_of(value_id, Some(func_type));

                // Don't typecheck the bodies of ellipsis-stubbed functions as they are
                // essentially either "extern" declarations or nops.
                let source = source.and_then(|(s_mref, seq)| {
                    (s_mref == cx.mref && !cx.code.is_sequence_ellipsis_stubbed(seq)).then(|| seq)
                });

                let s = match source {
                    Some(seq) => seq,
                    None => return Ok(func_type),
                };

                let mut def_stack = {
                    if cx.mref != ModuleRef(1) {
                        let module_value_id = store.get_module_value(ModuleRef(1));
                        let builtin_rib = store.get_rib_data_of(module_value_id).unwrap();

                        DefStack::new(Some(builtin_rib), Some(DefKind::Builtins))
                    } else {
                        DefStack::new(None, None)
                    }
                };

                {
                    let module = store.get_module_value(cx.mref);
                    let module_rib = store.get_rib_data_of(module).unwrap();

                    def_stack.extend(DefKind::Global, module_rib.into_iter());
                    def_stack.extend(DefKind::Parameters, params.into_iter());
                }

                let seq = cx.code.sequences().get(s).unwrap();
                let mut seq_value_types = vec![TypingContext::Unknown; seq.inst().len()];

                for (ix, inst) in seq.inst().iter().enumerate() {
                    log::trace!("[ValueContext::typecheck] {:?}", inst);

                    match &inst.op {
                        RawInst::Class { .. } | RawInst::Defn { .. } => todo!(),

                        RawInst::UseVar { variable } => {
                            let (var_t, _) = def_stack.get(variable.group()).unwrap();
                            seq_value_types[ix] = var_t;
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

                            let tcx = cx.gcx.typing_context.borrow();

                            if let Some(PythonType::Callable { ret, .. }) =
                                tcx.unify_func(callable_type_id, &args, TypingContext::Unknown)
                            {
                                seq_value_types[ix] = ret;
                            } else {
                                todo!();
                            }
                        }

                        RawInst::SetVar { variable, value } => {
                            let type_id = seq_value_types[*value];
                            assert_ne!(type_id, TypingContext::Unknown);

                            def_stack.add(variable.group(), type_id);
                        }

                        RawInst::GetAttribute { .. } => todo!(),
                        RawInst::SetAttribute { .. } => todo!(),

                        RawInst::GetDunder { object, dunder } => {
                            let mut tcx = cx.gcx.typing_context.borrow_mut();

                            let type_id = seq_value_types[*object];
                            assert_ne!(type_id, TypingContext::Unknown);

                            if tcx.is_tuple(type_id) {
                                let ty = tcx.contextualize(type_id).unwrap();
                                let py_kind = ty.as_python_type();

                                let members = match py_kind {
                                    PythonType::Tuple { members } => {
                                        members.clone().unwrap_or_default()
                                    }
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

                                        tcx.callable(
                                            Some(vec![TypingContext::TSelf, TypingContext::Int]),
                                            ret,
                                        )
                                    }

                                    Dunder::SetItem => todo!(),
                                };

                                continue;
                            }

                            let (_, type_class_value) =
                                store.class_of(type_id, cx.object_graph).unwrap();

                            let dunder_entry = match type_class_value {
                                Value::Class { properties, .. } => {
                                    let hash =
                                        cx.gcx.const_runtime.borrow().hash(format!("{}", dunder));

                                    properties.get(hash)
                                }

                                _ => unreachable!(),
                            };

                            let (_, dunder_index) = dunder_entry.unwrap();
                            let dunder_alloc = object_graph.alloc_id_of(dunder_index).unwrap();
                            let dunder_value = store.get_value_from_alloc(dunder_alloc).unwrap();
                            let dunder_type = store.type_of(dunder_value).unwrap();

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
                            let mut tcx = cx.gcx.typing_context.borrow_mut();

                            seq_value_types[ix] = tcx.tuple(
                                elements.iter().map(|elem| seq_value_types[*elem]).collect(),
                            );

                            let tuple_class = store
                                .class_of(TypingContext::UntypedTuple, object_graph)
                                .unwrap()
                                .0;

                            store.set_class_of(seq_value_types[ix], tuple_class);
                        }

                        RawInst::PhiJump { recv, value } => {
                            let recv = *recv;

                            let value_t = seq_value_types[*value];
                            assert_ne!(value_t, TypingContext::Unknown);

                            let phi_t = seq_value_types[recv];

                            if phi_t == TypingContext::Unknown {
                                seq_value_types[recv] = value_t;
                            } else if phi_t != value_t {
                                seq_value_types[recv] = cx
                                    .gcx
                                    .typing_context
                                    .borrow_mut()
                                    .make_union(phi_t, value_t);
                            }
                        }

                        RawInst::Return { value } => {
                            let type_id = seq_value_types[*value];
                            assert_ne!(type_id, TypingContext::Unknown);

                            let tcx = cx.gcx.typing_context.borrow();

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

                store.set_type_of(value_id, Some(func_type));

                match store.function_rib_stack(value_id) {
                    Ok(_) => unreachable!(),
                    Err(slot) => slot.replace(def_stack),
                };

                Ok(func_type)
            }

            Value::Class { name, properties } => {
                log::trace!(
                    "[Value::typecheck(ValueContext)] Typechecking Value::Class {{ name: {:?} }}",
                    name
                );

                let mut store = cx.gcx.value_store.borrow_mut();

                let alloc_id = object_graph.alloc_id_of(cx.value_idx).unwrap();
                let class_value_id = store.insert(cx.value_idx, alloc_id);
                let type_id = {
                    if cx.mref == ModuleRef(1) {
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

                        log::trace!("[Value::typecheck(ValueContext)] Setting value = {:?} as class of type = {:?}", class_value_id, type_id);

                        store.set_class_of(type_id, class_value_id);

                        type_id
                    } else {
                        // any other module.
                        todo!();
                    }
                };

                store.set_type_of(class_value_id, Some(type_id));
                std::mem::drop(store);

                let mut rib = DefScope::default();

                for (key_idx, value_idx) in properties.iter_by_alloc_asc(object_graph) {
                    let value = object_graph.node_weight(value_idx).unwrap();

                    let value_type = ValueContext {
                        mref: cx.mref,
                        object_graph,
                        gcx: cx.gcx,
                        code: cx.code,
                        value,
                        value_idx,
                    }
                    .typecheck()?;

                    if let Some(key) =
                        object_graph
                            .node_weight(key_idx)
                            .map(|weight| match weight {
                                Value::String(st) => {
                                    montyc_hlir::HostGlue::str_to_spanref(cx.gcx, st)
                                }
                                _ => unreachable!(),
                            })
                    {
                        rib.insert(key.group(), value_type);
                        cx.gcx
                            .value_store
                            .borrow_mut()
                            .set_rib_data_of(class_value_id, Some(rib.clone()));
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
