use montyc_core::{ModuleRef, MontyError, MontyResult, TypeError, TypeId};
use montyc_hlir::{
    flatcode::{
        raw_inst::{Const, Dunder, RawInst},
        FlatCode,
    },
    typing::{PythonType, TypingContext},
    ObjectGraph, ObjectGraphIndex, Value,
};
use montyc_parser::ast::InfixOp;

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

        match dbg!(cx.value) {
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

                cx.gcx
                    .value_store
                    .borrow_mut()
                    .set_type_of(value_id, Some(TypingContext::UntypedFunc));

                let return_type = {
                    let value = object_graph.node_weight(*ret_t).unwrap();

                    let value_id = cx
                        .gcx
                        .value_store
                        .borrow()
                        .get_value_from_alloc(object_graph.alloc_id_of(*ret_t).unwrap())
                        .unwrap();

                    if let Value::Class { .. } = value {
                        cx.gcx
                            .value_store
                            .borrow()
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

                        is_dynamically_typed = kind.is_none();

                        match kind.clone() {
                            Some(kind_value) => {
                                let kind_alloc_id = object_graph.alloc_id_of(kind_value).unwrap();
                                let kind_value_id = cx
                                    .gcx
                                    .value_store
                                    .borrow()
                                    .get_value_from_alloc(kind_alloc_id)
                                    .unwrap();

                                let kind =
                                    cx.gcx.value_store.borrow().type_of(kind_value_id).unwrap();

                                params.push((arg.group(), kind))
                            }

                            None => continue,
                        }
                    }

                    // Ignore any function definitions with non-annotated arguments.
                    if is_dynamically_typed {
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
                    .callable(dbg!(arg_t), return_type);

                cx.gcx
                    .value_store
                    .borrow_mut()
                    .set_type_of(value_id, Some(func_type));

                // Don't typecheck the bodies of ellipsis-stubbed functions as they are
                // essentially either "extern" declarations or nops.
                let source = source.and_then(|(s_mref, seq)| {
                    (s_mref == cx.mref && cx.code.is_sequence_ellipsis_stubbed(seq)).then(|| seq)
                });

                let s = match source {
                    Some(seq) => seq,
                    None => return Ok(func_type),
                };

                let mut def_stack = {
                    if cx.mref != ModuleRef(1) {
                        let module_value_id = cx
                            .gcx
                            .value_store
                            .borrow_mut()
                            .get_module_value(ModuleRef(1));

                        let builtin_rib = cx
                            .gcx
                            .value_store
                            .borrow_mut()
                            .get_rib_data_of(module_value_id)
                            .unwrap();

                        DefStack::new(Some(builtin_rib), Some(DefKind::Builtins))
                    } else {
                        DefStack::new(None, None)
                    }
                };

                let mut store = cx.gcx.value_store.borrow_mut();

                {
                    let module = store.get_module_value(cx.mref);
                    let module_rib = store.get_rib_data_of(module).unwrap();

                    def_stack.extend(DefKind::Global, module_rib.into_iter());
                    def_stack.extend(DefKind::Parameters, params.into_iter());
                }

                let seq = cx.code.sequences().get(s).unwrap();
                let mut seq_value_types = vec![TypingContext::Unknown; seq.inst().len()];

                for (ix, inst) in seq.inst().iter().enumerate() {
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

                            let arguments_types: Vec<_> = arguments
                                .iter()
                                .map(|i| {
                                    let ty = seq_value_types[*i];
                                    assert_ne!(callable_type_id, TypingContext::Unknown);
                                    ty
                                })
                                .collect();

                            let tcx = cx.gcx.typing_context.borrow();
                            let callable_type = tcx.contextualize(callable_type_id).unwrap();

                            seq_value_types[ix] =
                                if let PythonType::Callable { args: params, ret } =
                                    callable_type.as_python_type()
                                {
                                    match (params, arguments_types.as_slice()) {
                                        (None, []) => (),
                                        (Some(params), args @ [_, ..]) => {
                                            assert_eq!(params, args)
                                        }

                                        (Some(_params), []) => todo!(),
                                        (None, _args) => todo!(),
                                    }

                                    *ret
                                } else {
                                    todo!("Not callable.");
                                };
                        }

                        RawInst::SetVar { variable, value } => todo!(),

                        RawInst::GetAttribute { object, name } => todo!(),

                        RawInst::GetDunder { object, dunder } => {
                            let type_id = seq_value_types[*object];
                            assert_ne!(type_id, TypingContext::Unknown);

                            let (_, type_class_value) =
                                store.class_of(type_id, cx.object_graph).unwrap();

                            let dunder_entry = match type_class_value {
                                Value::Class { properties, .. } => match dunder {
                                    Dunder::Unary(_) => todo!(),

                                    Dunder::Infix(op) => match op {
                                        InfixOp::Add => todo!(),
                                        InfixOp::Sub => todo!(),
                                        InfixOp::Power => todo!(),
                                        InfixOp::Invert => todo!(),
                                        InfixOp::FloorDiv => todo!(),
                                        InfixOp::MatMult => todo!(),
                                        InfixOp::Mod => todo!(),
                                        InfixOp::Div => todo!(),
                                        InfixOp::Mult => todo!(),
                                        InfixOp::LeftShift => todo!(),
                                        InfixOp::RightShift => todo!(),
                                        InfixOp::NotEq => todo!(),

                                        InfixOp::Eq => properties
                                            .get(cx.gcx.const_runtime.borrow().hash("__eq__")),

                                        InfixOp::And => todo!(),
                                        InfixOp::Or => todo!(),
                                        InfixOp::Xor => todo!(),
                                    },

                                    Dunder::DocComment => todo!(),
                                },

                                _ => unreachable!(),
                            };

                            let (_, dunder_index) = dunder_entry.unwrap();
                            let dunder_alloc = object_graph.alloc_id_of(dunder_index).unwrap();
                            let dunder_value = store.get_value_from_alloc(dunder_alloc).unwrap();
                            let dunder_type = store.type_of(dunder_value).unwrap();

                            seq_value_types[ix] = dunder_type;
                        }

                        RawInst::SetAttribute {
                            object,
                            name,
                            value,
                        } => todo!(),

                        RawInst::SetDunder {
                            object,
                            dunder,
                            value,
                        } => todo!(),

                        RawInst::GetItem { object, index } => todo!(),
                        RawInst::SetItem {
                            object,
                            index,
                            value,
                        } => todo!(),

                        RawInst::Import { path, relative } => todo!(),

                        RawInst::Const(const_) => {
                            let ty = match const_ {
                                Const::Int(_) => TypingContext::Int,
                                Const::Float(_) => TypingContext::Float,
                                Const::Bool(_) => TypingContext::Bool,
                                Const::String(_) => TypingContext::Str,
                                Const::None => TypingContext::None,
                                Const::Ellipsis => TypingContext::None,
                            };

                            seq_value_types[ix] = ty;
                        }
                        RawInst::Tuple(_) => todo!(),
                        RawInst::Nop => todo!(),
                        RawInst::Undefined => todo!(),
                        RawInst::If {
                            test,
                            truthy,
                            falsey,
                        } => todo!(),

                        RawInst::Br { to } => todo!(),
                        RawInst::PhiJump { recv, value } => todo!(),
                        RawInst::PhiRecv => todo!(),
                        RawInst::Return { value } => todo!(),
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
