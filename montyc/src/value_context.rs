use std::{cell::RefCell, rc::Rc};

use montyc_core::{ModuleRef, MontyError, MontyResult, TypeError, TypeId};
use montyc_hlir::{typing::TypingContext, HostGlue, ObjectGraph, ObjectGraphIndex, Value};
use montyc_parser::AstNode;
use petgraph::graph::NodeIndex;

use crate::{
    def_stack::{DefKind, DefScope, DefStack},
    prelude::GlobalContext,
    // type_eval::TypeEvalContext,
    typechk::Typecheck,
};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub gcx: &'gcx GlobalContext,
    pub value: &'this Value,
    pub value_idx: ObjectGraphIndex,
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
    /// Get the AST node from the modules body via the given index.
    pub fn get_node_from_module_body(&self, idx: NodeIndex) -> Option<AstNode> {
        unimplemented!()
        // self.gcx
        //     .modules
        //     .get(self.mref)?
        //     .borrow()
        //     .data
        //     .body
        //     .node_weight(idx)
        //     .cloned()
    }
}

impl<'this, 'gcx> Typecheck<ValueContext<'this, 'gcx>, TypeId> for Value {
    fn typecheck(&self, cx: ValueContext) -> MontyResult<TypeId> {
        let object_graph = &cx.gcx.const_runtime.borrow().object_graph;

        match self {
            Value::Function {
                name: _,
                properties: _,
                annotations,
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

                todo!();

                // let defsite = match defsite {
                //     None => return Ok(TypingContext::UntypedFunc),
                //     Some(defsite) => *defsite,
                // };

                // let node = defsite
                //     .subgraph_index
                //     .and_then(|subgraph| object_graph.ast_subgraphs.get(&subgraph))
                //     .and_then(|subgraph| subgraph.node_weight(defsite.node_index).cloned())
                //     .or_else(|| cx.get_node_from_module_body(defsite.node_index))
                //     .unwrap();

                // let funcdef = match node {
                //     AstNode::FuncDef(funcdef) => funcdef,
                //     _ => unreachable!(),
                // };

                // let funcdef_args = funcdef.args.as_ref();
                // let mut def_stack = Rc::new(RefCell::new({
                //     if cx.mref != ModuleRef(1) {
                //         let module_value_id = cx
                //             .gcx
                //             .value_store
                //             .borrow_mut()
                //             .get_module_value(ModuleRef(1));

                //         let builtin_rib = cx
                //             .gcx
                //             .value_store
                //             .borrow_mut()
                //             .get_rib_data_of(module_value_id)
                //             .unwrap();

                //         DefStack::new(Some(builtin_rib), Some(DefKind::Builtins))
                //     } else {
                //         DefStack::new(None, None)
                //     }
                // }));

                // let return_type = if let Some((_, ret_v)) =
                //     annotations.get(cx.gcx.const_runtime.borrow().hash("return"))
                // {
                //     let value = object_graph.node_weight(ret_v).unwrap();

                //     let value_id = cx
                //         .gcx
                //         .value_store
                //         .borrow()
                //         .get_value_from_alloc(object_graph.alloc_id_of(ret_v).unwrap())
                //         .unwrap();

                //     if let Value::Class { .. } = value {
                //         cx.gcx
                //             .value_store
                //             .borrow()
                //             .type_of(value_id)
                //             .expect("class object does not have a type_id.")
                //     } else {
                //         panic!(
                //             "error: only expected class values as return annotations got {:#?}",
                //             value
                //         );
                //     }
                // } else {
                //     TypingContext::None
                // };

                // let mut params =
                //     Vec::with_capacity(funcdef_args.map(|args| args.len()).unwrap_or(0));

                // let arg_t = if let Some(args) = funcdef_args {
                //     let mut seen = ahash::AHashSet::with_capacity(args.len());

                //     // Verify that the parameter names are all unique.
                //     for (arg, kind) in args.iter() {
                //         // true iff the spanref group was already present in the set.
                //         if !seen.insert(arg.group()) {
                //             return Err(MontyError::TypeError {
                //                 module: cx.mref.clone(),
                //                 error: TypeError::DuplicateParameters,
                //             });
                //         }

                //         if let Some(_) = kind.as_ref() {
                //             let arg_name = cx.gcx.spanref_to_str(arg.clone());
                //             let (_, kind_value) = annotations
                //                 .get(cx.gcx.const_runtime.borrow().hash(arg_name))
                //                 .unwrap();

                //             let kind_alloc_id = object_graph.alloc_id_of(kind_value).unwrap();
                //             let kind_value_id = cx
                //                 .gcx
                //                 .value_store
                //                 .borrow()
                //                 .get_value_from_alloc(kind_alloc_id)
                //                 .unwrap();

                //             let kind = cx.gcx.value_store.borrow().type_of(kind_value_id).unwrap();

                //             params.push((arg.group(), kind))
                //         }
                //     }

                //     // Ignore any function definitions with non-annotated arguments.
                //     if funcdef.is_dynamically_typed() {
                //         return Ok(TypingContext::UntypedFunc);
                //     } else {
                //         Some(params.iter().map(|(_, t)| *t).collect())
                //     }
                // } else {
                //     None
                // };

                // let func_type = cx
                //     .gcx
                //     .typing_context
                //     .borrow_mut()
                //     .callable(arg_t, return_type);

                // cx.gcx
                //     .value_store
                //     .borrow_mut()
                //     .set_type_of(value_id, Some(func_type));

                // // Don't typecheck the bodies of ellipsis-stubbed functions as they are
                // // essentially either "extern" declarations or nops.
                // if funcdef.is_ellipsis_stubbed() || parent.is_none() {
                //     return Ok(func_type);
                // }

                // {
                //     let store = cx.gcx.value_store.borrow_mut();
                //     let module = store.get_module_value(cx.mref);
                //     let module_rib = store.get_rib_data_of(module).unwrap();

                //     def_stack.borrow_mut().extend(module_rib.into_iter());
                //     def_stack.borrow_mut().extend(params.into_iter());
                // }

                // for node in funcdef.body.iter() {
                //     node.typecheck(TypeEvalContext {
                //         value_cx: &cx,
                //         expected_return_value: return_type,
                //         def_stack: Rc::clone(&def_stack),
                //     })?;
                // }

                // let mut store = cx.gcx.value_store.borrow_mut();

                // store.set_type_of(value_id, Some(func_type));

                // match store.function_rib_stack(value_id) {
                //     Ok(_) => unreachable!(),
                //     Err(slot) => slot.replace(Rc::make_mut(&mut def_stack).clone().into_inner()),
                // };

                // Ok(func_type)
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

                    let value_type = value.typecheck(ValueContext {
                        mref: cx.mref,
                        gcx: cx.gcx,
                        value,
                        value_idx,
                    })?;

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
