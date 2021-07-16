use std::{cell::RefCell, rc::Rc};

use montyc_core::{ModuleRef, MontyError, MontyResult, TypeError, TypeId};
use montyc_hlir::{typing::TypingContext, ObjectGraph, ObjectGraphIndex, Value};
use montyc_parser::AstNode;
use petgraph::graph::NodeIndex;

use crate::{
    prelude::GlobalContext,
    ribs::{RibType, Ribs},
    typechk::{tyeval::TypeEvalContext, Typecheck},
};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub gcx: &'gcx GlobalContext,
    pub value: &'this Value,
    pub value_idx: ObjectGraphIndex,
    pub object_graph: &'this ObjectGraph,
    pub object_graph_index: usize,
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
    /// Get the AST node from the modules body via the given index.
    pub fn get_node_from_module_body(&self, idx: NodeIndex) -> Option<AstNode> {
        self.gcx
            .modules
            .get(self.mref)?
            .borrow()
            .body
            .node_weight(idx)
            .cloned()
    }
}

impl<'this, 'gcx> Typecheck<ValueContext<'this, 'gcx>, TypeId> for montyc_hlir::Value {
    fn typecheck(&self, cx: ValueContext) -> MontyResult<TypeId> {
        match self {
            montyc_hlir::Value::Function {
                name: _,
                properties: _,
                annotations,
                defsite,
                parent,
            } => {
                cx.gcx.value_store.borrow_mut().insert(
                    cx.value_idx,
                    cx.object_graph.alloc_id_of(cx.value_idx).unwrap(),
                    cx.object_graph_index,
                );

                let defsite = match defsite {
                    None => return Ok(TypingContext::UntypedFunc),
                    Some(defsite) => *defsite,
                };

                let node = defsite
                    .graph_index
                    .and_then(|subgraph| cx.object_graph.ast_subgraphs.get(&subgraph))
                    .and_then(|subgraph| {
                        subgraph
                            .node_weight(defsite.node_index_within_graph)
                            .cloned()
                    })
                    .or_else(|| cx.get_node_from_module_body(defsite.node_index_within_graph))
                    .unwrap();

                let funcdef = match node {
                    AstNode::FuncDef(funcdef) => funcdef,
                    _ => unreachable!(),
                };

                let funcdef_args = funcdef.args.as_ref();
                let ribs = Rc::new(RefCell::new({
                    if cx.mref != ModuleRef(1) {
                        let builtin_rib = cx
                            .gcx
                            .value_store
                            .borrow_mut()
                            .get_rib_data_of(ModuleRef(1))
                            .unwrap();

                        Ribs::new(Some(builtin_rib), Some(RibType::Builtins))
                    } else {
                        Ribs::new(None, None)
                    }
                }));

                let return_type = if let Some((_, ret_v)) =
                    annotations.get(cx.gcx.const_runtime.borrow().hash("return"))
                {
                    let value = cx.object_graph.node_weight(ret_v).unwrap();

                    let value_id = cx
                        .gcx
                        .value_store
                        .borrow()
                        .get_value_from_alloc(cx.object_graph.alloc_id_of(ret_v).unwrap())
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
                } else {
                    TypingContext::None
                };

                let mut params =
                    Vec::with_capacity(funcdef_args.map(|args| args.len()).unwrap_or(0));

                let arg_t = if let Some(args) = funcdef_args {
                    let mut seen = ahash::AHashSet::with_capacity(args.len());

                    // Verify that the parameter names are all unique.
                    for (arg, kind) in args.iter() {
                        // true iff the spanref group was already present in the set.
                        if !seen.insert(arg.group()) {
                            return Err(MontyError::TypeError {
                                module: cx.mref.clone(),
                                error: TypeError::DuplicateParameters,
                            });
                        }

                        if let Some(kind) = kind.as_ref() {
                            let kind = kind
                                .typecheck(TypeEvalContext {
                                    value_cx: &cx,
                                    expected_return_value: return_type,
                                    ribs: Rc::clone(&ribs),
                                })?
                                .unwrap();

                            params.push((arg.group(), kind))
                        }
                    }

                    // Ignore any function definitions with non-annotated arguments.
                    if funcdef.is_dynamically_typed() {
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

                // Don't typecheck the bodies of ellipsis-stubbed functions as they are
                // essentially either "extern" declarations or nops.
                if funcdef.is_ellipsis_stubbed() || parent.is_none() {
                    return Ok(func_type);
                }

                {
                    let module_rib = cx
                        .gcx
                        .value_store
                        .borrow_mut()
                        .get_rib_data_of(cx.mref)
                        .unwrap();

                    ribs.borrow_mut().extend(module_rib.into_iter());
                    ribs.borrow_mut().extend(params.into_iter());
                }

                for node in funcdef.body.iter() {
                    node.typecheck(TypeEvalContext {
                        value_cx: &cx,
                        expected_return_value: return_type,
                        ribs: Rc::clone(&ribs),
                    })?;
                }

                Ok(func_type)
            }

            montyc_hlir::Value::Class {
                name,
                properties: _,
            } => {
                log::trace!(
                    "[Value::typecheck(ValueContext)] Typechecking Value::Class {{ name: {:?} }}",
                    name
                );

                let mut store = cx.gcx.value_store.borrow_mut();

                let alloc_id = cx.object_graph.alloc_id_of(cx.value_idx).unwrap();
                let value_id = store.insert(cx.value_idx, alloc_id, cx.object_graph_index);
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

                        log::trace!("[Value::typecheck(ValueContext)] Setting value = {:?} as class of type = {:?}", value_id, type_id);

                        store.set_class_of(type_id, value_id);

                        Some(type_id)
                    } else {
                        // any other module.
                        todo!();
                    }
                };

                store.set_type_of(value_id, type_id);

                Ok(store.type_of(value_id).unwrap())
            }

            Value::Dict { object: _, data: _ } => todo!(),

            Value::Module { .. } => Ok(TypingContext::Module),
            Value::String(_) => Ok(TypingContext::Str),
            Value::Integer(_) => Ok(TypingContext::Int),
            Value::Object(_) => Ok(TypingContext::Object),
        }
    }
}
