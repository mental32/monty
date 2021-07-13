use std::{cell::RefCell, rc::Rc};

use montyc_core::{ModuleRef, MontyError, TypeError};
use montyc_hlir::{typing::TypingContext, ObjectGraph, ObjectGraphIndex, Value};
use montyc_parser::{AstNode, AstObject};

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
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
    /// Get the AST node from the modules body via the given index.
    pub fn get_node_from_body(&self, idx: petgraph::graph::NodeIndex) -> Option<AstNode> {
        self.gcx
            .modules
            .get(self.mref)?
            .borrow()
            .body
            .node_weight(idx)
            .cloned()
    }
}

impl<'this, 'gcx> Typecheck<ValueContext<'this, 'gcx>> for montyc_hlir::Value {
    fn typecheck(&self, cx: ValueContext) -> montyc_core::MontyResult<()> {
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
                );

                let defsite = match defsite {
                    None => return Ok(()),
                    Some(defsite) => *defsite,
                };

                let funcdef = match cx.get_node_from_body(defsite).unwrap() {
                    AstNode::FuncDef(funcdef) => funcdef,
                    _ => unreachable!(),
                };

                let funcdef_args = funcdef.args.as_ref();

                if let Some(args) = funcdef_args {
                    let mut seen = ahash::AHashSet::with_capacity(args.len());

                    // Verify that the parameter names are all unique.
                    for (arg, _) in args.iter() {
                        // true iff the spanref group was already present in the set.
                        if !seen.insert(arg.group()) {
                            return Err(MontyError::TypeError {
                                module: cx.mref.clone(),
                                error: TypeError::DuplicateParameters,
                            });
                        }
                    }

                    // Ignore any function definitions with non-annotated arguments.
                    if funcdef.is_dynamically_typed() {
                        return Ok(());
                    }
                }

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

                // Don't typecheck the bodies of ellipsis-stubbed functions as they are
                // essentially either "extern" declarations or nops.
                if funcdef.is_ellipsis_stubbed() || parent.is_none() {
                    return Ok(());
                }

                let parent = parent.unwrap();

                let ValueContext {
                    mref,
                    gcx,
                    object_graph,
                    ..
                } = cx;

                let ribs = {
                    let mut names = ahash::AHashMap::new();
                    let parent = object_graph.node_weight(parent).unwrap();

                    for (_, (key, value)) in parent.iter() {
                        let key = object_graph
                            .node_weight(*key)
                            .map(|weight| match weight {
                                Value::String(st) => {
                                    montyc_hlir::HostGlue::name_to_spanref(cx.gcx, st)
                                }
                                _ => unreachable!(),
                            })
                            .unwrap();

                        let value_t = match object_graph.node_weight(*value).unwrap() {
                            Value::Module { .. } => TypingContext::Module,
                            Value::String(_) => TypingContext::Str,
                            Value::Integer(_) => TypingContext::Int,
                            _ => TypingContext::Unknown,
                        };

                        names.insert(key.group(), value_t);
                    }

                    Ribs::new(Some(names), Some(RibType::Global))
                };

                let ribs = Rc::new(RefCell::new(ribs));

                for node in funcdef.body.iter() {
                    let node = node.into_ast_node();

                    node.typecheck(TypeEvalContext {
                        mref,
                        gcx,
                        object_graph,
                        expected_return_value: return_type,
                        ribs: Rc::clone(&ribs),
                    })?;
                }

                Ok(())
            }

            montyc_hlir::Value::Class { name, properties: _ } => {
                let mut store = cx.gcx.value_store.borrow_mut();

                let alloc_id = cx.object_graph.alloc_id_of(cx.value_idx).unwrap();
                let value_id = store.insert(cx.value_idx, alloc_id);

                store.set_type_of(value_id, {
                    if cx.mref == ModuleRef(1) {
                        // builtins

                        let ty = match name.as_str() {
                            "int" => TypingContext::Int,
                            "str" => TypingContext::Str,
                            "bool" => TypingContext::Bool,
                            "type" => TypingContext::Type,
                            "float" => TypingContext::Float,
                            "object" => TypingContext::Object,
                            name => todo!("custom builtin class is not supported yet {:?}", name),
                        };

                        Some(ty)
                    } else {
                        // any other module.
                        todo!();
                    }
                });

                Ok(())
            }

            Value::Dict { object: _, data: _ } => todo!(),

            Value::Module { mref: _, properties: _ } => unreachable!(),

            Value::Object(_)
            | Value::String(_)
            | Value::Integer(_) => Ok(()),
        }
    }
}
