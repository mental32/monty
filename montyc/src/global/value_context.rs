use std::cell::Ref;

use montyc_core::{ModuleRef, MontyError, TypeError};
use montyc_hlir::{ObjectGraph, ObjectGraphIndex, Value, typing::TypingContext};
use montyc_parser::{AstNode, AstObject};

use crate::{prelude::GlobalContext, typechk::Typecheck};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub gcx: &'gcx GlobalContext,
    pub value: &'this Value,
    pub value_idx: ObjectGraphIndex,
    pub object_graph: &'this ObjectGraph,
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
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
                name,
                properties,
                annotations,
                defsite,
                parent,
            } => {
                cx.gcx.value_store.borrow_mut().insert(cx.value_idx);

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

                let return_type = if let Some((_, ret_v)) = annotations.get(cx.gcx.const_runtime.borrow().hash("return")) {
                    let value = cx.object_graph.node_weight(ret_v).unwrap();

                    if let Value::Class { .. } = value {
                        cx.gcx.value_store.borrow().type_of(ret_v).unwrap()
                    } else {
                        panic!("error: only expected class values as return annotations got {:#?}", value);
                    }
                } else {
                    TypingContext::None
                };

                if funcdef.is_ellipsis_stubbed() {
                    return Ok(());
                }

                todo!("func returns {:?}", return_type);

                Ok(())
            }

            montyc_hlir::Value::Class { name, properties } => {
                let mut store = cx.gcx.value_store.borrow_mut();

                store.insert(cx.value_idx);
                store.set_type_of(cx.value_idx, {
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

            _ => Ok(()),
        }
    }
}
