use std::cell::Ref;

use montyc_core::{ModuleRef, MontyError, TypeError};
use montyc_hlir::Value;
use montyc_parser::{AstNode, AstObject};

use crate::{prelude::GlobalContext, typechk::Typecheck};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub gcx: &'gcx GlobalContext,
    pub value: &'this Value,
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
            } => {
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
                    if args.iter().any(|(_, ann)| ann.is_none()) {
                        return Ok(());
                    }
                }

                let ret = dbg!(dbg!(annotations).get(dbg!(cx.gcx.const_runtime.borrow().hash("return"))));

                // for stmt in funcdef.body.iter() {
                //     let node = stmt.inner.into_ast_node();
                //     dbg!(node);
                // }

                Ok(())
            }

            _ => Ok(()),
        }
    }
}
