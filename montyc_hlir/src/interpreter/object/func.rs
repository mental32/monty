use std::cell::RefCell;

use montyc_parser::{AstNode, AstObject};
use petgraph::{graph::NodeIndex, visit::NodeIndexable};

use crate::{
    grapher::NewType,
    interpreter::{
        runtime::eval::{AstExecutor, StackFrame, UniqueNodeIndex},
        HashKeyT, Runtime,
    },
    ObjectGraphIndex, Value,
};

use super::{ObjAllocId, PyObject, PyResult, RawObject};

pub(in crate::interpreter) type NativeFn =
    for<'rt> fn(&'rt AstExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>;

#[allow(dead_code)] // TODO: Remove once it is used.
pub(in crate::interpreter) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&AstExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef {
        source_index: UniqueNodeIndex,
        scope_index: NodeIndex,
    },
    Object(ObjAllocId),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

#[derive(Debug)]
pub(in crate::interpreter) struct Function {
    pub(in crate::interpreter) header: RefCell<RawObject>,
    pub(in crate::interpreter) inner: Callable,
    pub(in crate::interpreter) defsite: Option<UniqueNodeIndex>,
}

impl PyObject for Function {
    fn alloc_id(&self) -> ObjAllocId {
        self.header.borrow().alloc_id
    }

    fn set_attribute_direct(
        &mut self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
        value: ObjAllocId,
    ) {
        self.header
            .borrow_mut()
            .set_attribute_direct(rt, hash, key, value)
    }

    fn get_attribute_direct(
        &self,
        rt: &Runtime,
        hash: HashKeyT,
        key: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.header.borrow().get_attribute_direct(rt, hash, key)
    }

    fn for_each(
        &self,
        rt: &Runtime,
        f: &mut dyn FnMut(&Runtime, HashKeyT, ObjAllocId, ObjAllocId),
    ) {
        self.header.borrow().for_each(rt, f)
    }

    fn into_value(&self, rt: &Runtime, object_graph: &mut crate::ObjectGraph) -> ObjectGraphIndex {
        if let Some(idx) = object_graph.alloc_to_idx.get(&self.alloc_id()).cloned() {
            return idx;
        }

        object_graph.insert_node_traced(
            self.alloc_id(),
            |object_graph, _| {
                let name = self
                    .get_attribute_direct(rt, rt.hash("__name__"), self.alloc_id())
                    .map(|obj| rt.try_as_str_value(obj))
                    .unwrap()
                    .unwrap();

                Value::Function {
                    name,
                    annotations: Default::default(),
                    properties: Default::default(),
                    defsite: self.defsite,
                    parent: match self.inner {
                        Callable::SourceDef { scope_index, .. } => object_graph
                            .alloc_to_idx
                            .get(&rt.scope_graph.parent_of(scope_index).unwrap())
                            .or_else(|| {
                                rt.scope_graph
                                    .parent_of(scope_index)
                                    .unwrap()
                                    .as_ref(rt, |v| panic!("{:?}", v))
                            })
                            .cloned(),

                        _ => None,
                    },
                }
            },
            |object_graph, value| {
                let p = self.header.borrow().into_value_dict(rt, object_graph);

                let mut ann = Default::default();
                self.get_attribute_direct(rt, rt.hash("__annotations__"), self.alloc_id())
                    .unwrap()
                    .properties_into_values(rt, object_graph, &mut ann);

                let (properties, annotations) = if let Value::Function {
                    properties,
                    annotations,
                    ..
                } = value(object_graph)
                {
                    (properties, annotations)
                } else {
                    unreachable!()
                };

                *properties = p;
                *annotations = ann;
            },
        )
    }

    fn call(&self, ex: &mut AstExecutor, args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
        match self.inner {
            Callable::Native(_) => todo!(),
            Callable::BoxedDyn(_) => todo!(),
            Callable::SourceDef {
                source_index,
                scope_index,
            } => {
                let funcdef = source_index.to_node(ex).unwrap();
                let funcdef = match funcdef {
                    AstNode::FuncDef(funcdef) => funcdef.clone(),
                    _ => unreachable!(),
                };

                let mut value = None;

                ex.within_subgraph(
                    source_index.subgraph_index.unwrap(),
                    || NewType(funcdef.clone()).into(),
                    |ex, graph| {
                        if let Some(params) = &funcdef.args {
                            for (arg, (param, _)) in args.iter().zip(params) {
                                let param = ex.host.spanref_to_str(*param);
                                let (key, hash) = ex.string(param)?;

                                self.header.borrow_mut().set_attribute_direct(
                                    &ex.runtime,
                                    hash,
                                    key,
                                    *arg,
                                );
                            }
                        }

                        let raw_nodes = graph.raw_nodes();
                        let mut node_index = graph.from_index(0);

                        ex.eval_stack.push((scope_index, source_index, StackFrame::Function(self.alloc_id())));

                        while {
                            ex.eval_stack
                                .last()
                                .map(|(_, _, frame)| matches!(frame, &StackFrame::Function(fid) if fid == self.alloc_id()))
                                .unwrap_or(false)
                        } {
                            let node = if node_index == NodeIndex::<u32>::end() {
                                break;
                            } else {
                                &raw_nodes[node_index.index()].weight
                            };

                            value = node.visit_with(ex)?;

                            if let AstNode::Ret(_) = node {
                                break;
                            } else {
                                node_index = graph
                                    .neighbors_directed(node_index, petgraph::EdgeDirection::Outgoing)
                                    .next()
                                    .unwrap_or(NodeIndex::end());
                            }
                        }

                        Ok(())
                    },
                )?;

                Ok(value.unwrap())
            }

            Callable::Object(_) => todo!(),
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
