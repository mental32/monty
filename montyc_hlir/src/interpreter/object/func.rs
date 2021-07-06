use std::cell::RefCell;

use montyc_parser::{AstNode, AstObject};
use petgraph::{graph::NodeIndex, visit::NodeIndexable};

use crate::{
    grapher::NewType,
    interpreter::{
        runtime::eval::{AstExecutor, StackFrame},
        HashKeyT, Runtime,
    },
};

use super::{dict::PyDict, ObjAllocId, PyDictRaw, PyObject, PyResult, RawObject};

pub(in crate::interpreter) type NativeFn =
    for<'rt> fn(&'rt AstExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>;

pub(in crate::interpreter) enum Callable {
    Native(NativeFn),
    BoxedDyn(Box<dyn Fn(&AstExecutor, &[ObjAllocId]) -> PyResult<ObjAllocId>>),
    SourceDef(NodeIndex, NodeIndex),
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
    pub(in crate::interpreter) defsite: Option<NodeIndex>,
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

    fn into_value(&self, rt: &Runtime, object_graph: &mut crate::ObjectGraph) -> crate::Value {
        let properties = self.header.borrow().into_value_dict(rt, object_graph);
        let name = self
            .get_attribute_direct(rt, rt.hash("__name__"), self.alloc_id())
            .map(|obj| rt.try_as_str_value(obj))
            .unwrap()
            .unwrap();

        let mut annotations: PyDictRaw<_> = Default::default();

        let __annotations__ = self
            .get_attribute_direct(rt, rt.hash("__annotations__"), self.alloc_id())
            .unwrap();

        rt.get_object(__annotations__)
            .unwrap()
            .as_any()
            .downcast_ref::<PyDict>()
            .unwrap()
            .for_each(rt, &mut |rt, hash, key, value| {
                let key = key.into_value(rt, object_graph);
                let key = object_graph.add_string_node(
                    if let crate::Value::String(st) = &key {
                        rt.hash(st)
                    } else {
                        unreachable!()
                    },
                    key,
                );

                let value_alloc_id = value.alloc_id();
                let value = value.into_value(rt, object_graph);
                let value = if let crate::Value::String(st) = &value {
                    object_graph.add_string_node(rt.hash(st), value)
                } else {
                    object_graph.add_node_traced(value, value_alloc_id)
                };

                annotations.insert(hash, (key, value));
            });

        crate::Value::Function {
            name,
            annotations,
            properties,
            defsite: self.defsite,
            parent: match self.inner {
                Callable::SourceDef(_, scope) => object_graph
                    .alloc_to_idx
                    .get(&rt.scope_graph.parent_of(scope).unwrap())
                    .or_else(|| panic!())
                    .cloned(),

                _ => None,
            },
        }
    }

    fn call(&self, ex: &mut AstExecutor, args: &[ObjAllocId]) -> PyResult<ObjAllocId> {
        match self.inner {
            Callable::Native(_) => todo!(),
            Callable::BoxedDyn(_) => todo!(),
            Callable::SourceDef(idx, _) => {
                let funcdef = &ex.graph.raw_nodes().get(idx.index()).unwrap().weight;
                let funcdef = match funcdef {
                    AstNode::FuncDef(funcdef) => funcdef,
                    _ => unreachable!(),
                };

                let mut value = None;

                ex.within_subgraph(
                    idx,
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

                        ex.eval_stack.push((idx, StackFrame::Function(self.alloc_id())));

                        while {
                            ex.eval_stack
                                .last()
                                .map(|(_, frame)| matches!(frame, &StackFrame::Function(fid) if fid == self.alloc_id()))
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
