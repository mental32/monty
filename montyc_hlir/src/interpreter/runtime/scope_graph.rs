use std::ops::{Deref, DerefMut};

use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::NodeIndexable,
    EdgeDirection::Outgoing,
};

use crate::ObjAllocId;

#[derive(Debug)]
pub(crate) struct ScopeIndex(NodeIndex);

impl Default for ScopeIndex {
    fn default() -> Self {
        Self(NodeIndex::end())
    }
}

#[derive(Debug, Default)]
pub(in crate::interpreter) struct ScopeGraph(DiGraph<ObjAllocId, ()>);

impl DerefMut for ScopeGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Deref for ScopeGraph {
    type Target = DiGraph<ObjAllocId, ()>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ScopeGraph {
    #[inline]
    pub fn insert(&mut self, node: ObjAllocId) -> NodeIndex<u32> {
        self.0.add_node(node)
    }

    #[inline]
    pub fn nest(&mut self, child: NodeIndex<u32>, parent: NodeIndex<u32>) {
        assert_ne!(child, parent, "Can not nest scopes recursively.");
        self.0.add_edge(child, parent, ());
    }

    #[inline]
    pub(in crate::interpreter) fn parent_of(&self, idx: NodeIndex<u32>) -> Option<ObjAllocId> {
        self.0
            .neighbors_directed(idx, Outgoing)
            .next()
            .and_then(|idx| self.0.node_weight(idx))
            .cloned()
    }

    #[inline]
    pub fn search(
        &self,
        start: ObjAllocId,
        f: impl Fn(ObjAllocId) -> Option<ObjAllocId>,
    ) -> Option<ObjAllocId> {
        let nodes = self.0.raw_nodes();
        let index = {
            nodes
                .iter()
                .enumerate()
                .find(|(_, node)| node.weight == start)?
                .0
        };

        let mut node = nodes[index].weight;
        let mut index = self.0.from_index(index);

        loop {
            if let node @ Some(_) = f(node) {
                break node;
            }

            let parent = self
                .0
                .neighbors_directed(index, petgraph::EdgeDirection::Outgoing)
                .next()?;

            index = parent;
            node = nodes[index.index()].weight;
        }
    }
}
