use std::{
    ops::{Deref, DerefMut},
    rc::Rc,
    usize,
};

use ahash::AHashMap;
use petgraph::graph::{DiGraph, NodeIndex};

use crate::{
    grapher::AstNodeGraph,
    interpreter::{self, object::ObjAllocId},
    Value,
};

/// An index into the `ObjectGraph`.
pub type ObjectGraphIndex = NodeIndex<u32>;

/// A graph of objects in the program.
#[derive(Debug, Default)]
pub struct ObjectGraph {
    /// Subgraphs of the object graph, these are made up of smaller graphs of the bodies of class and function definitions.
    pub ast_subgraphs: AHashMap<NodeIndex, Rc<AstNodeGraph>>,

    pending_nodes: usize,

    pub(crate) graph: DiGraph<Value, ()>,
    pub(crate) strings: AHashMap<u64, ObjectGraphIndex>,
    pub(crate) alloc_to_idx: AHashMap<ObjAllocId, ObjectGraphIndex>,
}

impl ObjectGraph {
    /// Produce an iterator over all Object indecies in the graph from the oldest to the newest allocation.
    #[inline]
    pub fn iter_by_alloc_asc(&self) -> impl Iterator<Item = ObjectGraphIndex> {
        let mut allocs: Vec<_> = self.alloc_to_idx.iter().map(|(a, b)| (*a, *b)).collect();

        allocs.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

        allocs.into_iter().map(|(_, b)| b)
    }

    /// Get the allocation id for a given object index.
    #[inline]
    pub fn alloc_id_of(&self, idx: ObjectGraphIndex) -> Option<ObjAllocId> {
        self.alloc_to_idx
            .iter()
            .find_map(|(alloc, index)| (*index == idx).then(|| *alloc))
    }

    pub(crate) fn insert_node_traced(
        &mut self,
        alloc_id: ObjAllocId,
        mut make_value: impl FnOnce(&mut Self, ObjectGraphIndex) -> Value,
        mut mutate_value: impl FnOnce(&mut Self, &dyn Fn(&mut Self) -> &mut Value),
    ) -> ObjectGraphIndex {
        if let Some(idx) = self.alloc_to_idx.get(&alloc_id) {
            log::trace!(
                "[ObjectGraph::insert_node_traced] Value already exists! {:?}",
                idx
            );
            return *idx;
        }

        let current_nodes = self.graph.raw_nodes().len();

        if self.pending_nodes <= self.graph.raw_nodes().len() {
            self.pending_nodes = current_nodes;
        } else {
            self.pending_nodes += 1;
        }

        let index = NodeIndex::new(self.pending_nodes);
        self.alloc_to_idx.insert(alloc_id, index);

        let value = make_value(self, index);

        let real_index = self.graph.add_node(value);

        assert_eq!(real_index, index, "Expected index did not match the pre-computed index, was something added to the graph whilst the Value was being created?");

        mutate_value(self, &|s| s.graph.node_weight_mut(real_index).unwrap());

        real_index
    }
}

impl DerefMut for ObjectGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl Deref for ObjectGraph {
    type Target = DiGraph<Value, ()>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}
