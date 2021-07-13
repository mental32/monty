use std::{ops::{Deref, DerefMut}, rc::Rc};

use petgraph::graph::NodeIndex;

use crate::{Value, grapher::AstNodeGraph, interpreter::{self, object::ObjAllocId}};

pub type ObjectGraphIndex = NodeIndex<u32>;

#[derive(Debug, Default)]
pub struct ObjectGraph {
    graph: petgraph::graph::DiGraph<Value, ()>,
    pub ast_subgraphs: ahash::AHashMap<NodeIndex, Rc<AstNodeGraph>>,
    strings: ahash::AHashMap<u64, ObjectGraphIndex>,
    pub(crate) alloc_to_idx: ahash::AHashMap<interpreter::object::ObjAllocId, ObjectGraphIndex>,
}

impl ObjectGraph {
    #[inline]
    pub fn iter_by_alloc_asc(&self) -> impl Iterator<Item = ObjectGraphIndex> {
        let mut allocs: Vec<_> = self.alloc_to_idx.iter().map(|(a, b)| (*a, *b)).collect();

        allocs.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

        allocs.into_iter().map(|(_, b)| b)
    }

    #[inline]
    pub fn alloc_id_of(&self, idx: ObjectGraphIndex) -> Option<ObjAllocId> {
        self.alloc_to_idx
            .iter()
            .find_map(|(alloc, index)| (*index == idx).then(|| *alloc))
    }

    #[inline]
    pub fn add_string_node(&mut self, hash: u64, string: crate::Value) -> ObjectGraphIndex {
        if let Some(idx) = self.strings.get(&hash) {
            return idx.clone();
        }

        let idx = self.graph.add_node(string);

        self.strings.insert(hash, idx);

        idx
    }

    pub(crate) fn add_node_traced(
        &mut self,
        value: crate::Value,
        obj: ObjAllocId,
    ) -> ObjectGraphIndex {
        log::trace!("[ObjectGraph::trace] Tracing value: {:?}", value);

        if let Some(idx) = self.alloc_to_idx.get(&obj) {
            log::trace!("[ObjectGraph::trace] Value already exists! {:?}", idx);
            return *idx;
        }

        let idx = self.graph.add_node(value);
        self.alloc_to_idx.insert(obj, idx);

        log::trace!("[ObjectGraph::trace] {:?} -> {:?}", obj, idx);

        idx
    }
}

impl DerefMut for ObjectGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl Deref for ObjectGraph {
    type Target = petgraph::graph::DiGraph<Value, ()>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}
