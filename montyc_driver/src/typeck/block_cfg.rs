use crate::global_context::SessionContext;

use super::cfg_reducer::GraphLike;
use super::variable_flowgraph::VariableFlowGraph;
use super::{FlatInst, NodeIndex, RawInst};

pub(crate) type BlockCFG = petgraph::graph::Graph<Vec<FlatInst>, BlockCFGEdge>;

impl GraphLike<NodeIndex> for BlockCFG {
    fn n_nodes(&self) -> usize {
        self.raw_nodes().len()
    }
}

/// edge-type used to describe how blocks are connected.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum BlockCFGEdge {
    IfTrue(usize),
    IfFalse(usize),
    OrElse(usize),
    Direct(usize),
}

#[derive(Debug, Default, Clone)]
pub(crate) struct BlockCFGBuilder(pub BlockCFG);

impl BlockCFGBuilder {
    pub fn new_from_blocks(blocks: Vec<Vec<FlatInst>>) -> (Self, Option<NodeIndex>) {
        let mut cfg = BlockCFGBuilder::default();
        let mut edges = vec![];

        let mut block_indecies = cfg.append(blocks.iter()).peekable();
        let entry = block_indecies.peek().cloned();

        for block_index in block_indecies {
            let block = &blocks[block_index.index()];

            for (inst_ix, inst) in block.iter().enumerate() {
                match inst.op {
                    RawInst::If { truthy, falsey, .. } => {
                        if let Some(t) = truthy {
                            edges.push((block_index, t, BlockCFGEdge::IfTrue(inst_ix)));
                        }

                        if let Some(f) = falsey {
                            edges.push((block_index, f, BlockCFGEdge::IfFalse(inst_ix)));
                        }
                    }

                    RawInst::Br { to } => {
                        if inst_ix > 0
                            && matches!(block[inst_ix - 1].op, RawInst::If { falsey: None, .. })
                        {
                            edges.push((block_index, to, BlockCFGEdge::OrElse(inst_ix)));
                        } else {
                            edges.push((block_index, to, BlockCFGEdge::Direct(inst_ix)));
                        }
                    }

                    _ => continue,
                }
            }
        }

        for (from, to, edge) in edges {
            let to = cfg.find_block_of_val(to).unwrap();
            cfg.connect_nodes(from, to, edge);
        }

        (cfg, entry)
    }

    #[inline]
    pub fn variable_flowgraph(&self, cx: &SessionContext) -> VariableFlowGraph {
        VariableFlowGraph::new(cx, self).unwrap()
    }

    fn append<'a, I, T>(&'a mut self, it: I) -> impl Iterator<Item = NodeIndex> + 'a
    where
        I: Iterator<Item = T> + 'a,
        T: AsRef<[FlatInst]>,
    {
        it.map(move |block| self.0.add_node(block.as_ref().to_owned()))
    }

    fn find_block_of_val(&self, value: usize) -> Option<NodeIndex> {
        self.0.node_weights().enumerate().find_map(|(ix, seq)| {
            seq.iter()
                .any(|inst| inst.value == value)
                .then(|| NodeIndex::from(ix as u32))
        })
    }

    fn connect_nodes(&mut self, from: NodeIndex, to: NodeIndex, edge: BlockCFGEdge) {
        self.0.add_edge(from, to, edge);
    }
}
