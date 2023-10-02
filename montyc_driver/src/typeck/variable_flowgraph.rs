use montyc_core::MontyResult;
use montyc_flatcode::raw_inst::RawInst;
use petgraph::{graph::NodeIndex, visit::EdgeRef, EdgeDirection};

use crate::{
    cfg_reducer::CFGReducer, session_context::SessionContext, typeck::block_cfg::BlockCFGBuilder,
};

use super::block_cfg::BlockCFG;

#[derive(Debug, Clone)]
pub enum DefPlace<I, N> {
    Defined { at: I },
    DefinedFrom { source: N },
    Deleted { at: I },
}

#[derive(Clone, Debug, Default)]
pub struct VFGNode {
    pub defined: Vec<u32>,
}

type RawVFG = petgraph::graph::Graph<VFGNode, ()>;

#[derive(Debug)]
struct VFGBuilder {
    inp: BlockCFGBuilder,
}

impl crate::cfg_reducer::CFGReducer for VFGBuilder {
    type IndexT = NodeIndex;

    type InputGraphT = BlockCFG;

    type OutputT = RawVFG;

    fn make_output(&self) -> Self::OutputT {
        let mut graph = RawVFG::default();
        self.inp.0.raw_nodes().iter().for_each(|_n| {
            graph.add_node(Default::default());
        });
        graph
    }

    fn cfg_ref(&self) -> &Self::InputGraphT {
        &self.inp.0
    }

    fn cfg_mut_ref(&mut self) -> &mut Self::InputGraphT {
        &mut self.inp.0
    }

    fn visit_block(
        &mut self,
        cx: &crate::session_context::SessionContext,
        output: &mut Self::OutputT,
        ix: Self::IndexT,
        _errors: &mut Vec<montyc_core::error::TypeError>,
    ) -> montyc_core::MontyResult<Vec<Self::IndexT>> {
        let block = self.do_block(cx, ix);

        *output.node_weight_mut(ix).unwrap() = block;
        let edges = self
            .inp
            .0
            .edges_directed(ix, EdgeDirection::Outgoing)
            .map(|e| e.target())
            .collect();

        Ok(edges)
    }
}

impl VFGBuilder {
    fn do_block(&self, _cx: &SessionContext, ix: NodeIndex) -> VFGNode {
        let block = self.inp.0.node_weight(ix).unwrap();
        let mut node = VFGNode::default();

        for inst in block {
            match inst.op {
                RawInst::UseVar { variable } => node.defined.push(variable.group()),
                RawInst::SetVar { variable, .. } => node.defined.push(variable.group()),
                _ => continue,
            }
        }

        node
    }
}

#[derive(Debug)]
pub struct VariableFlowGraph(RawVFG);

impl VariableFlowGraph {
    pub(crate) fn new(cx: &SessionContext, b_cfg: &BlockCFGBuilder) -> MontyResult<Self> {
        let mut builder = VFGBuilder { inp: b_cfg.clone() };
        let vfg = builder.visit(cx, NodeIndex::from(0))?;

        Ok(VariableFlowGraph(vfg))
    }
}
