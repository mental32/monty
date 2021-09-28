use crate::{SpanRef, ValueId};

/// Codegen Instruction consumed by codegen lowering passes.
///
/// Previously FlatSeq's and FlatInst's were passed directly to
/// codegen to do its lowering and analysis. This was a bad idea.
///
#[derive(Debug, Clone)]
#[rustfmt::skip]
pub enum CgInst {
    Jump { to: CgBlockId },
    JumpIfTrue { to: CgBlockId, ctrl: usize },
    JumpIfFalse { to: CgBlockId, ctrl: usize },

    Use { value: ValueId, ret: usize, },
    Call { value: ValueId, args: Vec<usize>, ret: usize },
    Const { cst: crate::ast::Constant, ret: usize },

    ReadLocalVar { var: SpanRef, ret: usize },
    WriteLocalVar { var: SpanRef, orig: usize },

    Return(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CgBlockId(pub usize);

pub type CgBlockCFG = petgraph::Graph<Vec<CgInst>, ()>;
