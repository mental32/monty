//! HLIR to represent executable code in a function.
//!

use ahash::AHashSet;
use montyc_core::{utils::SSAMap, TypeId, ValueId};
use petgraph::graph::{DiGraph, NodeIndex};

/// An opaque identifier to a basic block in a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq, derive_more::From, derive_more::Into)]
pub struct BlockId(NodeIndex);

/// An opaque reference to an SSA value in a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq, derive_more::From, derive_more::Into)]
pub struct SSAValueId(usize);

/// A single instruction found within a block.
#[derive(Debug)]
pub enum Inst {
    /// A nop instruction.
    Nop,

    /// An integer constant.
    IntConst(i64),

    /// Alias a value to a new value.
    Alias(SSAValueId, SSAValueId),

    /// Jump to the given block if the given value is truthy.
    JumpIf(SSAValueId, BlockId),

    /// An unconditional jump.
    Jump(BlockId),

    /// Use a local variable.
    UseVar(u32),

    /// Call a value with the provided SSA values.
    CallValue(ValueId, Vec<SSAValueId>),

    /// Call a value with the provided SSA values.
    CallValueLocal(SSAValueId, Vec<SSAValueId>),
}

#[derive(Debug)]
enum Terminator {
    Jump(BlockId),
    Return(SSAValueId),
}

/// A basic block is a linear sequence of instructions with no branching.
///
/// The block may have any number of predeccessors and successors and uses
/// EBB parameters to describe the data flow of values instead of Phi(φ) nodes.
///
#[derive(Debug, Default)]
pub struct BasicBlock {
    /// The input parameters of the block.
    pub parameters: Vec<(SSAValueId, TypeId)>,

    /// The list of instructions in the block.
    pub instructions: Vec<Inst>,

    /// The terminating instruction of the block.
    pub(self) terminator: Option<Terminator>,
}

impl BasicBlock {
    /// Push a new instruction onto the block and return its index.
    #[inline]
    pub fn push_inst(&mut self, inst: Inst) -> usize {
        self.instructions.push(inst);
        return self.instructions.len() - 1;
    }
}

/// Where did a value come from?
#[derive(Debug)]
pub enum SSAValueDef {
    /// The n'th instruction in a block.
    Inst(BlockId, usize),

    /// The n'th parameter in a block.
    Param(BlockId, usize),
}

impl SSAValueDef {
    /// return the `(block_id, inst_number)` pair from the value def if it's an instruction.
    pub fn inst(&self) -> (BlockId, usize) {
        match self {
            Self::Inst(block, inst) => (*block, *inst),
            _ => unreachable!(),
        }
    }

    /// return the `(block_id, param_number)` pair from the value def if it's an parameter.
    pub fn param(&self) -> (BlockId, usize) {
        match self {
            Self::Param(block, inst) => (*block, *inst),
            _ => unreachable!(),
        }
    }
}

/// Data regaurding an SSA Value.
#[derive(Debug)]
pub struct SSAValueData {
    /// The type of the value.
    pub type_id: TypeId,

    /// Where the value was defined.
    pub value_def: SSAValueDef,
}

#[derive(Debug)]
struct InstId(BlockId, usize);

/// The `Layout` struct determines the layout of blocks and instructions in a function.
#[derive(Debug, Default)]
pub struct Layout(DiGraph<BasicBlock, InstId>);

impl Layout {
    /// Create a new empty layout.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a block to the layout.
    #[inline]
    pub fn add(&mut self) -> BlockId {
        BlockId(self.0.add_node(BasicBlock::default()))
    }

    /// falltrough from `from` to `to`.
    #[inline]
    pub fn fallthrough(&mut self, from: BlockId, to: BlockId) {
        match self
            .get_mut(from)
            .unwrap()
            .terminator
            .replace(Terminator::Jump(to))
        {
            Some(p) => panic!("replaced previous terminator for block {:?}", p),
            None => (),
        }
    }

    /// falltrough from `from` to `to`.
    #[inline]
    pub fn ret(&mut self, block: BlockId, value: SSAValueId) {
        match self
            .get_mut(block)
            .unwrap()
            .terminator
            .replace(Terminator::Return(value))
        {
            Some(p) => panic!("replaced previous terminator for block {:?}", p),
            None => (),
        }
    }

    /// Add a control flow edge from `from` to `to` with an index to the jump instruction in `from`.
    #[inline]
    pub fn jump(&mut self, from: BlockId, to: BlockId, jump_inst: usize) {
        self.0.add_edge(from.0, to.0, InstId(from, jump_inst));
    }

    /// Get a mutable ref to the `BasicBlock` at index `block`.
    #[inline]
    pub fn get_mut(&mut self, block: BlockId) -> Option<&mut BasicBlock> {
        self.0.node_weight_mut(block.0)
    }
}

/// A HLIR function which represents a single Python function lowered to a bytecode-like-form.
#[derive(Debug, Default)]
pub struct Function {
    /// The type signature of the function.
    pub type_id: TypeId,

    /// The layout is a directed graph of basic blocks containing instructions.
    pub layout: Layout,

    /// The list of SSA values in the function.
    pub ssa_values: SSAMap<SSAValueId, SSAValueData>,

    /// A list of external references.
    pub refs: AHashSet<ValueId>,
}

// def f(x: i32):
//     if x > 0:
//         y = x + 1
//         z = y + 1
//         return z
//     else:
//         return 0
//
// Function("f", [("x", i32)]) {
//  bb0(parameters=[(v0, i32)]):
//     v1 = int.const(0)
//     v2 = i32.__gt__(v0, v1)
//     br(v2, bb1(v0), bb2)
//  bb1(parameters=[(v3, i32)]):
//     v4 = int.const(1)
//     v5 = i32.__add__(v3, v4)
//     v6 = i32.__add__(v5, v4)
//     return(v6)
//  bb2(parameters=[]):
//     v8 = int.const(0)
//     return(v8)
// }
//
