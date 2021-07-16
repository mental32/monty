//! HLIR to represent executable code in a function.
//!

use montyc_core::TypeId;
use petgraph::graph::NodeIndex;

/// An opaque identifier to a basic block in a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct BlockId(NodeIndex);

/// An opaque reference to an SSA value in a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct BlockValueId(NodeIndex);

/// A single instruction found within a block.
#[derive(Debug)]
pub enum Inst {
    /// A nop instruction.
    Nop,
}

/// A basic block is a linear sequence of instructions with no branching.
///
/// The block may have any number of predeccessors and successors and uses
/// EBB parameters to describe the data flow of values instead of Phi(φ) nodes.
///
#[derive(Debug, Default)]
pub struct BasicBlock {
    /// The input parameters of the block.
    pub parameters: Vec<(BlockValueId, TypeId)>,

    /// The list of instructions in the block.
    pub instructions: Vec<Inst>,
}

/// The `Layout` struct determines the layout of blocks and instructions in a function.
#[derive(Debug, Default)]
pub struct Layout(petgraph::graph::DiGraph<BasicBlock, usize>);

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

    /// Add a control flow edge from `from` to `to` with an index to the jump instruction in `from`.
    #[inline]
    pub fn jump(&mut self, from: BlockId, to: BlockId, jump_inst: usize) {
        self.0.add_edge(from.0, to.0, jump_inst);
    }
}

/// A HLIR function which represents a single Python function lowered to a bytecode-like-form.
#[derive(Debug, Default)]
pub struct Function {
    /// The type signature of the function.
    pub type_id: TypeId,

    /// The layout is a directed graph of basic blocks containing instructions.
    pub layout: Layout,
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
