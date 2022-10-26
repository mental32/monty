use crate::{SpanRef, TypeId, ValueId};

#[derive(Debug, Clone, Copy)]
pub enum Field {
    ByVal(usize),
    Imm(i64),
}

/// Codegen Instruction consumed by codegen lowering passes.
///
/// Previously FlatSeq's and FlatInst's were passed directly to
/// codegen to do its lowering and analysis. This was a bad idea.
///
#[derive(Debug, Clone)]
#[rustfmt::skip]
pub enum CgInst {
    Jump { to: CgBlockId, with: Vec<usize> },
    JumpIfTrue { to: CgBlockId, ctrl: usize },
    JumpIfFalse { to: CgBlockId, ctrl: usize },

    Use { value: ValueId, ret: usize, },
    Call { value: ValueId, args: Vec<usize>, ret: usize },
    Const { cst: crate::ast::Constant, ret: usize },

    FieldLoad { orig: usize, orig_t: TypeId, field: Field, field_t: TypeId, ret: usize },

    ReadLocalVar { var: SpanRef, ret: usize },
    WriteLocalVar { var: SpanRef, orig: usize },

    Alloc { type_id: TypeId, ilist: Vec<usize>, ret: usize },

    Return(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CgBlockId(pub usize);

pub type CgBlockCFG<E = ()> = petgraph::Graph<Vec<CgInst>, E>;
