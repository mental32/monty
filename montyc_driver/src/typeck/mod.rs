#![allow(dead_code)]

use montyc_core::codegen::{CgBlockId, CgInst};

use montyc_core::{
    Function, MapT, ModuleRef, MontyError, MontyResult, PythonType, TypeError, TypeId,
    TypingConstants, ValueId,
};

use montyc_flatcode::{raw_inst::RawInst, FlatInst};

use montyc_parser::ast::{AstNode, Constant};
use montyc_query::Queries;
use petgraph::{data::DataMap, graph::NodeIndex, EdgeDirection};

use crate::cfg_reducer::CFGReducer;
use crate::session_context::SessionContext;
use crate::session_context::SessionMode;

mod block_cfg;
mod typing_machine;
mod variable_flowgraph;

use block_cfg::BlockCFG;
use typing_machine::TypingMachine;

/// run through a sequence of instructions and group linear sequences of instructions into "blocks"
///
/// blocks typically have a jump-target or phi-recv as the first instruction (unless its the first block.)
/// and should end with a branch instruction, it is permitted for branching instructions to exist
/// in between the first and last instruction in a block.
///
fn flatseq_to_blocks(inst: &[FlatInst]) -> Vec<Vec<FlatInst>> {
    inst.iter().fold(vec![], |mut blocks, inst| {
        match (&inst.op, blocks.last_mut()) {
            (_, None) | (RawInst::JumpTarget | RawInst::PhiRecv, _) => {
                tracing::trace!("%{} := {}", inst.value, inst.op);
                blocks.push(vec![inst.clone()])
            }

            (_, Some(seq)) => {
                tracing::trace!("    %{} := {}", inst.value, inst.op);

                seq.push(inst.clone())
            }
        }

        blocks
    })
}

#[derive(Debug, Clone)]
struct Binding {
    block: NodeIndex,
    inst: usize,
    type_id: TypeId,
}

#[derive(Debug, Default)]
pub(crate) struct Variable(Vec<Binding>);

type ErrorTy = TypeError;

/// Run the TypingMachine on the supplied function `fun`.
///
/// This routine performs abstract interpretation of the provided function
/// whilst analyzing and performing type correctess verification (typechecking)
///
/// As a side effect if the context build options are set to `Build` then `CgInst`
/// instructions will be emitted for later codegen.
///
pub fn typecheck(
    cx: &SessionContext,
    fun: &mut Function,
) -> MontyResult<montyc_core::codegen::CgBlockCFG<Constant>> {
    tracing::trace!("typechecking funtion: {:?}", fun.value_id);

    let code = cx.get_function_flatcode(fun.value_id)?;

    match &cx.opts.mode {
        SessionMode::Check => todo!(),
        SessionMode::Build => {
            let (return_t, params_t) = match cx.tcx().get_python_type_of(fun.type_id).unwrap() {
                PythonType::Callable { ret, params } => (ret, params),
                _ => unreachable!(),
            };

            let mut tm = TypingMachine::new(flatseq_to_blocks(code.inst()), return_t, fun.mref);
            let entry = match tm.entry {
                Some(entry) => entry,
                None => unreachable!("code should always have one block."),
            };

            if let Some(AstNode::FuncDef(f)) = code.ast {
                for (param, type_id) in f
                    .args
                    .into_iter()
                    .zip(params_t.unwrap_or_default().into_iter())
                {
                    let var = tm.locals.entry(param.inner.named.group()).or_default();

                    var.0.push(Binding {
                        block: entry,
                        inst: 0,
                        type_id,
                    });
                }
            }

            let mut cg_cfg = tm.visit(cx, entry)?;

            for edge in tm.cfg.edge_indices() {
                if let Some((start, end)) = tm.cfg.edge_endpoints(edge) {
                    cg_cfg.update_edge(start, end, ());
                }
            }

            Ok(cg_cfg)
        }
    }
}
