#![allow(dead_code)]

use std::collections::VecDeque;

use montyc_core::ast::Constant;
use montyc_core::codegen::{CgBlockId, CgInst};
use montyc_core::{
    Function, MapT, ModuleRef, MontyError, MontyResult, PythonType, TypeError, TypeId,
    TypingConstants, TypingContext, ValueId,
};

use montyc_flatcode::{raw_inst::RawInst, FlatInst};

use montyc_query::Queries;
use petgraph::{data::DataMap, graph::NodeIndex, visit::EdgeRef, EdgeDirection};

use crate::prelude::SessionContext;

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
                log::trace!("[typeck::flatseq_to_blocks] %{} := {}", inst.value, inst.op);
                blocks.push(vec![inst.clone()])
            }

            (_, Some(seq)) => {
                log::trace!(
                    "[typeck::flatseq_to_blocks]     %{} := {}",
                    inst.value,
                    inst.op
                );

                seq.push(inst.clone())
            }
        }

        blocks
    })
}

/// edge-type used to describe how blocks are connected.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum BlockCFGEdge {
    IfTrue(usize),
    IfFalse(usize),
    OrElse(usize),
    Direct(usize),
}

type BlockCFG = petgraph::graph::Graph<Vec<FlatInst>, BlockCFGEdge>;

#[derive(Debug, Default)]
struct BlockCFGBuilder(BlockCFG);

impl BlockCFGBuilder {
    fn new_from_blocks(blocks: Vec<Vec<FlatInst>>) -> (Self, Option<NodeIndex>) {
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

#[derive(Debug, Clone)]
struct Binding {
    block: NodeIndex,
    inst: usize,
    type_id: TypeId,
}

#[derive(Debug, Default)]
struct Variable(Vec<Binding>);

type ErrorTy = TypeError;

/// Akin to Pytype's VM this is a virtual machine for abstract interpretation of code.
#[derive(Debug)]
struct TypingMachine {
    cfg: BlockCFG,
    return_t: TypeId,
    entry: Option<NodeIndex>,
    mref: ModuleRef,
    locals: MapT<u32, Variable>,
    nonlocals: MapT<usize, ValueId>,
    values: MapT<usize, TypeId>,
}

impl TypingMachine {
    fn new(blocks: Vec<Vec<FlatInst>>, return_t: TypeId, mref: ModuleRef) -> Self {
        let (BlockCFGBuilder(cfg), entry) = BlockCFGBuilder::new_from_blocks(blocks);

        Self {
            cfg,
            entry,
            return_t,
            mref,
            locals: MapT::new(),
            nonlocals: MapT::new(),
            values: MapT::new(),
        }
    }

    fn analyze_block(
        &mut self,
        cx: &SessionContext,
        block_ix: NodeIndex,
        errors: &mut Vec<ErrorTy>,
    ) -> MontyResult<Vec<montyc_core::codegen::CgInst>> {
        log::trace!(
            "[TypingMaching::analyze_block] analyzing block={:?}",
            block_ix
        );

        let Self {
            cfg,
            entry: _,
            mref,
            locals,
            nonlocals,
            return_t,
            values: value_types,
        } = self;

        let block = cfg.node_weight(block_ix).ok_or(MontyError::None)?;
        let mut cg_block = vec![];

        for inst in block {
            log::trace!(
                "[TypingMaching::analyze_block]  %{} := {}",
                inst.value,
                inst.op
            );

            match &inst.op {
                RawInst::Defn { .. } | RawInst::Class { .. } | RawInst::Import { .. } => {
                    todo!("{:#?}", inst)
                }

                RawInst::Br { to } => {
                    let (_, block) = Self::find_inst(cfg, *to).unwrap();

                    cg_block.push(CgInst::Jump {
                        to: CgBlockId(block.index()),
                    });
                }

                RawInst::If {
                    test,
                    truthy,
                    falsey,
                } => match (truthy, falsey) {
                    (None, None) => (),
                    (None, Some(target)) => {
                        let (_, block) = Self::find_inst(cfg, *target).unwrap();

                        cg_block.push(CgInst::JumpIfFalse {
                            to: CgBlockId(block.index()),
                            ctrl: *test,
                        })
                    }

                    (Some(target), None) => {
                        let (_, block) = Self::find_inst(cfg, *target).unwrap();

                        cg_block.push(CgInst::JumpIfTrue {
                            to: CgBlockId(block.index()),
                            ctrl: *test,
                        })
                    }

                    (Some(target), Some(orelse)) => {
                        let (_, block) = Self::find_inst(cfg, *target).unwrap();

                        cg_block.push(CgInst::JumpIfTrue {
                            to: CgBlockId(block.index()),
                            ctrl: *test,
                        });

                        let (_, block) = Self::find_inst(cfg, *orelse).unwrap();

                        cg_block.push(CgInst::Jump {
                            to: CgBlockId(block.index()),
                        });
                    }
                },

                RawInst::Undefined
                | RawInst::Nop
                | RawInst::BuildClass { .. }
                | RawInst::JumpTarget => continue,

                RawInst::SetVar { variable, value } => {
                    let sref = variable.clone();
                    let type_id = value_types[value];

                    log::trace!(
                        "[TypingMaching::analyze_block]   assigning var {:?} with type {:?}",
                        sref,
                        type_id
                    );

                    let var = locals.entry(sref.group()).or_default();

                    var.0.push(Binding {
                        block: block_ix,
                        inst: inst.value,
                        type_id,
                    });

                    cg_block.push(CgInst::WriteLocalVar {
                        var: variable.clone(),
                        orig: *value,
                    });
                }

                RawInst::UseVar { variable } => {
                    match locals.get(&variable.group()) {
                        None => {
                            // NOT a local variable.
                            let module = cx.value_store.get_by_assoc(*mref).unwrap();
                            let value = cx
                                .value_store
                                .with_value(module, |m| {
                                    m.properties.get(&variable.group()).cloned()
                                })
                                .unwrap()
                                .unwrap();

                            let value_t = cx.get_type_of(value)?;

                            value_types.insert(inst.value, value_t);

                            nonlocals.insert(inst.value, value);

                            cg_block.push(CgInst::Use {
                                value,
                                ret: inst.value,
                            });
                        }

                        Some(Variable(bindings)) => {
                            // HACK:
                            //     we should be using some kind of separate binding graph
                            //     where the names are nodes and the edges are control flow edges
                            //     so that we can use a dominance algorithm as a rough check whether
                            //     a binding is potentially unbound.
                            //
                            //     but I'm not quite sure how to go about implementing this right now...
                            //     the current implementation works as follows:
                            //
                            //     * given a list of block indecies: the first index is the current block. the rest are the indecies of the blocks immediately preceding it.
                            //     * for every block in the list: find an binding that references it `bindings.iter().find(|b| b.block == ix)`
                            //     * if the first index has a binding, use that. everything is ok.
                            //     * otherwise look at the rest of the indecies and assert they all contain a binding.
                            //     * the result is a vec of types that describes a union of what the type of the variable may be.

                            let mut bindings_it = Some(block_ix)
                                .into_iter()
                                .chain(
                                    cfg.edges_directed(block_ix, EdgeDirection::Incoming)
                                        .map(|e| e.source()),
                                )
                                .map(|ix| (ix, bindings.iter().find(|b| b.block == ix).cloned()));

                            let variable_types = if let Some((index, binding)) = bindings_it.next()
                            {
                                assert_eq!(index, block_ix);

                                match binding {
                                    Some(Binding { type_id, .. }) => vec![type_id],

                                    None => {
                                        // no binding in current block, inspect the immediate predecessors.

                                        let mut types = ahash::AHashSet::with_capacity(
                                            bindings_it.size_hint().0,
                                        );

                                        for (_, binding) in bindings_it {
                                            match binding {
                                            Some(Binding { type_id, .. }) => { types.insert(type_id); },

                                            None => todo!("all immediate predecessors must contain a variable binding."),
                                        }
                                        }

                                        types.into_iter().collect()
                                    }
                                }
                            } else {
                                todo!("unbound local variable.");
                            };

                            let var_t = match variable_types.as_slice() {
                                [] => todo!("unbound local variable."),

                                [one] => *one,

                                _ => cx.tcx().insert(
                                    PythonType::Union {
                                        members: Some(variable_types),
                                    }
                                    .into(),
                                ),
                            };

                            value_types.insert(inst.value, var_t);

                            cg_block.push(CgInst::ReadLocalVar {
                                var: variable.clone(),
                                ret: inst.value,
                            });
                        }
                    }
                }

                RawInst::RefAsStr { .. } => {
                    value_types.insert(inst.value, TypingConstants::Str);
                }

                RawInst::Call {
                    callable,
                    arguments,
                } => {
                    let callable_t = value_types[callable];
                    let callable_pytype = cx.tcx().get_python_type_of(callable_t).unwrap();

                    match callable_pytype {
                        PythonType::Instance { .. } => todo!(),
                        PythonType::NoReturn => todo!(),
                        PythonType::Any => todo!(),

                        PythonType::Union { .. }
                        | PythonType::Tuple { .. }
                        | PythonType::List { .. } => todo!("not callable"),

                        PythonType::Type { .. } => todo!(),
                        PythonType::TypeVar { .. } => todo!(),

                        PythonType::Callable { params: _, ret } => {
                            let argument_types =
                                arguments.iter().map(|a| value_types[a]).collect::<Vec<_>>();

                            if let Err(failure) =
                                cx.tcx().unify_func(callable_t, &argument_types, ret)
                            {
                                match failure {
                                    montyc_core::UnifyFailure::UnequalArity(_, _) => todo!(),
                                    montyc_core::UnifyFailure::BadArgumentTypes(_) => todo!(),
                                    montyc_core::UnifyFailure::NotCallable => unreachable!(),
                                }
                            }

                            value_types.insert(inst.value, ret);

                            let attr = match Self::find_inst(cfg, *callable).unwrap().0.op {
                                RawInst::GetAttribute { attr, object } => {
                                    if let RawInst::RefAsStr { r } =
                                        Self::find_inst(cfg, attr).unwrap().0.op
                                    {
                                        let spanref_to_str = cx.spanref_to_str(r)?.to_string();
                                        let object_t = value_types[&object];

                                        Some((object, object_t, spanref_to_str))
                                    } else {
                                        None
                                    }
                                }

                                RawInst::GetDunder { dunder, object } => {
                                    let object_t = value_types[&object];
                                    Some((object, object_t, format!("{}", dunder)))
                                }

                                RawInst::UseVar { .. } => {
                                    if let Some(value) = nonlocals.get(callable) {
                                        cg_block.push(CgInst::Call {
                                            value: *value,
                                            args: arguments.clone(),
                                            ret: inst.value,
                                        });
                                    }

                                    continue;
                                }

                                _ => None,
                            };

                            if let Some((object, object_t, st)) = attr {
                                let object_pytype = cx.tcx().get_python_type_of(object_t).unwrap();

                                match (object_pytype, st.as_str()) {
                                    (PythonType::Tuple { members }, "__getitem__") => {
                                        let members = members.unwrap_or_default();
                                        let index = arguments[1];

                                        let (field, field_t) =
                                            match Self::find_inst(cfg, index).unwrap().0.op {
                                                RawInst::Const(Constant::Int(n)) => (
                                                    montyc_core::codegen::Field::Imm(n),
                                                    members.get(n as usize).cloned(),
                                                ),

                                                _ => (
                                                    montyc_core::codegen::Field::ByVal(index),
                                                    Some(ret),
                                                ),
                                            };

                                        let field_t = field_t.unwrap_or(TypingConstants::Never);

                                        cg_block.push(CgInst::FieldLoad {
                                            orig: object,
                                            orig_t: object_t,

                                            field,
                                            field_t,

                                            ret: inst.value,
                                        });

                                        continue;
                                    }

                                    _ => (),
                                }

                                if let Ok(property) = cx
                                    .typing_context
                                    .get_property(object_t, &st)
                                    .ok_or_else(|| TypeError::InvalidAttributeAccess {
                                        base: object_t,
                                        access: (
                                            *mref,
                                            inst.attrs.span.clone().unwrap_or_default(),
                                        ),
                                    })
                                {
                                    match property.value {
                                        montyc_core::PropertyValue::Id(value) => {
                                            cg_block.push(CgInst::Call {
                                                value,
                                                args: arguments.clone(),
                                                ret: inst.value,
                                            })
                                        }

                                        montyc_core::PropertyValue::Builtin(kind, slot_name) => {
                                            todo!()
                                        }
                                    }
                                }
                            }
                        }

                        PythonType::Generic { .. } => todo!(),
                        PythonType::Builtin { .. } => todo!(),
                    }
                }

                RawInst::GetAttribute { object, attr } => {
                    let object_t = value_types[object];

                    match Self::find_inst(cfg, *attr).unwrap().0.op {
                        RawInst::RefAsStr { r } => {
                            let attr = cx.spanref_to_str(r)?;
                            let property = cx
                                .typing_context
                                .get_property(object_t, attr)
                                .ok_or_else(|| TypeError::InvalidAttributeAccess {
                                    base: object_t,
                                    access: (*mref, inst.attrs.span.clone().unwrap_or_default()),
                                });

                            let attr_t = match property {
                                Ok(p) => p.type_id,
                                Err(exc) => {
                                    errors.push(exc);
                                    continue;
                                }
                            };

                            value_types.insert(inst.value, attr_t);
                        }

                        _ => todo!(),
                    }
                }

                RawInst::SetAttribute { .. } => todo!(),

                RawInst::GetDunder { object, dunder } => {
                    let object_t = value_types[object];

                    let dunder = format!("{}", dunder);
                    let property = cx
                        .typing_context
                        .get_property(object_t, &dunder)
                        .ok_or_else(|| TypeError::InvalidAttributeAccess {
                            base: object_t,
                            access: (*mref, inst.attrs.span.clone().unwrap_or_default()),
                        });

                    let dunder_t = match property {
                        Ok(p) => p.type_id,
                        Err(exc) => {
                            todo!("{:?}", exc);
                            errors.push(exc);
                            continue;
                        }
                    };

                    value_types.insert(inst.value, dunder_t);
                }

                RawInst::SetDunder { .. } => todo!(),

                RawInst::Const(cst) => {
                    let cst_t = match cst {
                        Constant::Int(_) => TypingConstants::Int,
                        Constant::Float(_) => TypingConstants::Float,
                        Constant::Bool(_) => TypingConstants::Bool,
                        Constant::String(_) => TypingConstants::Str,
                        Constant::None => TypingConstants::None,
                        Constant::Ellipsis => TypingConstants::Ellipsis,
                    };

                    value_types.insert(inst.value, cst_t);

                    let cst = cst.clone();

                    cg_block.push(CgInst::Const {
                        cst,
                        ret: inst.value,
                    });
                }

                RawInst::Tuple(elems) => {
                    let type_id = cx
                        .typing_context
                        .tuple(elems.iter().map(|e| value_types[e]).collect());

                    cg_block.push(CgInst::Alloc {
                        type_id,
                        ilist: elems.to_vec(),
                        ret: inst.value,
                    });

                    value_types.insert(inst.value, type_id);
                }

                RawInst::PhiJump { .. } => todo!(),
                RawInst::PhiRecv => todo!(),

                RawInst::Return { value } => {
                    let val_t = value_types[value];

                    if val_t != *return_t {
                        errors.push(TypeError::BadReturnType {
                            expected: *return_t,
                            actual: val_t,
                            ret_node: (*mref, inst.attrs.span.clone().unwrap_or_default()),
                            def_node: (*mref, inst.attrs.span.clone().unwrap_or_default()), // TODO: use the real def span
                        })
                    } else {
                        cg_block.push(montyc_core::codegen::CgInst::Return(*value));
                    }
                }
            }
        }

        Ok(cg_block)
    }

    pub(crate) fn find_inst(cfg: &BlockCFG, inst_ix: usize) -> Option<(&FlatInst, NodeIndex)> {
        for (ix, node) in cfg.raw_nodes().iter().enumerate() {
            if let Some(inst) = node.weight.iter().find(|i| i.value == inst_ix) {
                return Some((inst, NodeIndex::from(ix as u32)));
            }
        }

        None
    }
}

/// Run the TypingMachine on the supplied function `fun`.
///
/// This routine performs abstract interpretation of the provided function
/// whilst analyzing and performing type correctess verification (typechecking)
///
/// As a side effect from the analysis. extra metadata will be generated to be used
/// for codegen.
///
pub fn typecheck(
    cx: &SessionContext,
    fun: &mut Function,
) -> MontyResult<montyc_core::codegen::CgBlockCFG> {
    log::trace!(
        "[typeck::typecheck] typechecking funtion: {:?}",
        fun.value_id
    );

    let code = cx.get_function_flatcode(fun.value_id)?;

    let (return_t, params_t) = match cx.tcx().get_python_type_of(fun.type_id).unwrap() {
        PythonType::Callable { ret, params } => (ret, params),
        _ => unreachable!(),
    };

    let mut tm = TypingMachine::new(flatseq_to_blocks(code.inst()), return_t, fun.mref);
    let entry = match tm.entry {
        Some(entry) => entry,
        None => unreachable!("code should always have one block."),
    };

    if let Some(montyc_parser::AstNode::FuncDef(f)) = code.ast {
        for ((sref, _), type_id) in f
            .args
            .unwrap_or_default()
            .into_iter()
            .zip(params_t.unwrap_or_default().into_iter())
        {
            let var = tm.locals.entry(sref.group()).or_default();

            var.0.push(Binding {
                block: entry,
                inst: 0,
                type_id,
            });
        }
    }

    let mut blocks_processed = Vec::with_capacity(tm.cfg.raw_nodes().len());
    let mut blocks_to_analyze = VecDeque::with_capacity(tm.cfg.raw_nodes().len());
    blocks_to_analyze.push_front(entry);

    let mut errors = Vec::with_capacity(16);

    let mut cg_cfg = montyc_core::codegen::CgBlockCFG::new();

    for _ in tm.cfg.raw_nodes() {
        cg_cfg.add_node(vec![]);
    }

    while let Some(ix) = blocks_to_analyze.pop_front() {
        errors.clear();

        if let Ok(_) = blocks_processed.binary_search(&ix) {
            continue;
        }

        let cg_block = tm.analyze_block(cx, ix, &mut errors)?;
        assert!(!cg_block.is_empty(), "{:#?}", tm);

        if let [] = errors.as_slice() {
            match blocks_processed.binary_search(&ix) {
                Ok(_) => unreachable!(),
                Err(pos) => blocks_processed.insert(pos, ix),
            }

            if let Some(block) = cg_cfg.node_weight_mut(ix) {
                let _ = std::mem::replace(block, cg_block);
            }

            for edge in tm.cfg.edges_directed(ix, EdgeDirection::Outgoing) {
                assert_eq!(edge.source(), ix);

                if let Ok(_) = blocks_processed.binary_search(&edge.target()) {
                    continue;
                }

                if let None = blocks_to_analyze.iter().find(|ix| **ix == edge.target()) {
                    blocks_to_analyze.push_back(edge.target())
                }
            }

            log::trace!(
                "[typeck::typecheck] blocks to analyze: {:?}",
                blocks_to_analyze
            );
        } else {
            todo!("{:#?}", errors)
        }
    }

    for edge in tm.cfg.edge_indices() {
        if let Some((start, end)) = tm.cfg.edge_endpoints(edge) {
            cg_cfg.update_edge(start, end, ());
        }
    }

    Ok(cg_cfg)
}
