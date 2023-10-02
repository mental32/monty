use std::collections::HashSet;

use montyc_core::{BuiltinType, TypingContext};
use montyc_flatcode::FlatInst;
use montyc_parser::ast::Constant;
use montyc_query::Queries;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::session_context::SessionContext;

use super::block_cfg::BlockCFGBuilder;
use super::*;

/// Akin to Pytype's VM this is a virtual machine for abstract interpretation of code.
#[derive(Debug)]
pub struct TypingMachine {
    pub(crate) cfg: BlockCFG,
    pub(crate) return_t: TypeId,
    pub(crate) entry: Option<NodeIndex>,
    pub(crate) mref: ModuleRef,
    pub(crate) locals: MapT<u32, Variable>,
    pub(crate) nonlocals: MapT<usize, ValueId>,
    pub(crate) values: MapT<usize, TypeId>,
}

impl TypingMachine {
    pub fn new(blocks: Vec<Vec<FlatInst>>, return_t: TypeId, mref: ModuleRef) -> Self {
        let (bcfg, entry) = BlockCFGBuilder::new_from_blocks(blocks);

        Self {
            cfg: bcfg.0,
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
    ) -> MontyResult<Vec<montyc_core::codegen::CgInst<Constant>>> {
        tracing::trace!("analyzing block={:?}", block_ix);

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

        let mref = *mref;

        for inst in block {
            tracing::trace!(" %{} := {}", inst.value, inst.op);

            match &inst.op {
                RawInst::SetAnnotation {
                    name: _,
                    annotation,
                } => {
                    let (inst, _block) = Self::find_inst(cfg, *annotation).unwrap();

                    match inst.op {
                        RawInst::UseVar { .. } => todo!(),
                        _ => todo!("annotation is too complex, can only refer to names."),
                    }
                }

                RawInst::Defn { .. } | RawInst::Class { .. } | RawInst::Import { .. } => {
                    todo!("{:#?}", inst)
                }

                RawInst::Br { to } => {
                    let (_, block) = Self::find_inst(cfg, *to).unwrap();

                    cg_block.push(CgInst::Jump {
                        to: CgBlockId(block.index()),
                        with: vec![],
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
                            with: vec![],
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

                    tracing::trace!("  assigning var {:?} with type {:?}", sref, type_id);

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
                            let module = cx.value_store.get_by_assoc(mref).unwrap();
                            let value = cx
                                .value_store
                                .with_value(module, |m| {
                                    m.properties.get(&variable.group()).cloned()
                                })
                                .unwrap()
                                .unwrap();

                            tracing::trace!("  variable {:?} references {:?}", variable, value);

                            let value_t = cx.get_type_of(value)?;

                            value_types.insert(inst.value, value_t);

                            nonlocals.insert(inst.value, value);

                            cg_block.push(CgInst::Use {
                                value,
                                ret: inst.value,
                            });
                        }

                        Some(Variable(bindings)) => {
                            // let vfg = BlockCFGBuilder(cfg.clone()).variable_flowgraph(cx); // TODO(mental): build this once and cache that.

                            let type_ids =
                                bindings.iter().map(|b| b.type_id).collect::<HashSet<_>>();

                            assert_eq!(type_ids.len(), 1);

                            // panic!("{:#?}", vfg);

                            // let defs: Vec<Defs> = vfg.block(block_ix).unwrap().def_blocks(variable);

                            // defs.foo;
                            //                             let mut bindings_it = Some(block_ix)
                            //                                 .into_iter()
                            //                                 .chain(
                            //                                     cfg.edges_directed(block_ix, EdgeDirection::Incoming)
                            //                                         .map(|e| e.source())
                            //                                         .filter(|ix| *ix < block_ix),
                            //                                 )
                            //                                 .map(|ix| (ix, bindings.iter().find(|b| b.block == ix).cloned()));
                            //
                            //                             let variable_types = if let Some((index, binding)) = bindings_it.next()
                            //                             {
                            //                                 assert_eq!(index, block_ix);
                            //
                            //                                 match binding {
                            //                                     Some(Binding { type_id, .. }) => vec![type_id],
                            //
                            //                                     None => {
                            //                                         // no binding in current block, inspect the immediate predecessors.
                            //
                            //                                         let mut types = ahash::AHashSet::with_capacity(
                            //                                             bindings_it.size_hint().0,
                            //                                         );
                            //
                            //                                         for (ix, binding) in bindings_it {
                            //                                             tracing::trace!("{ix:?} {block_ix:?}");
                            //
                            //                                             match binding {
                            //                                                 Some(Binding { type_id, .. }) => {
                            //                                                     types.insert(type_id);
                            //                                                 }
                            //                                                 None => {
                            //                                                     tracing::trace!("no binding in {ix:?} checking predecessors.");
                            //
                            //                                                     for ix_pred in cfg
                            //                                                         .edges_directed(ix, EdgeDirection::Incoming)
                            //                                                         .map(|e| e.source())
                            //                                                         .filter(|i| *i < ix)
                            //                                                     {
                            //                                                         tracing::trace!("{ix_pred:?} is a predecessor to {ix:?}");
                            //                                                         if let Some(binding) = dbg!(bindings
                            //                                                             .iter()
                            //                                                             .find(|b| b.block == ix_pred)
                            //                                                             .cloned())
                            //                                                         {
                            //                                                             types.insert(binding.type_id);
                            //                                                         }
                            //                                                     }
                            //                                                 }
                            //                                             }
                            //                                         }
                            //
                            //                                         // assert!(n_bound != 0);
                            //                                         types.into_iter().collect()
                            //                                     }
                            //                                 }
                            //                             } else {
                            //                                 todo!("unbound local variable.");
                            //                             };
                            //
                            //                             let var_t = match variable_types.as_slice() {
                            //                                 [] => todo!("unbound local variable."),
                            //
                            //                                 [one] => *one,
                            //
                            //                                 _ => cx.tcx().insert(
                            //                                     PythonType::Union {
                            //                                         members: Some(variable_types),
                            //                                     }
                            //                                     .into(),
                            //                                 ),
                            //                             };
                            value_types.insert(inst.value, type_ids.into_iter().next().unwrap());

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
                        PythonType::Class { .. } => {
                            todo!()
                        }

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

                                    montyc_core::UnifyFailure::BadArgumentTypes(mismatch) => {
                                        for (_, expected, actual) in mismatch {
                                            tracing::error!(
                                                "{:?} != {:?}",
                                                cx.tcx().display_type(expected, &|v| cx
                                                    .get_type_of(v)
                                                    .ok()),
                                                cx.tcx().display_type(actual, &|v| cx
                                                    .get_type_of(v)
                                                    .ok())
                                            );

                                            errors.push(TypeError::BadArgumentType {
                                                expected,
                                                actual,
                                                arg_node: (
                                                    mref,
                                                    inst.attrs.span.clone().unwrap_or_default(),
                                                ),
                                                def_node: (
                                                    mref,
                                                    inst.attrs.span.clone().unwrap_or_default(),
                                                ),
                                            })
                                        }

                                        break;
                                    }

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
                                        access: (mref, inst.attrs.span.clone().unwrap_or_default()),
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

                                        montyc_core::PropertyValue::Builtin(_, _) => {
                                            todo!()
                                        }
                                    }
                                }
                            }
                        }

                        PythonType::Generic { .. } => todo!(),
                        PythonType::Builtin { inner } => match inner {
                            BuiltinType::Type => {
                                let _klass = nonlocals[callable];
                                todo!();
                                // let init = cx.typing_context.get_property(base_t, name)
                            }
                            _ => unimplemented!(),
                        },
                    }
                }

                RawInst::GetAttribute { object, attr } => {
                    let object_t = value_types[object];

                    match Self::find_inst(cfg, *attr).unwrap().0.op {
                        RawInst::RefAsStr { r } => {
                            let attr = cx.spanref_to_str(r)?;
                            let property = cx
                                .typing_context
                                .get_property(object_t, &attr)
                                .ok_or_else(|| TypeError::InvalidAttributeAccess {
                                    base: object_t,
                                    access: (mref, inst.attrs.span.clone().unwrap_or_default()),
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
                            access: (mref, inst.attrs.span.clone().unwrap_or_default()),
                        });

                    let dunder_t = match property {
                        Ok(p) => p.type_id,
                        Err(exc) => {
                            tracing::error!("{exc:?}");
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

                RawInst::PhiJump { recv, value } => {
                    let (_, block) = Self::find_inst(cfg, *recv).unwrap();
                    let to = CgBlockId(block.index());
                    cg_block.push(CgInst::Jump {
                        to,
                        with: vec![*value],
                    });
                }

                RawInst::PhiRecv => todo!(),

                RawInst::Return { value } => {
                    let val_t = value_types[value];

                    if val_t != *return_t {
                        errors.push(TypeError::BadReturnType {
                            expected: *return_t,
                            actual: val_t,
                            ret_node: (mref, inst.attrs.span.clone().unwrap_or_default()),
                            def_node: (mref, inst.attrs.span.clone().unwrap_or_default()), // TODO: use the real def span
                        })
                    } else {
                        cg_block.push(montyc_core::codegen::CgInst::Return(*value));
                    }
                }
            }
        }

        assert!(!cg_block.is_empty());

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

impl crate::cfg_reducer::CFGReducer for TypingMachine {
    type InputGraphT = BlockCFG;
    type OutputT = montyc_core::codegen::CgBlockCFG<Constant, ()>;
    type IndexT = NodeIndex;

    fn make_output(&self) -> Self::OutputT {
        let mut cfg = montyc_core::codegen::CgBlockCFG::new();

        for _ in self.cfg.raw_nodes() {
            cfg.add_node(vec![]);
        }

        cfg
    }

    fn cfg_ref(&self) -> &Self::InputGraphT {
        &self.cfg
    }

    fn cfg_mut_ref(&mut self) -> &mut Self::InputGraphT {
        &mut self.cfg
    }

    fn visit_block(
        &mut self,
        cx: &SessionContext,
        output: &mut Self::OutputT,
        ix: Self::IndexT,
        errors: &mut Vec<montyc_core::error::TypeError>,
    ) -> Result<Vec<NodeIndex>, montyc_core::MontyError> {
        let cg_block = self.analyze_block(cx, ix, errors)?;

        if let Some(block) = output.node_weight_mut(ix) {
            let _ = std::mem::replace(block, cg_block);
        }

        let edges = self
            .cfg
            .edges_directed(ix, EdgeDirection::Outgoing)
            .map(|e| e.target())
            .collect();

        Ok(edges)
    }
}
