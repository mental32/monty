use std::collections::VecDeque;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};

use cranelift_codegen::ir;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    Block, FuncRef, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind,
};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;

use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;
use montyc_core::ast::Constant;
use montyc_core::codegen::{CgBlockId, CgInst};
use montyc_core::{patma, MapT, PythonType, TypeId, TypingConstants, ValueId};
use montyc_flatcode::{raw_inst::RawInst, FlatInst};
use montyc_query::Queries;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::EdgeDirection;

use crate::backend::ir_type_of;
use crate::tvalue::TValue;

trait Allocatable {
    fn alloc_uninit(&self, fx: &mut Builder) -> ir::Value;

    fn initialize<I>(&self, fx: &mut Builder, addr: ir::Value, values: I)
    where
        I: Iterator<Item = TValue<ir::Value>>;
}

impl Allocatable for TypeId {
    fn alloc_uninit(&self, fx: &mut Builder) -> ir::Value {
        let tcx = fx.host.tcx();

        match tcx.get_python_type_of(*self).unwrap() {
            PythonType::NoReturn => todo!(),
            PythonType::Any => todo!(),
            PythonType::Tuple { members } => todo!(),
            PythonType::List { inner } => todo!(),
            PythonType::Union { members } => todo!(),
            PythonType::Type { of } => todo!(),
            PythonType::Instance { of } => todo!(),
            PythonType::TypeVar { name } => todo!(),
            PythonType::Callable { params, ret } => todo!(),
            PythonType::Generic { args } => todo!(),
            PythonType::Builtin { inner } => todo!(),
            // PythonType::NoReturn => todo!(),
            // PythonType::Any => todo!(),

            // PythonType::Tuple { members } => {
            //     let (layout, _) = {
            //         let members = members.clone().unwrap_or_default();

            //         montyc_core::calculate_layout(
            //             &mut members.iter().map(|tid| tcx.layout_of(*tid)),
            //         )
            //     };

            //     assert_ne!(
            //         0,
            //         layout.size(),
            //         "{:?}",
            //         tcx.get_python_type_of(*self).unwrap()
            //     );

            //     let size = fx.inner.ins().iconst(
            //         ir::types::I64,
            //         <_ as TryInto<i64>>::try_into(layout.size()).unwrap(),
            //     );

            //     let malloc_inst = todo!();

            //     patma!(*addr, [addr] in fx.inner.inst_results(malloc_inst)).unwrap()
            // }

            // PythonType::Union { members } => todo!(),
            // PythonType::Type { of } => todo!(),
            // PythonType::TypeVar { name } => todo!(),
            // PythonType::Callable { params, ret } => todo!(),
            // PythonType::Generic { args } => todo!(),
            // PythonType::Builtin { inner } => todo!(),
        }
    }

    fn initialize<I>(&self, fx: &mut Builder, addr: ir::Value, values: I)
    where
        I: Iterator<Item = TValue<ir::Value>>,
    {
        let kind = { fx.host.tcx().get_python_type_of(*self).unwrap().clone() };

        match kind {
            PythonType::NoReturn => todo!(),
            PythonType::Any => todo!(),
            PythonType::Tuple { members } => todo!(),
            PythonType::List { inner } => todo!(),
            PythonType::Union { members } => todo!(),
            PythonType::Type { of } => todo!(),
            PythonType::Instance { of } => todo!(),
            PythonType::TypeVar { name } => todo!(),
            PythonType::Callable { params, ret } => todo!(),
            PythonType::Generic { args } => todo!(),
            PythonType::Builtin { inner } => todo!(),
            // PythonType::NoReturn => todo!(),
            // PythonType::Any => todo!(),

            // PythonType::Tuple { members } => {
            //     let (_, offsets) = {
            //         let members = members.clone().unwrap_or_default();
            //         let tcx = fx.host.tcx();

            //         montyc_core::calculate_layout(
            //             &mut members.iter().map(|tid| tcx.layout_of(*tid)),
            //         )
            //     };

            //     for (ix, elem) in values.into_iter().enumerate() {
            //         let offset = offsets[ix];

            //         fx.ins()
            //             .store(MemFlags::new(), elem.as_value(), addr, offset);
            //     }
            // }

            // PythonType::Union { members } => todo!(),
            // PythonType::Type { of } => todo!(),
            // PythonType::TypeVar { name } => todo!(),
            // PythonType::Callable { args, ret } => todo!(),
            // PythonType::Generic { args } => todo!(),
            // PythonType::Builtin { inner } => todo!(),
        }
    }
}

fn stack_alloc(builder: &mut FunctionBuilder, queries: &dyn Queries, tid: TypeId) -> StackSlot {
    let size = queries.tcx().size_of(tid);

    // FIXME: Don't force the size to a multiple of 16 bytes once
    //        Cranelift gets a way to specify stack slot alignment.
    let slot_size = (size + 15) / 16 * 16;

    log::trace!(
        "[stack_alloc] allocating a stack slot of {:?} bytes for {:?}",
        slot_size,
        tid
    );

    builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size))
}

pub(super) struct Builder<'a, 'b> {
    pub inner: FunctionBuilder<'a>,
    pub cfg: &'b montyc_core::codegen::CgBlockCFG,

    pub values: MapT<usize, TValue<ir::Value>>,
    pub locals: MapT<u32, (StackSlot, TypeId, ir::Type)>,
    pub f_refs: MapT<ValueId, FuncRef>,

    pub host: &'a dyn Queries,
}

impl Builder<'_, '_> {
    pub fn stack_alloc(&mut self, tid: TypeId) -> StackSlot {
        stack_alloc(&mut self.inner, self.host, tid)
    }

    pub fn lower(
        self,
        blocks: MapT<CgBlockId, ir::Block>,
        module: &mut ObjectModule,
        fids: &MapT<ValueId, FuncId>,
        cg_settings: Box<dyn TargetIsa>,
    ) {
        let Self {
            inner: mut builder,
            cfg,
            mut values,
            mut locals,
            mut f_refs,
            host,
            ..
        } = self;

        let mut blocks_processed = vec![];
        let mut block_indecies = VecDeque::from(vec![NodeIndex::new(0)]);

        while let Some(block_ix) = block_indecies.pop_front() {
            log::trace!(
                "[cranelift::builder::Builder::lower] lowering block {:?}",
                &block_ix
            );

            let current_block_id = CgBlockId(block_ix.index());
            let current_block = blocks[&current_block_id];

            if builder.current_block() != Some(current_block) {
                log::trace!(
                    "switching {:?} -> {:?}",
                    builder.current_block(),
                    current_block
                );

                builder.switch_to_block(current_block);
            }

            for inst in &cfg[block_ix] {
                log::trace!("[cranelift::builder::Builder::lower]   {:?}", inst);

                match inst {
                    CgInst::Jump { to } => {
                        builder.ins().jump(blocks[&to], &[]);
                        break;
                    }

                    CgInst::JumpIfTrue { to, ctrl } => {
                        let one = builder.ins().iconst(ir::types::I64, 1);
                        let ctrl = values[ctrl].as_value();

                        builder
                            .ins()
                            .br_icmp(IntCC::Equal, *ctrl, one, blocks[to], &[]);
                    }

                    CgInst::JumpIfFalse { to, ctrl } => {
                        let one = builder.ins().iconst(ir::types::I64, 0);
                        let ctrl = values[ctrl].as_value();

                        builder
                            .ins()
                            .br_icmp(IntCC::Equal, *ctrl, one, blocks[to], &[]);
                    }

                    CgInst::Use { value, ret } => {
                        let f_ref = match f_refs.get(value) {
                            Some(f_ref) => *f_ref,
                            None => {
                                let fid = fids[value];
                                let f_ref = module.declare_func_in_func(fid, builder.func);
                                f_refs.insert(*value, f_ref);
                                f_ref
                            }
                        };

                        let addr = builder.ins().func_addr(cg_settings.pointer_type(), f_ref);

                        let ty = host.get_type_of(*value).unwrap();

                        values.insert(*ret, TValue::reference(addr, ty));
                    }

                    CgInst::Call { value, args, ret } => {
                        let func = match f_refs.get(value) {
                            Some(f) => *f,
                            None => {
                                let fid = fids[value];
                                let f_ref = module.declare_func_in_func(fid, builder.func);
                                f_refs.insert(*value, f_ref);
                                f_ref
                            }
                        };

                        let args = args
                            .iter()
                            .map(|v| *values[v].as_value())
                            .collect::<Vec<_>>();

                        let inst = builder.ins().call(func, &args);

                        let value_t = match host
                            .tcx()
                            .get_python_type_of(host.get_type_of(*value).unwrap())
                            .unwrap()
                        {
                            PythonType::Callable { ret, .. } => ret,
                            _ => unreachable!(),
                        };

                        match builder.inst_results(inst) {
                            [] => {
                                todo!()
                            }

                            [rval] => {
                                values.insert(*ret, TValue::imm(*rval, value_t));
                            }

                            _ => unimplemented!(),
                        }
                    }

                    CgInst::Const { cst, ret } => {
                        let (ssa, const_t) = match cst {
                            Constant::Int(n) => (
                                builder.ins().iconst(ir::types::I64, *n),
                                TypingConstants::Int,
                            ),

                            Constant::Float(n) => (
                                builder.ins().iconst(ir::types::F64, *n as i64),
                                TypingConstants::Float,
                            ),

                            Constant::Bool(b) => (
                                builder.ins().iconst(ir::types::I64, *b as i64),
                                TypingConstants::Bool,
                            ),

                            Constant::String(_) => todo!(),

                            Constant::None => (
                                builder.ins().iconst(ir::types::R64, 0),
                                TypingConstants::None,
                            ),

                            Constant::Ellipsis => todo!(),
                        };

                        values.insert(*ret, TValue::imm(ssa, const_t));
                    }

                    CgInst::ReadLocalVar { var, ret } => {
                        let (ss, ty, _) = locals[&var.group()];
                        let v = builder.ins().stack_load(ir::types::I64, ss, 0);

                        values.insert(*ret, TValue::imm(v, ty));
                    }

                    CgInst::WriteLocalVar { var, orig } => {
                        if !locals.contains_key(&var.group()) {
                            let slot_t = TypingConstants::Int;
                            let ss = stack_alloc(&mut builder, host, slot_t);

                            locals
                                .insert(var.group(), (ss, slot_t, ir_type_of(host.tcx(), slot_t)));
                        }

                        let (ss, slot_t, _) = locals[&var.group()];

                        let dest = builder.ins().stack_addr(ir::types::I64, ss, 0);

                        let val = values[orig];
                        let src = *values[orig].as_value();

                        if host.tcx().is_integer(val.type_id) {
                            builder.ins().stack_store(src, ss, 0);
                        } else {
                            let size = host.tcx().size_of(slot_t);
                            let size = builder
                                .ins()
                                .iconst(ir::types::I64, i64::try_from(size).unwrap());

                            builder.call_memcpy(cg_settings.frontend_config(), dest, src, size);
                        }
                    }

                    CgInst::Return(val) => {
                        let rval = *values[&val].as_value();

                        builder.ins().return_(&[rval]);
                        break;
                    }
                }
            }

            builder.seal_block(current_block);

            blocks_processed.push(block_ix);

            for edge in cfg.edges_directed(block_ix, EdgeDirection::Outgoing) {
                assert_eq!(edge.source(), block_ix);

                if let Ok(_) = blocks_processed.binary_search(&edge.target()) {
                    continue;
                }

                if let None = block_indecies.iter().find(|ix| **ix == edge.target()) {
                    block_indecies.push_back(edge.target())
                }
            }
        }

        builder.finalize();
    }
}

impl<'a> Deref for Builder<'a, '_> {
    type Target = FunctionBuilder<'a>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> DerefMut for Builder<'a, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
