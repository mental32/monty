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
        I: Iterator<Item = crate::tvalue::TValue<ir::Value>>;
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
        I: Iterator<Item = crate::tvalue::TValue<ir::Value>>,
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

pub fn stack_alloc(builder: &mut FunctionBuilder, queries: &dyn Queries, tid: TypeId) -> StackSlot {
    let size = queries.tcx().size_of(tid);

    // FIXME: Don't force the size to a multiple of 16 bytes once
    //        Cranelift gets a way to specify stack slot alignment.
    let slot_size = (size + 15) / 16 * 16;

    builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size))
}

pub(super) struct Builder<'a, 'b> {
    pub inner: FunctionBuilder<'a>,
    pub cfg: &'b montyc_core::codegen::CgBlockCFG,

    pub values: MapT<usize, crate::tvalue::TValue<ir::Value>>,
    pub locals: MapT<u32, (StackSlot, TypeId, ir::Type)>,
    pub f_refs: MapT<ValueId, FuncRef>,

    pub size_cache: MapT<TypeId, u32>,

    pub host: &'a dyn Queries,
}

impl Builder<'_, '_> {
    pub fn lower(
        mut self,
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
            size_cache,
            host,
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

            builder.switch_to_block(current_block);

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

                    CgInst::Use { value, ret } => todo!(),

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

                        match builder.inst_results(inst) {
                            [] => {
                                todo!()
                            }

                            [rval] => {
                                values.insert(*ret, TValue::imm(*rval, TypingConstants::Unknown));
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

                            Constant::Float(_) => todo!(),

                            Constant::Bool(b) => (
                                builder.ins().iconst(ir::types::I64, *b as i64),
                                TypingConstants::Bool,
                            ),

                            Constant::String(_) => todo!(),
                            Constant::None => todo!(),
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
                        let src = *values[orig].as_value();

                        let size = host.tcx().size_of(slot_t);
                        let size = builder
                            .ins()
                            .iconst(ir::types::I64, i64::try_from(size).unwrap());

                        // builder.call_memcpy(cg_settings.frontend_config(), dest, src, size);
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

        // for inst in code {
        //     log::trace!("[BuilderContext::build] building block for inst {:?}", inst);

        //     if self.inner.current_block() != Some(block) {
        //         assert!(self.inner.is_filled(), "{:?}", self.inner.func);
        //         self.inner.switch_to_block(block);
        //     }

        //     match inst.op.clone() {
        //         RawInst::Nop
        //         | RawInst::Import { .. }
        //         | RawInst::Class { .. }
        //         | RawInst::Privileged(PrivInst::TreatAsStructPointer { .. })
        //         | RawInst::Defn { .. } => continue,

        //         RawInst::Privileged(p) => {
        //             let (val, ty) = match p {
        //                 PrivInst::UseLocal { var } => {
        //                     let (ss, tid, ty) = self.locals.get(&var.group()).unwrap();

        //                     (self.inner.ins().stack_load(*ty, *ss, 0), tid)
        //                 }

        //                 PrivInst::RefVal { val } => {
        //                     todo!();
        //                 }

        //                 PrivInst::CallVal { val: _ } => todo!(),

        //                 PrivInst::TreatAsStructPointer { .. } => unreachable!(),

        //                 PrivInst::AccessMemberPointer {
        //                     value,
        //                     offset: logical_offset,
        //                 } => {
        //                     let base_ptr = self.values[&value].as_value();

        //                     let tcx = self.host.tcx();
        //                     let type_id = code[value].attrs.type_id.unwrap();

        //                     let ptr =
        //                         if let RawInst::Const(Constant::Int(n)) = code[logical_offset].op {
        //                             let (_, offsets) = if let PythonType::Tuple { members } =
        //                                 tcx.get_python_type_of(type_id).unwrap()
        //                             {
        //                                 let members = members.clone().unwrap_or_default();
        //                                 montyc_core::calculate_layout(
        //                                     &mut members.iter().map(|tid| tcx.layout_of(*tid)),
        //                                 )
        //                             } else {
        //                                 todo!();
        //                             };

        //                             let n = if n < 0 { -n } else { n } as usize;

        //                             let byte_offset = offsets[n];

        //                             self.inner.ins().load(
        //                                 ir::types::I64,
        //                                 MemFlags::new(),
        //                                 base_ptr,
        //                                 byte_offset,
        //                             )
        //                         } else {
        //                             todo!()
        //                         };

        //                     self.values
        //                         .insert(inst.value, TValue::reference(ptr, type_id));

        //                     continue;
        //                 }
        //             };

        //             self.values.insert(inst.value, TValue::imm(val, *ty));
        //         }

        //         RawInst::Call {
        //             callable,
        //             arguments,
        //         } => {
        //             let f_val = match code[callable].op {
        //                 RawInst::Privileged(PrivInst::RefVal { val }) => val,
        //                 _ => unimplemented!(),
        //             };

        //             let mut args = arguments
        //                 .iter()
        //                 .map(|arg| self.values[arg].as_value())
        //                 .collect::<Vec<_>>();

        //             let f_ref = *f_refs.get(&f_val).unwrap();

        //             {
        //                 let dfg = &self.inner.func.dfg;
        //                 let ex_func = &dfg.ext_funcs[f_ref];
        //                 let ex_func_sig = &dfg.signatures[ex_func.signature];

        //                 if let Some(param) = ex_func_sig.params.get(0) {
        //                     if param.value_type == ir::types::R64 {
        //                         args[0] = self.inner.ins().raw_bitcast(ir::types::R64, args[0]);
        //                     }
        //                 }
        //             }

        //             let f_inst = self.inner.ins().call(f_ref, &args);

        //             match self.inner.inst_results(f_inst) {
        //                 [res] => {
        //                     self.values
        //                         .insert(inst.value, TValue::imm(*res, TypingConstants::Unknown));
        //                 }

        //                 _ => todo!(),
        //             };
        //         }

        //         RawInst::SetVar { variable, value } => {
        //             let (ss, _, _) = self.locals.get(&variable.group()).unwrap();

        //             let val = self.values[&value].as_value();

        //             self.inner.ins().stack_store(val, *ss, 0);
        //         }

        //         RawInst::UseVar { .. } => unreachable!(),
        //         RawInst::GetDunder { .. } => unreachable!(),

        //         RawInst::GetAttribute { object: _, attr: _ } => todo!(),

        //         RawInst::SetAttribute {
        //             object: _,
        //             attr: _,
        //             value: _,
        //         } => todo!(),

        //         RawInst::SetDunder {
        //             object: _,
        //             dunder: _,
        //             value: _,
        //         } => todo!(),

        //         RawInst::Const(cst) => {
        //             let (val, ty) = match cst {
        //                 Constant::Int(i) => (
        //                     self.inner.ins().iconst(ir::types::I64, i),
        //                     TypingConstants::Int,
        //                 ),

        //                 Constant::Float(f) => {
        //                     (self.inner.ins().f64const(f), TypingConstants::Float)
        //                 }

        //                 Constant::Bool(i) => (
        //                     self.inner.ins().bconst(ir::types::B64, i),
        //                     TypingConstants::Bool,
        //                 ),

        //                 Constant::String(_) => todo!(),
        //                 Constant::None => todo!(),
        //                 Constant::Ellipsis => todo!(),
        //             };

        //             self.values.insert(inst.value, TValue::imm(val, ty));
        //         }

        //         RawInst::Tuple(elements) => {
        //             let tuple_ty = code[inst.value].attrs.type_id.unwrap();

        //             let addr = tuple_ty.alloc_uninit(&mut self);
        //             let mut values = elements
        //                 .to_owned()
        //                 .into_iter()
        //                 .map(|val| self.values[val].clone())
        //                 .collect::<Vec<_>>();

        //             tuple_ty.initialize(&mut self, addr, values.drain(..));

        //             self.values
        //                 .insert(inst.value, TValue::reference(addr, tuple_ty));
        //         }

        //         RawInst::PhiRecv => {
        //             block = blocks[&inst.value];

        //             if !self.inner.is_filled() {
        //                 self.inner.ins().jump(block, &[]);
        //             }

        //             let arg = match self.inner.block_params(blocks[&inst.value]) {
        //                 [arg] => *arg,
        //                 _ => unreachable!(),
        //             };

        //             self.values
        //                 .insert(inst.value, TValue::imm(arg, TypingConstants::Unknown));
        //         }

        //         RawInst::JumpTarget => {
        //             block = blocks[&inst.value];

        //             if !self.inner.is_filled() {
        //                 self.inner.ins().jump(block, &[]);
        //             }
        //         }

        //         RawInst::Undefined => {
        //             todo!();
        //         }

        //         RawInst::If {
        //             test,
        //             truthy,
        //             falsey,
        //         } => {
        //             let test = self.values[&test].as_value();

        //             let fx = match (truthy, falsey) {
        //                 (None, None) => continue,
        //                 (None, Some(ix)) => {
        //                     self.inner.ins().brz(test, blocks[&ix], &[]);
        //                     inst.value + 1
        //                 }

        //                 (Some(ix), None) => {
        //                     self.inner.ins().brnz(test, blocks[&ix], &[]);
        //                     inst.value + 1
        //                 }

        //                 (Some(tx), Some(fx)) => {
        //                     self.inner.ins().brnz(test, blocks[&tx], &[]);
        //                     fx
        //                 }
        //             };

        //             self.inner.ins().jump(blocks[&fx], &[]);
        //         }

        //         RawInst::Br { to } => {
        //             self.inner.ins().jump(blocks[&to], &[]);
        //         }

        //         RawInst::PhiJump { recv, value } => {
        //             let ty = code[value].attrs.type_id.unwrap();
        //             let ty = self.module.get_scalar_type_of(ty);

        //             if self.inner.block_params(blocks[&recv]).is_empty() {
        //                 self.inner.append_block_param(blocks[&recv], ty);
        //             }

        //             let input = self.values[&value];

        //             self.inner.ins().jump(blocks[&recv], &[input.as_value()]);
        //         }

        //         RawInst::Return { value } => {
        //             let rval = *self.values.get(&value).unwrap();

        //             self.inner.ins().return_(&[rval.as_value()]);
        //         }

        //         RawInst::RefAsStr { r } => todo!(),
        //     };
        // }
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
