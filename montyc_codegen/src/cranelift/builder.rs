use std::collections::VecDeque;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, ExternalName};
use cranelift_codegen::ir::{
    FuncRef, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind,
};
use cranelift_codegen::isa::TargetIsa;

use cranelift_frontend::FunctionBuilder;
use cranelift_module::{DataContext, FuncId, Module};
use cranelift_object::ObjectModule;

use montyc_core::ast::Constant;
use montyc_core::codegen::{CgBlockId, CgInst, Field};
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
        I: IntoIterator<Item = TValue<ir::Value>>;
}

impl Allocatable for TypeId {
    fn alloc_uninit(&self, fx: &mut Builder) -> ir::Value {
        let tcx = fx.host.tcx();

        match tcx.get_python_type_of(*self).unwrap() {
            PythonType::NoReturn => todo!(),
            PythonType::Any => todo!(),
            PythonType::Tuple { members } => {
                let (layout, _) = {
                    let members = members.clone().unwrap_or_default();

                    montyc_core::calculate_layout(
                        &mut members.iter().map(|tid| tcx.layout_of(*tid)),
                    )
                };

                assert_ne!(
                    0,
                    layout.size(),
                    "{:?}",
                    tcx.get_python_type_of(*self).unwrap()
                );

                let malloc_inst = fx.heap_alloc(
                    <_ as std::convert::TryInto<i64>>::try_into(layout.size()).unwrap(),
                );

                patma!(*addr, [addr] in fx.inner.inst_results(malloc_inst)).unwrap()
            }
            PythonType::List { inner } => todo!(),
            PythonType::Union { members } => todo!(),
            PythonType::Type { of } => todo!(),
            PythonType::Instance { of } => todo!(),
            PythonType::TypeVar { name } => todo!(),
            PythonType::Callable { params, ret } => todo!(),
            PythonType::Generic { args } => todo!(),
            PythonType::Builtin { inner } => todo!(),
        }
    }

    fn initialize<I>(&self, fx: &mut Builder, addr: ir::Value, values: I)
    where
        I: IntoIterator<Item = TValue<ir::Value>>,
    {
        let kind = { fx.host.tcx().get_python_type_of(*self).unwrap().clone() };

        match kind {
            PythonType::NoReturn => todo!(),
            PythonType::Any => todo!(),
            PythonType::Tuple { members } => {
                let (_, offsets) = {
                    let members = members.clone().unwrap_or_default();
                    let tcx = fx.host.tcx();

                    montyc_core::calculate_layout(
                        &mut members.iter().map(|tid| tcx.layout_of(*tid)),
                    )
                };

                for (ix, elem) in values.into_iter().enumerate() {
                    let offset = offsets[ix];

                    fx.ins()
                        .store(MemFlags::new(), *elem.as_value(), addr, offset);
                }
            }
            PythonType::List { inner } => todo!(),
            PythonType::Union { members } => todo!(),
            PythonType::Type { of } => todo!(),
            PythonType::Instance { of } => todo!(),
            PythonType::TypeVar { name } => todo!(),
            PythonType::Callable { params, ret } => todo!(),
            PythonType::Generic { args } => todo!(),
            PythonType::Builtin { inner } => todo!(),
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
    pub alloc: ExternalName,
    pub cg_settings: Box<dyn TargetIsa>,
}

impl Builder<'_, '_> {
    pub fn heap_alloc(&mut self, size: i64) -> ir::Inst {
        let pointer_type = self.cg_settings.pointer_type();
        let signature = self.inner.import_signature(signature!(
            self.cg_settings.default_call_conv(),
            &[ir::types::I64],
            pointer_type
        ));

        let alloc = self.inner.import_function(ir::ExtFuncData {
            name: self.alloc.clone(),
            signature,
            colocated: false,
        });

        let size = self.inner.ins().iconst(ir::types::I64, size);

        self.ins().call(alloc, &[size])
    }

    pub fn stack_alloc(&mut self, tid: TypeId) -> StackSlot {
        stack_alloc(&mut self.inner, self.host, tid)
    }

    pub fn lower(
        mut self,
        blocks: MapT<CgBlockId, ir::Block>,
        module: &mut ObjectModule,
        fids: &MapT<ValueId, FuncId>,
        cg_settings: Box<dyn TargetIsa>,
    ) {
        let mut blocks_processed = vec![];
        let mut block_indecies = VecDeque::from(vec![NodeIndex::new(0)]);

        while let Some(block_ix) = block_indecies.pop_front() {
            log::trace!(
                "[cranelift::builder::Builder::lower] lowering block {:?}",
                &block_ix
            );

            let current_block_id = CgBlockId(block_ix.index());
            let current_block = blocks[&current_block_id];

            if self.inner.current_block() != Some(current_block) {
                log::trace!(
                    "switching {:?} -> {:?}",
                    self.inner.current_block(),
                    current_block
                );

                self.inner.switch_to_block(current_block);
            }

            for inst in &self.cfg[block_ix] {
                log::trace!("[cranelift::builder::Builder::lower]   {:?}", inst);

                match inst {
                    CgInst::Alloc {
                        type_id,
                        ilist,
                        ret,
                    } => {
                        let addr = type_id.alloc_uninit(&mut self);

                        let values = self.values.clone();

                        type_id.initialize(
                            &mut self,
                            addr,
                            ilist.iter().map(move |v| values[v].clone()),
                        );

                        self.values.insert(*ret, TValue::reference(addr, *type_id));
                    }

                    CgInst::FieldLoad {
                        orig,
                        orig_t,
                        field,
                        field_t,
                        ret,
                    } => {
                        let orig = self.values[orig];
                        let origin_type = self.host.tcx().get_python_type_of(*orig_t).unwrap();

                        match origin_type {
                            PythonType::Tuple { members } => {
                                let (_, offsets) = {
                                    let members = members.clone().unwrap_or_default();
                                    let tcx = self.host.tcx();

                                    montyc_core::calculate_layout(
                                        &mut members.iter().map(|tid| tcx.layout_of(*tid)),
                                    )
                                };

                                match field {
                                    Field::ByVal(_) => todo!(),
                                    Field::Imm(offset) => {
                                        let offset = offsets[*offset as usize];

                                        let field = self.inner.ins().load(
                                            ir::types::I64,
                                            MemFlags::new(),
                                            *orig.as_value(),
                                            offset,
                                        );

                                        self.values.insert(*ret, TValue::imm(field, *field_t));
                                    }
                                }
                            }

                            _ => todo!(),
                        }
                    }

                    CgInst::Jump { to } => {
                        self.inner.ins().jump(blocks[&to], &[]);
                        break;
                    }

                    CgInst::JumpIfTrue { to, ctrl } => {
                        let one = self.inner.ins().iconst(ir::types::I64, 1);
                        let ctrl = self.values[ctrl].as_value();

                        self.inner
                            .ins()
                            .br_icmp(IntCC::Equal, *ctrl, one, blocks[to], &[]);
                    }

                    CgInst::JumpIfFalse { to, ctrl } => {
                        let one = self.inner.ins().iconst(ir::types::I64, 0);
                        let ctrl = self.values[ctrl].as_value();

                        self.inner
                            .ins()
                            .br_icmp(IntCC::Equal, *ctrl, one, blocks[to], &[]);
                    }

                    CgInst::Use { value, ret } => {
                        let f_ref = match self.f_refs.get(value) {
                            Some(f_ref) => *f_ref,
                            None => {
                                let fid = fids[value];
                                let f_ref = module.declare_func_in_func(fid, self.inner.func);
                                self.f_refs.insert(*value, f_ref);
                                f_ref
                            }
                        };

                        let addr = self
                            .inner
                            .ins()
                            .func_addr(cg_settings.pointer_type(), f_ref);

                        let ty = self.host.get_type_of(*value).unwrap();

                        self.values.insert(*ret, TValue::reference(addr, ty));
                    }

                    CgInst::Call { value, args, ret } => {
                        let func = match self.f_refs.get(value) {
                            Some(f) => *f,
                            None => {
                                let fid = fids[value];
                                let f_ref = module.declare_func_in_func(fid, self.inner.func);
                                self.f_refs.insert(*value, f_ref);
                                f_ref
                            }
                        };

                        let args = args
                            .iter()
                            .map(|v| *self.values[v].as_value())
                            .collect::<Vec<_>>();

                        let inst = self.inner.ins().call(func, &args);

                        let type_id = self.host.get_type_of(*value).unwrap();
                        let value_t = match self.host.tcx().get_python_type_of(type_id).unwrap() {
                            PythonType::Callable { ret, .. } => ret,
                            _ => unreachable!(),
                        };

                        match self.inner.inst_results(inst) {
                            [] => {
                                todo!()
                            }

                            [rval] => {
                                self.values.insert(*ret, TValue::imm(*rval, value_t));
                            }

                            _ => unimplemented!(),
                        }
                    }

                    CgInst::Const { cst, ret } => {
                        let (ssa, const_t) = match cst {
                            Constant::Int(n) => (
                                self.inner.ins().iconst(ir::types::I64, *n),
                                TypingConstants::Int,
                            ),

                            Constant::Float(n) => (
                                self.inner.ins().iconst(ir::types::F64, *n as i64),
                                TypingConstants::Float,
                            ),

                            Constant::Bool(b) => (
                                self.inner.ins().iconst(ir::types::I64, *b as i64),
                                TypingConstants::Bool,
                            ),

                            Constant::String(st) => {
                                let data_id = module
                                    .declare_data(
                                        &format!("str,{}", st.distinct()),
                                        cranelift_module::Linkage::Local,
                                        false,
                                        false,
                                    )
                                    .unwrap();

                                let mut dcx = DataContext::new();
                                dcx.set_align(16); // TODO(mental): this was in the old implementation. I do not know why.

                                let string = self.host.spanref_to_str(*st).unwrap();
                                let string = std::ffi::CString::new(string).unwrap();
                                let string = string.into_bytes_with_nul();
                                let string = string.into_boxed_slice();

                                dcx.define(string);

                                module.define_data(data_id, &mut dcx).unwrap();

                                let str_gv = module.declare_data_in_func(data_id, self.inner.func);

                                let ptr = self.inner.ins().global_value(ir::types::I64, str_gv);

                                (ptr, TypingConstants::Str)
                            }

                            Constant::None => (
                                self.inner.ins().iconst(ir::types::I64, 0),
                                TypingConstants::None,
                            ),

                            Constant::Ellipsis => todo!(),
                        };

                        self.values.insert(*ret, TValue::imm(ssa, const_t));
                    }

                    CgInst::ReadLocalVar { var, ret } => {
                        let (ss, ty, _) = self.locals[&var.group()];
                        let v = self.inner.ins().stack_load(ir::types::I64, ss, 0);

                        self.values.insert(*ret, TValue::imm(v, ty));
                    }

                    CgInst::WriteLocalVar { var, orig } => {
                        if !self.locals.contains_key(&var.group()) {
                            let slot_t = TypingConstants::Int;
                            let ss = stack_alloc(&mut self.inner, self.host, slot_t);

                            self.locals.insert(
                                var.group(),
                                (ss, slot_t, ir_type_of(self.host.tcx(), slot_t)),
                            );
                        }

                        let (ss, slot_t, _) = self.locals[&var.group()];

                        let dest = self.inner.ins().stack_addr(ir::types::I64, ss, 0);

                        let val = self.values[orig];
                        let src = *self.values[orig].as_value();

                        if self.host.tcx().is_integer(val.type_id) {
                            self.inner.ins().stack_store(src, ss, 0);
                        } else {
                            let size = self.host.tcx().size_of(slot_t);
                            let size = self
                                .inner
                                .ins()
                                .iconst(ir::types::I64, i64::try_from(size).unwrap());

                            self.inner
                                .call_memcpy(cg_settings.frontend_config(), dest, src, size);
                        }
                    }

                    CgInst::Return(val) => {
                        let rval = *self.values[&val].as_value();

                        self.inner.ins().return_(&[rval]);
                        break;
                    }
                }
            }

            self.inner.seal_block(current_block);

            blocks_processed.push(block_ix);

            for edge in self.cfg.edges_directed(block_ix, EdgeDirection::Outgoing) {
                assert_eq!(edge.source(), block_ix);

                if let Ok(_) = blocks_processed.binary_search(&edge.target()) {
                    continue;
                }

                if let None = block_indecies.iter().find(|ix| **ix == edge.target()) {
                    block_indecies.push_back(edge.target())
                }
            }
        }

        self.inner.finalize();
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
