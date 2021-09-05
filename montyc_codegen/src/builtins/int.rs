use cranelift_codegen::{ir::{self, AbiParam, Function, InstBuilder, Signature, condcodes::IntCC}, isa::CallConv};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;
use montyc_core::TypeId;
use montyc_hlir::typing::TypingContext;

use crate::module::CodegenModule;


pub fn int_add(cx: &mut CodegenModule, obj: &mut ObjectModule) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params.extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::I64));

    let mut func = Function::with_name_signature(ir::ExternalName::User { namespace: 0, index: 0 }, sig);
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!()
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().iadd(a, b);

    fx.ins().return_(&[ret]);

    (func, TypingContext::Int, "__add__")
}

pub fn int_sub(cx: &mut CodegenModule, obj: &mut ObjectModule) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params.extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::I64));

    let mut func = Function::with_name_signature(ir::ExternalName::User { namespace: 0, index: 0 }, sig);
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!()
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().isub(a, b);

    fx.ins().return_(&[ret]);

    (func, TypingContext::Int, "__sub__")
}

pub fn int_eq(cx: &mut CodegenModule, obj: &mut ObjectModule) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params.extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::B1));

    let mut func = Function::with_name_signature(ir::ExternalName::User { namespace: 0, index: 0 }, sig);
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!()
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().icmp(IntCC::Equal, a, b);

    fx.ins().return_(&[ret]);

    (func, TypingContext::Int, "__eq__")
}

pub fn int_ne(cx: &mut CodegenModule, obj: &mut ObjectModule) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params.extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::B1));

    let mut func = Function::with_name_signature(ir::ExternalName::User { namespace: 0, index: 0 }, sig);
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!()
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().icmp(IntCC::NotEqual, a, b);

    fx.ins().return_(&[ret]);

    (func, TypingContext::Int, "__ne__")
}

pub fn int_bool(cx: &mut CodegenModule, obj: &mut ObjectModule) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params.extend_from_slice(&[AbiParam::new(ir::types::R64)]);
    sig.returns.push(AbiParam::new(ir::types::B1));

    let mut func = Function::with_name_signature(ir::ExternalName::User { namespace: 0, index: 0 }, sig);
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let this = match fx.block_params(start) {
        [this] => *this,
        _ => unreachable!()
    };

    let this = fx.ins().raw_bitcast(ir::types::I64,this);
    let zero = fx.ins().iconst(ir::types::I64, 0);

    let ret = fx.ins().icmp(IntCC::NotEqual, this, zero);

    fx.ins().return_(&[ret]);

    (func, TypingContext::Int, "__bool__")
}
