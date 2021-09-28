use cranelift_codegen::ir::{self, condcodes::IntCC, AbiParam, Function, InstBuilder, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_object::ObjectModule;

use montyc_core::{TypeId, TypingConstants};

use crate::backend::BackendImpl;

pub(crate) fn int_add(
    _cx: &BackendImpl,
    _obj: &mut ObjectModule,
) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params
        .extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::I64));

    let mut func = Function::with_name_signature(
        ir::ExternalName::User {
            namespace: 0,
            index: 0,
        },
        sig,
    );
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!(),
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().iadd(a, b);

    fx.ins().return_(&[ret]);

    (func, TypingConstants::Int, "__add__")
}

pub(crate) fn int_sub(
    _cx: &BackendImpl,
    _obj: &mut ObjectModule,
) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params
        .extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::I64));

    let mut func = Function::with_name_signature(
        ir::ExternalName::User {
            namespace: 0,
            index: 0,
        },
        sig,
    );
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!(),
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().isub(a, b);

    fx.ins().return_(&[ret]);

    (func, TypingConstants::Int, "__sub__")
}

pub(crate) fn int_eq(
    _cx: &BackendImpl,
    _obj: &mut ObjectModule,
) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params
        .extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::B1));

    let mut func = Function::with_name_signature(
        ir::ExternalName::User {
            namespace: 0,
            index: 0,
        },
        sig,
    );
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!(),
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().icmp(IntCC::Equal, a, b);

    fx.ins().return_(&[ret]);

    (func, TypingConstants::Int, "__eq__")
}

pub(crate) fn int_ne(
    _cx: &BackendImpl,
    _obj: &mut ObjectModule,
) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params
        .extend_from_slice(&[AbiParam::new(ir::types::R64), AbiParam::new(ir::types::I64)]);
    sig.returns.push(AbiParam::new(ir::types::B1));

    let mut func = Function::with_name_signature(
        ir::ExternalName::User {
            namespace: 0,
            index: 0,
        },
        sig,
    );
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let (a, b) = match fx.block_params(start) {
        [a, b] => (*a, *b),
        _ => unreachable!(),
    };

    let a = fx.ins().raw_bitcast(ir::types::I64, a);

    let ret = fx.ins().icmp(IntCC::NotEqual, a, b);

    fx.ins().return_(&[ret]);

    (func, TypingConstants::Int, "__ne__")
}

pub(crate) fn int_bool(
    _cx: &BackendImpl,
    _obj: &mut ObjectModule,
) -> (Function, TypeId, &'static str) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.params
        .extend_from_slice(&[AbiParam::new(ir::types::R64)]);
    sig.returns.push(AbiParam::new(ir::types::B1));

    let mut func = Function::with_name_signature(
        ir::ExternalName::User {
            namespace: 0,
            index: 0,
        },
        sig,
    );
    let mut f_cx = FunctionBuilderContext::new();
    let mut fx = FunctionBuilder::new(&mut func, &mut f_cx);

    let start = fx.create_block();

    fx.append_block_params_for_function_params(start);
    fx.seal_block(start);
    fx.switch_to_block(start);

    let this = match fx.block_params(start) {
        [this] => *this,
        _ => unreachable!(),
    };

    let this = fx.ins().raw_bitcast(ir::types::I64, this);
    let zero = fx.ins().iconst(ir::types::I64, 0);

    let ret = fx.ins().icmp(IntCC::NotEqual, this, zero);

    fx.ins().return_(&[ret]);

    (func, TypingConstants::Int, "__bool__")
}
