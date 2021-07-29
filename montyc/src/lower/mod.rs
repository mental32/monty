//! Organized here are all the lower'ing phases of the compiler.
//!
//! Most notably the lower pass for AST FuncDefs to HLIR Code and
//! HLIR Code to Cranelift IR.
//!

use montyc_core::MontyResult;

/// A generic lowering trait.
///
/// Keep readers will observer that this trait is identical to
/// `crate::typechk::Typecheck` except that it's meant for lowering
/// passes instead of verification passes.
///
pub(in crate) trait Lower<Context, Output = ()> {
    fn lower(&self, cx: Context) -> MontyResult<Output>;
}

pub(crate) mod fndef_to_hlir {
    //! Convert a FuncDef to an HLIR Code.

    use std::rc::Rc;

    use ahash::AHashSet;

    use montyc_core::{patma, MontyResult, TypeId, ValueId};
    use montyc_hlir::{
        code::{BlockId, Inst, SSAValueData, SSAValueDef, SSAValueId},
        interpreter::UniqueNodeIndex,
        typing::{PythonType, TypingContext},
        ObjAllocId, ObjectGraph, ObjectGraphIndex, Value,
    };
    use montyc_parser::{
        ast::{Assign, Atom, Expr, Primary, Return, While},
        AstObject, AstVisitor,
    };

    use crate::{def_stack::DefStack, prelude::GlobalContext};

    use super::Lower;

    type HLIRFunc = montyc_hlir::code::Function;
    type ASTFunc = montyc_parser::ast::FunctionDef;

    #[derive(Debug)]
    pub(crate) struct FunctionContext {
        pub(crate) object_graph: Rc<ObjectGraph>,
        pub(crate) value_index: ObjectGraphIndex,
        pub(crate) value_alloc_id: ObjAllocId,
        pub(crate) ast_index: UniqueNodeIndex,
        pub(crate) type_id: TypeId,
        pub(crate) def_stack: DefStack,
    }

    type I<'a> = (FunctionContext, &'a mut GlobalContext);

    impl<'a> Lower<I<'a>, Vec<HLIRFunc>> for ASTFunc {
        fn lower(&self, cx: I) -> MontyResult<Vec<HLIRFunc>> {
            let top_level: HLIRFunc = self.lower(cx)?;

            todo!("{:#?}", top_level);
        }
    }

    #[derive(Debug)]
    struct FunctionBuilder<'cx> {
        context: &'cx mut FunctionContext,
        gcx: &'cx GlobalContext,
        function: HLIRFunc,
        local_variables: AHashSet<u32>,
        current_block: Option<BlockId>,
    }

    impl<'cx> FunctionBuilder<'cx> {
        fn get_type_of(&mut self, value: SSAValueId) -> (TypeId, PythonType) {
            let type_id = self.function.ssa_values.get(value).unwrap().type_id.clone();
            let tcx = self.gcx.typing_context.borrow();
            let localized_type_id = tcx.contextualize(type_id).unwrap();

            (type_id, localized_type_id.as_python_type().clone())
        }

        fn fallthrough_to_new_block(&mut self) -> (BlockId, BlockId) {
            let new = self.function.layout.add();
            let current = self.current_block.replace(new).unwrap();

            self.function.layout.fallthrough(current, new);

            (current, new)
        }

        fn append_instruction(&mut self, inst: Inst) -> SSAValueDef {
            let block = self
                .current_block
                .expect("Must be focused on a block before inserting instructions.");

            let mut_block = self
                .function
                .layout
                .get_mut(block)
                .expect("out of bounds block index for function.");

            let inst = mut_block.push_inst(inst);

            SSAValueDef::Inst(block, inst)
        }

        fn int_const(&mut self, value: i64) -> SSAValueId {
            let value_def = self.append_instruction(Inst::IntConst(value));

            self.function.ssa_values.insert(SSAValueData {
                type_id: TypingContext::Int,
                value_def,
            })
        }

        fn alias(&mut self, origin: SSAValueId) -> SSAValueId {
            let type_id = self
                .function
                .ssa_values
                .get(origin)
                .unwrap()
                .type_id
                .clone();

            let alias = self.function.ssa_values.reserve();
            let value_def = self.append_instruction(Inst::Alias(alias, origin));

            self.function
                .ssa_values
                .try_set_value(alias, SSAValueData { type_id, value_def })
                .unwrap();

            alias
        }

        fn use_var(&mut self, var: u32, type_id: TypeId) -> SSAValueId {
            let value_def = self.append_instruction(Inst::UseVar(var));
            self.function
                .ssa_values
                .insert(SSAValueData { type_id, value_def })
        }

        fn call_value(
            &mut self,
            (value, value_type): (ValueId, TypeId),
            args: impl IntoIterator<Item = SSAValueId>,
        ) -> SSAValueId {
            self.function.refs.insert(value);

            let value_def =
                self.append_instruction(Inst::CallValue(value, args.into_iter().collect()));

            self.function.ssa_values.insert(SSAValueData {
                type_id: value_type,
                value_def,
            })
        }

        fn jump_if_truthy(&mut self, test: SSAValueId, target: BlockId) {
            let (from, inst) = self.append_instruction(Inst::JumpIf(test, target)).inst();
            self.function.layout.jump(from, target, inst);
        }
    }

    type HResult = MontyResult<Option<SSAValueId>>;

    impl<'cx> AstVisitor<HResult> for FunctionBuilder<'cx> {
        fn visit_any(&mut self, object: &dyn AstObject) -> HResult {
            unimplemented!("{:#?}", object.into_ast_node());
        }

        fn visit_assign(&mut self, asn: &Assign) -> HResult {
            let Assign { name, value, .. } = asn;
            log::trace!("[FunctionBuilder::visit_assign] {:?} := {:?}", name, value);

            let value = value.visit_with(self)?.unwrap();

            match &name.inner {
                Primary::Atomic(name) => {
                    let name_ref =
                        patma!(name_ref, Some(name_ref) in name.inner.as_name()).unwrap();

                    let _ = self.local_variables.insert(name_ref.group());

                    Ok(Some(self.alias(value)))
                }

                Primary::Subscript { value, index: _ } => {
                    let _lhs = value.visit_with(self)?;

                    todo!();
                }

                Primary::Attribute { left, attr: _ } => {
                    let _lhs = left.visit_with(self)?;

                    todo!();
                }

                Primary::Call { .. } | Primary::Await(_) => unreachable!(),
            }
        }

        fn visit_while(&mut self, while_: &While) -> HResult {
            let While { test, body } = while_;
            log::trace!("[FunctionBuilder::visit_assign] {:?}", while_);

            let (_, loop_test_block) = self.fallthrough_to_new_block();

            // emit the test code.
            let test = test.visit_with(self)?.unwrap();

            // if the test was truthy, then jump to the loop body.
            let body_block = self.function.layout.add();
            self.jump_if_truthy(test, body_block);

            // otherwise, fall through to a new block.
            let (_, post_loop_block) = self.fallthrough_to_new_block();

            // emit the loop body.
            {
                self.current_block.replace(body_block);

                for node in body.iter() {
                    node.visit_with(self)?;
                }

                self.function
                    .layout
                    .fallthrough(self.current_block.unwrap(), loop_test_block);
            }

            self.current_block.replace(post_loop_block);

            Ok(None)
        }

        fn visit_binop(&mut self, expr: &Expr) -> HResult {
            fn try_get_class_dunder<'a>(
                cx: &'a FunctionBuilder,
                dunder: String,
                value: SSAValueId,
            ) -> (ValueId, Option<(ValueId, TypeId)>) {
                let rhs_type = cx.function.ssa_values.get(value).unwrap().type_id;
                let store = cx.gcx.value_store.borrow();
                let (class_value_id, class_value, graph) = store.class_of(rhs_type).unwrap();

                if let Value::Class { properties, .. } = class_value {
                    let rt_ref = cx.gcx.const_runtime.borrow();

                    let dunder = properties.get(rt_ref.hash(dunder.clone())).and_then(|(_, attr)| {
                        log::trace!(
                            "[TypeEvalContext::typecheck] Checking BinOp {:?} compatability on method {:?}",
                            dunder,
                            attr
                        );

                        let alloc_id = graph.alloc_id_of(attr).unwrap();
                        let value_id = cx.gcx.value_store.borrow().get_value_from_alloc(alloc_id).unwrap();
                        let type_id = cx.gcx.value_store.borrow().type_of(value_id).unwrap();

                        Some((value_id, type_id))
                    });

                    (class_value_id, dunder)
                } else {
                    unreachable!("The `class_of` return value for the type_id of the this value {:?} was not a class value!", value);
                }
            }

            let (lhs, op, rhs) =
                patma!((left, op, right), Expr::BinOp { left, op, right } in expr).unwrap();

            // let store = self.gcx.value_store.borrow();

            let lhs = lhs.visit_with(self)?.unwrap();
            let (lhs_class, lhs_dunder_method) =
                try_get_class_dunder(self, format!("__{}__", op.as_ref()), lhs);

            let rhs = rhs.visit_with(self)?.unwrap();
            let (rhs_class, rhs_dunder_method) =
                try_get_class_dunder(self, format!("__r{}__", op.as_ref()), rhs);

            let ((dunder_method, _), args) = lhs_dunder_method
                .zip(Some(lhs_class))
                .zip(Some([lhs, rhs]))
                .or(rhs_dunder_method.zip(Some(rhs_class)).zip(Some([rhs, lhs])))
                .unwrap();

            let result = self.call_value(dunder_method, args);

            Ok(Some(result))
        }

        fn visit_name(&mut self, node: &Atom) -> HResult {
            let name = patma!(name, Atom::Name(name) in node).unwrap();

            Ok(Some(self.use_var(
                name.group(),
                self.context.def_stack.get(name.group()).unwrap().0,
            )))
        }

        fn visit_int(&mut self, int: &Atom) -> HResult {
            let n = patma!(*i, Atom::Int(i) in int).unwrap();
            log::trace!("[FunctionBuilder::visit_int] Atom::Int({:?})", n);

            Ok(Some(self.int_const(n)))
        }

        fn visit_ifstmt(&mut self, ifch: &montyc_parser::ast::IfChain) -> HResult {
            let (_, test_block) = self.fallthrough_to_new_block();

            let body_block_count = ifch.branches.iter().count() + ifch.orelse.is_some() as usize;

            let body_blocks: Vec<_> = (0..body_block_count)
                .map(|_| self.function.layout.add())
                .collect();

            let after_if = self.function.layout.add();

            for (if_case, block_id) in ifch.branches.iter().zip(body_blocks.iter()) {
                let test = if_case.inner.test.visit_with(self)?.unwrap();
                self.jump_if_truthy(test, *block_id);

                self.current_block.replace(*block_id);

                for body in if_case.inner.body.iter() {
                    body.visit_with(self)?;
                }

                self.append_instruction(Inst::Jump(after_if));

                self.current_block.replace(test_block);
            }

            assert_eq!(self.current_block, Some(test_block));

            if let Some(orelse_body) = &ifch.orelse {
                let (before_orelse, _) = self.fallthrough_to_new_block();

                debug_assert_eq!(before_orelse, test_block);

                for body in orelse_body.iter() {
                    body.visit_with(self)?;
                }
            }

            self.append_instruction(Inst::Jump(after_if));
            self.current_block.replace(after_if);

            Ok(None)
        }

        fn visit_call(&mut self, call: &Primary) -> HResult {
            let (func, args) = patma!((func, args), Primary::Call { func, args } in call).unwrap();

            let callable = func.visit_with(self)?.unwrap();
            let args = match args {
                Some(args) => {
                    let mut arguments = Vec::with_capacity(args.len());
                    for arg in args.iter() {
                        let arg_value = arg.visit_with(self)?.unwrap();
                        arguments.push(arg_value);
                    }
                    arguments
                }

                None => vec![],
            };

            let return_type_id = match self.get_type_of(callable) {
                (_, PythonType::Callable { ret, .. }) => ret,
                _ => unreachable!(),
            };

            let value_def = self.append_instruction(Inst::CallValueLocal(callable, args));
            let return_value = self.function.ssa_values.insert(SSAValueData {
                type_id: return_type_id,
                value_def,
            });

            Ok(Some(return_value))
        }

        fn visit_return(&mut self, ret: &Return) -> HResult {
            let value = ret.value.as_ref().unwrap().visit_with(self)?.unwrap();

            self.function.layout.ret(self.current_block.unwrap(), value);
            Ok(None)
        }

        fn visit_ternary(&mut self, ternary: &Expr) -> HResult {
            let (test, body, orelse) =
                patma!((test, body, orelse), Expr::If { test, body, orelse } in ternary).unwrap();

            let (_, test_block) = self.fallthrough_to_new_block();

            let test_value = test.inner.visit_with(self)?.unwrap();
            let true_block = self.function.layout.add();

            self.jump_if_truthy(test_value, true_block);
            self.current_block.replace(true_block);

            let _true_value = body.inner.visit_with(self)?.unwrap();

            self.current_block.replace(test_block);
            let _ = self.fallthrough_to_new_block();

            let false_value = orelse.inner.visit_with(self)?.unwrap();

            let (_, phi) = self.fallthrough_to_new_block();

            // the typechecker should guarantee that the true and false sides of this ternary are either the same type or it normalizes them by creating a union of their types.
            let (param_t, _) = self.get_type_of(false_value);

            let phi_block = self.function.layout.get_mut(phi).unwrap();

            let param = self.function.ssa_values.insert(SSAValueData {
                type_id: param_t,
                value_def: SSAValueDef::Param(phi, phi_block.parameters.len()),
            });

            phi_block.parameters.push((param, param_t));

            Ok(Some(param))
        }

        fn visit_subscript(&mut self, subscr: &Primary) -> HResult {
            let (_value, _index) =
                patma!((value, index), Primary::Subscript { value, index } in subscr).unwrap();

            todo!("{:#?}", subscr);
        }
    }

    impl<'a> Lower<I<'a>, HLIRFunc> for ASTFunc {
        fn lower(&self, (mut cx, gcx): I<'a>) -> MontyResult<HLIRFunc> {
            let mut fn_builder = FunctionBuilder {
                context: &mut cx,
                gcx,
                function: Default::default(),
                local_variables: Default::default(),
                current_block: None,
            };

            fn_builder
                .current_block
                .replace(fn_builder.function.layout.add());

            for node in self.body.iter() {
                node.visit_with(&mut fn_builder)?;
            }

            Ok(fn_builder.function)
        }
    }
}
