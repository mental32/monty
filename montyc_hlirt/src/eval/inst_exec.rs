use montyc_core::SpanRef;
use montyc_flatcode::raw_inst::{Dunder, RawInst};
use montyc_flatcode::FlatInst;
use montyc_parser::ast::Constant;

use crate::exception::PyResult;

use super::frame::FrameState;

pub type InstResult = PyResult<usize>;

pub trait InstExec {
    /// Top-level dispatcher for calling the appropriate inst handler given the instruction op.
    ///
    /// You usually don't ever need to overload this yourself unless you're doing something spooky.
    ///
    #[inline]
    fn exec_inst(&mut self, frame: &mut FrameState, inst: &FlatInst) -> InstResult {
        let FlatInst {
            op,
            value: _inst_result,
            ..
        } = &inst;

        tracing::trace!("{:?}", inst);

        match op {
            RawInst::SetAnnotation { .. } => {
                // self.set_annotation(name, annotation);
                Ok(frame.next_inst())
            }
            RawInst::Tuple(_) => todo!(),
            RawInst::Undefined => todo!(),

            RawInst::BuildClass { sequence, class } => self.build_class(frame, *sequence, *class),

            RawInst::UseVar { variable: v } => self.use_var(frame, v.clone()),
            RawInst::SetVar { variable: v, value } => self.set_var(frame, v.clone(), value.clone()),

            RawInst::If {
                test,
                truthy,
                falsey,
            } => self.if_(frame, *test, truthy.clone(), falsey.clone()),

            RawInst::Br { to } => self.branch(frame, *to),
            RawInst::Const(cst) => self.const_(frame, cst),
            RawInst::PhiJump { recv, value } => self.phi_jump(frame, *recv, *value),
            RawInst::PhiRecv => self.phi_recv(frame),
            RawInst::Nop => self.nop(frame),
            RawInst::JumpTarget => self.jump_target(frame),
            RawInst::Return { value } => self.return_(frame, *value),

            RawInst::SetDunder {
                object,
                dunder,
                value,
            } => self.set_dunder(frame, *object, *dunder, *value),

            RawInst::GetDunder {
                object: _,
                dunder: _,
            } => todo!(),
            RawInst::Defn {
                name,
                params,
                returns,
                sequence_id: seq_id,
            } => self.define_fn(
                frame,
                name.clone(),
                params.as_slice(),
                returns.clone(),
                *seq_id,
            ),

            RawInst::Class { name } => self.class(frame, *name),

            RawInst::Call {
                callable,
                arguments,
            } => self.call(frame, *callable, arguments.as_slice()),

            RawInst::GetAttribute { object, attr } => self.get_attribute(frame, *object, *attr),
            RawInst::SetAttribute {
                object,
                attr,
                value,
            } => self.set_attribute(frame, *object, *attr, *value),

            RawInst::Import { path, relative } => self.import(frame, path, *relative),
            RawInst::RefAsStr { r } => self.ref_as_str(frame, *r),
        }
    }

    fn nop(&mut self, frame: &mut FrameState) -> InstResult {
        Ok(frame.next_inst())
    }

    fn jump_target(&mut self, frame: &mut FrameState) -> InstResult {
        Ok(frame.next_inst())
    }

    fn phi_recv(&mut self, frame: &mut FrameState) -> InstResult {
        Ok(frame.next_inst())
    }

    fn branch(&mut self, _frame: &mut FrameState, to: usize) -> InstResult {
        Ok(to)
    }

    fn build_class(&mut self, frame: &mut FrameState, sequence: usize, class: usize) -> InstResult;

    fn ref_as_str(&mut self, _frame: &mut FrameState, r: SpanRef) -> InstResult;

    fn class(&mut self, frame: &mut FrameState, name: SpanRef) -> InstResult;

    fn call(&mut self, frame: &mut FrameState, callable: usize, arguments: &[usize]) -> InstResult;

    fn set_attribute(
        &mut self,
        frame: &mut FrameState,
        object: usize,
        attr: usize,
        value: usize,
    ) -> InstResult;

    fn get_attribute(&mut self, frame: &mut FrameState, object: usize, attr: usize) -> InstResult;

    fn phi_jump(&mut self, frame: &mut FrameState, recv: usize, value: usize) -> InstResult;

    fn if_(
        &mut self,
        frame: &mut FrameState,
        test: usize,
        truthy: Option<usize>,
        falsey: Option<usize>,
    ) -> InstResult;

    fn define_fn(
        &mut self,
        frame: &mut FrameState,
        name: SpanRef,
        params: &[(SpanRef, Option<usize>)],
        returns: Option<usize>,
        seq_id: usize,
    ) -> InstResult;

    fn set_var(&mut self, frame: &mut FrameState, var: SpanRef, value: usize) -> InstResult;

    fn use_var(&mut self, frame: &mut FrameState, var: SpanRef) -> InstResult;

    fn import(&mut self, frame: &mut FrameState, path: &[SpanRef], relative: usize) -> InstResult;

    fn return_(&mut self, frame: &mut FrameState, value: usize) -> InstResult;

    fn const_(&mut self, frame: &mut FrameState, cst: &Constant) -> InstResult;

    fn set_dunder(
        &mut self,
        frame: &mut FrameState,
        object: usize,
        dunder: Dunder,
        value: usize,
    ) -> InstResult;
}
