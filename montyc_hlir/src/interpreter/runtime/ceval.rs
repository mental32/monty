use std::{cell::RefCell, rc::Rc};

use montyc_core::{patma, SpanRef};
use montyc_parser::ast::UnaryOp;
use petgraph::graph::NodeIndex;

use crate::{
    flatcode::{
        raw_inst::{Dunder, RawInst},
        FlatCode,
    },
    interpreter::{
        object::{string::StrObj, PyObject, RawObject},
        HashKeyT, PyResult,
    },
    HostGlue, ObjAllocId,
};

use super::Runtime;

#[derive(Debug, Clone, Copy)]
pub(in crate::interpreter) enum StackFrame {
    Module(ObjAllocId),
    Function(ObjAllocId),
    Class(ObjAllocId),
}

#[derive(Debug)]
pub struct ConstEvalContext<'code, 'gcx, 'rt> {
    /// The Runtime this evalutation belongs to.
    pub(crate) runtime: &'rt mut Runtime,

    /// The module object that the frame was derived from.
    pub(crate) module: ObjAllocId,

    /// The host of the runtime, typically its GlobalContext in the `montyc` crate.
    pub(crate) host: &'gcx dyn HostGlue,

    /// The amount of remaning ticks for this evaluator, seeded by the runtimes default tick count.
    ticks: usize,

    /// The code being evaluated.
    pub(crate) code: &'code FlatCode,

    /// The produced values for instructions in sequences in the code.
    pub(in crate::interpreter) values: Vec<Vec<Option<ObjAllocId>>>,

    /// The call stack.
    pub(in crate::interpreter) call_stack: Vec<(usize, usize, StackFrame)>,
}

pub(in crate::interpreter) trait TryIntoObject {
    fn try_into_object(&self, ex: &mut ConstEvalContext) -> PyResult<ObjAllocId>;
}

impl TryIntoObject for ObjAllocId {
    fn try_into_object(&self, _: &mut ConstEvalContext) -> PyResult<ObjAllocId> {
        Ok(*self)
    }
}

impl TryIntoObject for &str {
    fn try_into_object(&self, ex: &mut ConstEvalContext) -> PyResult<ObjAllocId> {
        ex.string(self).map(|st| st.0)
    }
}

impl TryIntoObject for SpanRef {
    fn try_into_object(&self, ex: &mut ConstEvalContext) -> PyResult<ObjAllocId> {
        ex.host.spanref_to_str(*self).try_into_object(ex)
    }
}

impl<'code, 'gcx, 'rt> ConstEvalContext<'code, 'gcx, 'rt> {
    // /// Create a new `AstExecutor`
    // #[inline]
    // pub fn new_with_module(
    //     runtime: &'global mut super::Runtime,
    //     module: &'module ModuleObject,
    //     host: &'global dyn HostGlue,
    // ) -> Self {
    //     let graph = &module.body;

    //     let ast_object_id = runtime.objects.reserve();

    //     {
    //         let __class__ = runtime
    //             .builtins
    //             .get_attribute_direct(runtime, runtime.hash("_module_type_"), ast_object_id)
    //             .unwrap()
    //             .alloc_id();

    //         runtime
    //             .objects
    //             .try_set_value(
    //                 ast_object_id,
    //                 RawObject {
    //                     __class__,
    //                     __dict__: Default::default(),
    //                     alloc_id: ast_object_id,
    //                 },
    //             )
    //             .unwrap();

    //         runtime.modules.insert(module.mref, ast_object_id);
    //     }

    //     let namespace = runtime.scope_graph.insert(ast_object_id);
    //     let frame = StackFrame::Module(ast_object_id);

    //     if runtime.builtins_scope == NodeIndex::end() {
    //         runtime.builtins_scope = namespace;
    //     } else {
    //         runtime.scope_graph.nest(namespace, runtime.builtins_scope);
    //     }

    //     Self {
    //         runtime,
    //         graph,
    //         host,
    //         subgraphs: Default::default(),
    //         ast_object: ast_object_id,
    //         node_index: module.body.from_index(0),
    //         eval_stack: vec![(
    //             namespace,
    //             UniqueNodeIndex {
    //                 subgraph_index: None,
    //                 node_index: NodeIndex::end(),
    //             },
    //             frame,
    //         )],
    //     }
    // }

    #[inline]
    pub fn define(&mut self, name: SpanRef, value: ObjAllocId) -> PyResult<()> {
        let (_, _, frame) = self.call_stack.last().as_ref().unwrap();
        let mut frame =
            patma!(*o, StackFrame::Class(o) | StackFrame::Function(o) | StackFrame::Module(o) in frame).unwrap();

        let st_name = self.host.spanref_to_str(name);
        let (name, hash) = self.string(st_name)?;

        log::trace!(
            "[ConstEvalContext::define] setattr {:?} . {:?} = {:?}",
            frame,
            st_name,
            value
        );

        frame.set_attribute_direct(self.runtime, hash, name, value);

        Ok(())
    }

    #[inline]
    pub fn lookup(&mut self, name: SpanRef) -> Option<ObjAllocId> {
        let (_, _, frame) = self
            .call_stack
            .last()
            .cloned()
            .expect("Must be running to perform lookups.");

        let frame = patma!(o, StackFrame::Class(o) | StackFrame::Function(o) | StackFrame::Module(o) in frame).unwrap();

        let name = self.host.spanref_to_str(name);

        let object = self
            .runtime
            .scope_graph
            .search(frame, |scope| scope.getattr_static(self.runtime, name))?;

        Some(object)
    }

    /// A helper method to set the stringy property `key` to `value` on some `object`.
    #[inline]
    fn set_attribute(
        &mut self,
        object: &mut impl PyObject,
        key: &str,
        value: impl TryIntoObject,
    ) -> PyResult<()> {
        let value = value.try_into_object(self)?;
        let (key, hash) = self.string(key)?;

        object.set_attribute_direct(self.runtime, hash, key, value);

        Ok(())
    }

    #[inline]
    fn insert_new_object<F, O>(&mut self, __class__: ObjAllocId, f: F) -> PyResult<ObjAllocId>
    where
        F: FnOnce(&mut Self, RawObject, ObjAllocId) -> PyResult<O>,
        O: PyObject,
    {
        let object_alloc_id = self.runtime.objects.reserve();

        let raw_object = RawObject {
            alloc_id: object_alloc_id,
            __dict__: Default::default(),
            __class__,
        };

        let object = f(self, raw_object, object_alloc_id)?;
        let object = Box::new(object) as Box<dyn PyObject>;

        let _ = self
            .runtime
            .objects
            .try_set_value(object_alloc_id, Rc::new(RefCell::new(object.into())));

        Ok(object_alloc_id)
    }

    /// Create a new string object.
    #[inline]
    pub fn string(&mut self, initial: impl AsRef<str>) -> PyResult<(ObjAllocId, HashKeyT)> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "str")
            .unwrap();

        let initial = initial.as_ref().to_owned();
        let st_hash = self.runtime.hash(initial.clone());

        if let Some(cached) = self.runtime.strings.get(&st_hash) {
            return Ok((*cached, st_hash));
        }

        let obj = self.insert_new_object(__class__, move |this, object, object_alloc_id| {
            this.runtime.strings.insert(st_hash, object_alloc_id);

            let string = StrObj {
                header: object,
                value: initial,
                value_hashed: st_hash,
            };

            Ok(string)
        })?;

        Ok((obj, st_hash))
    }
}

impl<'code, 'gcx, 'rt> ConstEvalContext<'code, 'gcx, 'rt> {
    pub fn new(
        runtime: &'rt mut Runtime,
        host: &'gcx dyn HostGlue,
        code: &'code FlatCode,
        module: ObjAllocId,
    ) -> Self {
        let values = code
            .sequences
            .iter()
            .map(|seq| Vec::with_capacity(seq.len()))
            .collect();

        Self {
            ticks: runtime.op_ticks,
            runtime,
            host,
            code,
            values,
            module,
            call_stack: vec![],
        }
    }

    #[inline]
    fn get_value(&self, seq_ix: usize, inst_ix: usize) -> Option<ObjAllocId> {
        self.values[seq_ix][inst_ix]
    }

    #[inline]
    fn set_value(
        &mut self,
        seq_ix: usize,
        inst_ix: usize,
        value: ObjAllocId,
    ) -> Option<ObjAllocId> {
        self.values[seq_ix][inst_ix].replace(value)
    }

    pub fn eval(mut self) -> PyResult<NodeIndex> {
        assert!(self.call_stack.is_empty());

        self.call_stack
            .push((0, 0, StackFrame::Module(self.module)));

        let mut call_stack = std::mem::take(&mut self.call_stack);

        loop {
            let (seq_ix, inst_ix, frame) = match call_stack.last_mut() {
                Some((a, b, c)) => (a, b, c),
                None => break,
            };

            macro_rules! value {
                () => {{
                    self.get_value(*seq_ix, *inst_ix).unwrap()
                }};

                ($ix:expr) => {{
                    self.get_value(*seq_ix, $ix).unwrap()
                }};

                ($ix:expr, $val:expr) => {{
                    self.set_value(*seq_ix, $ix, $val)
                }};
            }

            let seq = match self.code.sequences.get(*seq_ix) {
                Some(seq) => seq.as_slice(),
                None => break,
            };

            let inst = &seq[*inst_ix];

            match &inst.op {
                RawInst::Return { value } => {
                    if matches!(frame, StackFrame::Function(_)) {
                        todo!()
                    } else {
                        unreachable!()
                    }

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::Br { to } => *inst_ix = *to,

                RawInst::SetVar { variable, value } => {
                    let value = value!();

                    self.define(variable.clone(), value)?;

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::UseVar { variable } => {
                    let object = match self.lookup(variable.clone()) {
                        Some(o) => o,
                        None => todo!("NameError: {:?}", variable),
                    };

                    let _ = value!(*inst_ix, object);

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::Defn {
                    name,
                    params,
                    returns,
                    sequence_id,
                } => todo!(),

                RawInst::Class { name } => todo!(),

                RawInst::Call {
                    callable,
                    arguments,
                } => {
                    let callable = self
                        .get_value(*seq_ix, *inst_ix)
                        .expect("Attempted to call an undefined value.");

                    let arguments: Vec<_> =
                        arguments.iter().cloned().map(|arg| value!(arg)).collect();

                    let result = callable.call(&mut self, arguments.as_slice())?;

                    let _ = value!(*inst_ix, result);

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::If {
                    test,
                    truthy,
                    falsey,
                } => {
                    let test = value!(*test);
                    let is_true = test.is_truthy()?;

                    *inst_ix = match (is_true, truthy, falsey) {
                        (true, None, _) | (false, _, None) => inst_ix.saturating_add(1),
                        (true, Some(true_ix), _) => true_ix.saturating_sub(1),
                        (false, _, Some(false_ix)) => false_ix.saturating_sub(1),
                        (_, None, None) => unreachable!(),
                    };
                }
                RawInst::GetAttribute { object, name } => {
                    let object = value!(*object);
                    let name = self.host.spanref_to_str(name.clone());

                    let attr = match object.getattr_static(&self.runtime, name) {
                        Some(attr) => attr,
                        None => todo!(),
                    };

                    let _ = value!(*inst_ix, attr);

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::SetAttribute {
                    object,
                    name,
                    value,
                } => {
                    let mut object = value!(*object);
                    let key = self.host.spanref_to_str(name.clone());
                    let value = self.get_value(*seq_ix, *inst_ix).unwrap();

                    object.setattr_static(&mut self.runtime, key, value);

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::GetDunder { object, dunder } => {
                    let object = value!(*object);
                    let key = match dunder {
                        Dunder::Unary(op) => op.as_ref(),
                        Dunder::Infix(op) => op.as_ref(),
                        Dunder::DocComment => "__doc__",
                    };

                    let attr = match object.getattr_static(&self.runtime, key) {
                        Some(attr) => attr,
                        None => todo!(),
                    };

                    let _ = value!(*inst_ix, attr);

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::SetDunder {
                    object,
                    dunder,
                    value,
                } => {
                    let mut object = value!(*object);
                    let val = self.get_value(*seq_ix, *inst_ix).unwrap();
                    let key = match dunder {
                        Dunder::Unary(op) => op.as_ref(),
                        Dunder::Infix(op) => op.as_ref(),
                        Dunder::DocComment => "__doc__",
                    };

                    object.setattr_static(&mut self.runtime, key, val);
                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::GetItem { object, index } => {
                    let object = value!(*object);
                    let index = value!(*index);

                    let getitem = object.getattr_static(self.runtime, "__getitem__").unwrap();
                    let result = getitem.call(&mut self, &[object, index])?;

                    let _ = value!(*inst_ix, result);

                    *inst_ix = inst_ix.saturating_add(1);
                }

                RawInst::SetItem {
                    object,
                    index,
                    value,
                } => {}

                RawInst::Import(_) => todo!(),
                RawInst::Const(_) => todo!(),
                RawInst::Tuple(_) => todo!(),
                RawInst::Nop => todo!(),
                RawInst::Undefined => todo!(),

                RawInst::PhiJump { recv, value } => todo!(),
                RawInst::PhiRecv => todo!(),
            }
        }

        todo!();
    }
}
