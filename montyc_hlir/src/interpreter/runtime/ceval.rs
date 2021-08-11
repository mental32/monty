use std::{cell::RefCell, rc::Rc};

use ahash::AHashMap;
use montyc_core::{patma, SpanRef};
use montyc_parser::ast::UnaryOp;
use petgraph::graph::NodeIndex;

use crate::{
    flatcode::{
        raw_inst::{Const, Dunder, RawInst},
        FlatCode, FlatInst,
    },
    interpreter::{
        object::{frame::FrameObject, string::StrObj, PyObject, RawObject},
        HashKeyT, PyResult,
    },
    HostGlue, ModuleData, ModuleObject, ObjAllocId,
};

use super::Runtime;

#[derive(Debug)]
struct FrameValues(Box<[Option<ObjAllocId>]>);

impl FrameValues {
    #[inline]
    fn get(&mut self, index: usize) -> Option<ObjAllocId> {
        self.0[index]
    }

    #[inline]
    fn set(&mut self, index: usize, value: ObjAllocId) -> Option<ObjAllocId> {
        self.0[index].replace(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub(in crate::interpreter) enum StackFrame {
    Module(ObjAllocId),
    Function(ObjAllocId),
    Class(ObjAllocId),
}

impl From<StackFrame> for ObjAllocId {
    fn from(frame: StackFrame) -> Self {
        match frame {
            StackFrame::Module(o) | StackFrame::Function(o) | StackFrame::Class(o) => o,
        }
    }
}

#[derive(Debug)]
pub struct ConstEvalContext<'code, 'gcx, 'rt> {
    /// The Runtime this evalutation belongs to.
    pub(crate) runtime: &'rt mut Runtime,

    /// The module object that the frame was derived from.
    pub(crate) module: ObjAllocId,

    /// The host of the runtime, typically its GlobalContext in the `montyc` crate.
    pub(crate) host: &'gcx mut dyn HostGlue,

    /// The amount of remaning ticks for this evaluator, seeded by the runtimes default tick count.
    ticks: usize,

    /// The code being evaluated.
    pub(crate) code: &'code FlatCode,

    module_object: &'gcx ModuleData,

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
        ex.host
            .spanref_to_str(*self)
            .to_string()
            .as_str()
            .try_into_object(ex)
    }
}

impl<'code, 'gcx, 'rt> ConstEvalContext<'code, 'gcx, 'rt> {
    #[inline]
    pub fn define(&mut self, name: SpanRef, value: ObjAllocId) -> PyResult<()> {
        let (_, _, frame) = self.call_stack.last().as_ref().unwrap();
        let mut frame =
            patma!(*o, StackFrame::Class(o) | StackFrame::Function(o) | StackFrame::Module(o) in frame).unwrap();

        let st_name = self.host.spanref_to_str(name).to_string();
        let (name, hash) = self.string(&st_name)?;

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
}

impl<'code, 'gcx, 'rt> ConstEvalContext<'code, 'gcx, 'rt> {
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
            .internals
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
        host: &'gcx mut dyn HostGlue,
        code: &'code FlatCode,
        module: ObjAllocId,
        module_object: &'gcx ModuleData,
    ) -> Self {
        Self {
            module_object,
            ticks: runtime.op_ticks,
            runtime,
            host,
            code,
            module,
            call_stack: vec![],
        }
    }

    /// Evaluate a single frame.
    ///
    /// This is separate from `Self::eval` because it let's me use the try operator
    /// and not fuck up any exception handling.
    ///
    #[inline]
    fn eval_frame(
        &mut self,
        (seq_ix, mut inst_ix, frame): (usize, usize, StackFrame),
    ) -> PyResult<(ObjAllocId, FrameValues)> {
        assert_eq!(inst_ix, 0, "Refusing to start evaluating a frame mid-way.");

        let seq = match self.code.sequences.get(seq_ix) {
            Some(seq) => seq.as_slice(),
            None => unreachable!("out-of-bounds sequence index {:?}", seq_ix),
        };

        log::trace!("[ConstEvalContext::eval] Sequence {}", self.code);

        let mut values = FrameValues(vec![None; seq.len()].into_boxed_slice());
        let frame_object_alloc_id = self.runtime.objects.reserve();
        let mut frame_object = FrameObject {
            inner: RawObject {
                alloc_id: frame_object_alloc_id,
                __class__: frame_object_alloc_id,
                __dict__: Default::default(),
            },

            locals: AHashMap::<u32, (HashKeyT, ObjAllocId, ObjAllocId)>::new(),
        };

        let final_value = 'outer: loop {
            if self.ticks != 0 {
                self.ticks -= 1;
            } else {
                panic!("Exceeded tick budget for const runtime");
            }

            if inst_ix >= seq.len() {
                break 'outer if let StackFrame::Module(mut module) = frame {
                    debug_assert_eq!(module, self.module);

                    for (group, (hash, key, value)) in frame_object.locals.drain() {
                        module.set_attribute_direct(&mut self.runtime, hash, key, value);
                    }

                    module
                } else {
                    self.runtime
                        .internals
                        .getattr_static(&self.runtime, "none")
                        .unwrap()
                };
            }

            let inst = &seq[inst_ix];

            log::trace!(
                "[ConstEvalContext::eval] [seq={:?}, inst={:?}] {:?}",
                seq_ix,
                inst_ix,
                inst
            );

            match &inst.op {
                RawInst::Nop => inst_ix += 1,

                RawInst::Br { to: dest } => inst_ix = *dest,

                /*  Just a jump target; handoff logic is handled below */
                RawInst::PhiRecv => inst_ix += 1,
                RawInst::PhiJump { recv, value } => {
                    let value = values.get(*value).unwrap();
                    let _ = values.set(*recv, value);

                    inst_ix = *recv;
                }

                RawInst::Const(cst) => {
                    let value = match cst {
                        Const::Int(_) => todo!(),
                        Const::Float(_) => todo!(),
                        Const::Bool(_) => todo!(),

                        Const::String(sref) => {
                            self.string(
                                self.host.spanref_to_str(sref.clone()).to_string().as_str(),
                            )?
                            .0
                        }

                        Const::None => todo!(),
                        Const::Ellipsis => todo!(),
                    };

                    let _ = values.set(inst_ix, value);

                    inst_ix += 1;
                }

                RawInst::SetVar { variable, value } => {
                    let (st, hash) = self.string(self.host.spanref_to_str(variable.clone()).to_string().as_str())?;

                    let object = values.get(*value).unwrap();

                    frame_object
                        .locals
                        .insert(variable.group(), (hash, st, object));

                    inst_ix += 1;
                }

                RawInst::UseVar { variable } => {
                    log::trace!("{:?}", self.host.spanref_to_str(*variable));

                    let object = match frame_object
                        .locals
                        .get(&variable.group())
                        .cloned()
                        .map(|(_, _, a)| a)
                        .or_else(|| self.lookup(variable.clone()))
                    {
                        Some(o) => o,
                        None => todo!("NameError"),
                    };

                    values.set(inst_ix, object);

                    inst_ix += 1;
                }

                RawInst::GetAttribute { object, name } => {
                    let object = values.get(*object).unwrap();
                    let name = self.host.spanref_to_str(name.clone());

                    let attr = match object.getattr_static(&self.runtime, name) {
                        Some(attr) => attr,
                        None => todo!(
                            "AttributeError: {:?} does not have attribute {:?}",
                            object,
                            name
                        ),
                    };

                    let _ = values.set(inst_ix, attr);

                    inst_ix += 1;
                }

                RawInst::SetAttribute {
                    object,
                    name,
                    value,
                } => {
                    let mut object = values.get(*object).unwrap();
                    let value = values.get(*value).unwrap();

                    let key = self.host.spanref_to_str(name.clone());

                    object.setattr_static(&mut self.runtime, key, value);

                    inst_ix += 1;
                }

                RawInst::GetDunder { object, dunder } => todo!(),

                RawInst::SetDunder {
                    object,
                    dunder,
                    value,
                } => {
                    let mut object = values.get(*object).unwrap();
                    let value = values.get(*value).unwrap();

                    match dunder {
                        Dunder::Unary(_) => todo!(),
                        Dunder::Infix(_) => todo!(),
                        Dunder::DocComment => {
                            object.setattr_static(&mut self.runtime, "__doc__", value)
                        }
                    }

                    inst_ix += 1;
                }

                RawInst::GetItem { object, index } => todo!(),
                RawInst::SetItem {
                    object,
                    index,
                    value,
                } => todo!(),

                RawInst::Return { value } => {
                    let value = values.get(*value).unwrap();

                    break 'outer value;
                }

                RawInst::Call {
                    callable,
                    arguments,
                } => {
                    let object = self.runtime.objects.reserve();

                    values.set(inst_ix, object);

                    inst_ix += 1;
                }

                RawInst::Import { path, relative } => {
                    let Self {
                        module_object,
                        host,
                        runtime,
                        ..
                    } = self;

                    let base = (*relative != 0).then(|| (*relative, module_object.path.as_path()));

                    let modules = host.import_module(path, base);
                    let (mref, sr) = modules.first().unwrap();

                    let object = runtime.consteval(*mref, *host)?;
                    let object = runtime.object_graph.alloc_id_of(object).unwrap();

                    let (st, hash) = self.string(
                        self.host
                            .spanref_to_str(path[0].clone())
                            .to_string()
                            .as_str(),
                    )?;
                    frame_object
                        .locals
                        .insert(path[0].group(), (hash, st, object));

                    values.set(inst_ix, object);

                    inst_ix += 1;
                }

                RawInst::Tuple(elements) => {
                    let object = self.runtime.objects.reserve();

                    values.set(inst_ix, object);

                    inst_ix += 1;
                }

                RawInst::Undefined => todo!(),

                RawInst::If {
                    test,
                    truthy,
                    falsey,
                } => todo!(),

                RawInst::Defn {
                    name,
                    params,
                    returns,
                    sequence_id,
                } => {
                    let object = self.string("foo")?.0;

                    values.set(inst_ix, object);

                    inst_ix += 1;
                }

                RawInst::Class { name } => {
                    let name = self.host.spanref_to_str(name.clone()).to_string();
                    let object_class = self
                        .runtime
                        .internals
                        .getattr_static(self.runtime, "_object_type_")
                        .unwrap();

                    let object = self.runtime.define_new_static_class(name, &[object_class]);

                    values.set(inst_ix, object);

                    inst_ix += 1;
                }
            }
        };

        Ok((final_value, values))
    }

    pub fn eval(mut self) -> PyResult<ObjAllocId> {
        assert!(self.call_stack.is_empty());

        self.call_stack
            .push((0, 0, StackFrame::Module(self.module)));

        let (module, values) = self.eval_frame((0, 0, StackFrame::Module(self.module)))?;

        Ok(module)
    }
}
