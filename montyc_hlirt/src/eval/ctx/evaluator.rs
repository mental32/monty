use std::collections::hash_map::Entry;
use std::hash::{BuildHasher, Hash, Hasher};
use std::{num::NonZeroU64, rc::Rc};

use montyc_core::ast::Constant;
use montyc_core::{patma, ModuleRef, SpanRef};
use montyc_flatcode::SequenceType;
use montyc_flatcode::{raw_inst::Dunder, FlatCode};

use crate::eval::frame::FrameState;
use crate::eval::inst_exec::{InstExec, InstResult};
use crate::exception::{InnerExc, PyException, PyResult, PyResultExt};
use crate::object::{
    AnyFunc, GlobalsHook, ObjectId, PyIter, PyObject, PyValue, RawObject, SharedObject,
};
use crate::rt::{ModuleKey, ModuleMetadata, Runtime, RuntimeHost, RuntimeHostExt};
use crate::storage::ObjectSpace;

use super::{CallCx, EvalGlue};

// -- EvaluationContext

pub struct UnboundEvaluationContext {
    pub(crate) code: Rc<FlatCode>,
    pub(crate) mref: ModuleRef,
}

impl UnboundEvaluationContext {
    /// Bind this evaluation context to a runtime letting it be executable.
    pub fn bind<'a, 'b, H>(
        self,
        rt: &'a mut Runtime,
        host: &'b mut H,
    ) -> EvaluationContext<'a, 'b, H>
    where
        H: RuntimeHostExt,
    {
        EvaluationContext {
            rt,
            host,
            preamble: vec![],
            state: self,
            n_ticks: None,
        }
    }
}

// -- BoundEvaluationContext<'rt>

pub struct EvaluationContext<'rt, 'host, H>
where
    H: RuntimeHostExt,
{
    pub rt: &'rt mut Runtime,
    pub host: &'host mut H,

    preamble: Vec<(ModuleRef, Vec<Box<str>>)>,
    n_ticks: Option<NonZeroU64>,
    pub(crate) state: UnboundEvaluationContext,
}

impl<'rt, 'host, H> EvalGlue for EvaluationContext<'rt, 'host, H>
where
    H: RuntimeHostExt,
{
    #[inline]
    fn runtime_mut<'a>(&'a mut self) -> &'a mut Runtime {
        self.rt
    }

    #[inline]
    fn hash_object(&mut self, object: ObjectId) -> PyResult<u64> {
        let __hash__ = self.rt.hash("__hash__");
        let mut hasher = self.rt.hash_state.build_hasher();

        enum Defered {
            Call(ObjectId),
            CallShared(SharedObject),
            Callable(AnyFunc),
        }

        let hash = |this: &PyValue| -> Option<Defered> {
            match this {
                PyValue::Any(raw) => match raw.__dict__.get(__hash__) {
                    Some((_, hash_method)) => return Some(Defered::Call(hash_method)),
                    None => object.hash(&mut hasher),
                },

                PyValue::Int(n) => n.hash(&mut hasher),
                PyValue::Float(n) => (*n as u64).hash(&mut hasher),
                PyValue::Bool(n) => n.hash(&mut hasher),
                PyValue::Bytes(n) => n.hash(&mut hasher),
                PyValue::Str(n) => n.hash(&mut hasher),

                PyValue::Function { .. }
                | PyValue::Module { .. }
                | PyValue::Class { .. }
                | PyValue::None
                | PyValue::Ellipsis => object.hash(&mut hasher),

                PyValue::List(_) | PyValue::Dict(_) => todo!("TypeError: unhashable type."),

                PyValue::Dynamic(object) => return Some(Defered::CallShared(object.clone())),
                PyValue::Callable(f) => return Some(Defered::Callable(f.clone())),
            };

            None
        };

        match self.rt.objects.with_object(object, hash) {
            Some(defered) => {
                let _n = match defered {
                    Defered::Call(assoc_hash_method) => {
                        self.call_object(assoc_hash_method, &[]).trace()?
                    }

                    Defered::CallShared(shared) => shared.hash(self).trace()?,
                    Defered::Callable(any) => self.call_any_func(&any, &[]).trace()?,
                };

                let n = self.rt.objects.with_object(object, |val| match val {
                    PyValue::Int(n) => Ok((*n) as u64),
                    PyValue::Bool(b) => Ok((*b) as u64),
                    _ => PyException::type_error()
                        .set_message("TypeError: __hash__ method should return an integer.")
                        .into(),
                })?;

                Ok(n)
            }

            None => Ok(hasher.finish()),
        }
    }

    #[inline]
    fn iter_object(&mut self, object: ObjectId) -> PyIter {
        self.rt.objects.with_object(object, |this| match this {
            PyValue::Int(_)
            | PyValue::Float(_)
            | PyValue::Bool(_)
            | PyValue::None
            | PyValue::Ellipsis
            | PyValue::Class { .. }
            | PyValue::Module { .. }
            | PyValue::Function { .. } => PyIter::empty(),

            PyValue::List(lst) if lst.is_empty() => PyIter::empty(),

            PyValue::Dynamic(_) => todo!(),

            PyValue::Any(_) => todo!(),

            PyValue::Bytes(_) => todo!(),

            PyValue::Str(_) => todo!(),

            PyValue::List(lst) => PyIter::from(lst.iter().cloned()),
            PyValue::Dict(dict) => PyIter::from(dict.values().map(|kv| kv.0)),
            PyValue::Callable(_) => todo!(),
        })
    }

    #[inline]
    fn call_object(&mut self, callable: ObjectId, arguments: &[ObjectId]) -> PyResult<ObjectId> {
        let callable = self
            .rt
            .objects
            .with_object(callable, |this| match this {
                PyValue::Dynamic(o) => Ok(PyValue::Dynamic(o.clone())),
                PyValue::Callable(func) => Ok(PyValue::Callable(func.clone())),
                PyValue::Function {
                    body,
                    params,
                    parent,
                    returns,
                    inner,
                } => Ok(PyValue::Function {
                    body: body.clone(),
                    params: params.clone(),
                    parent: parent.clone(),
                    inner: inner.clone(),
                    returns: returns.clone(),
                }),

                _ => PyException::type_error()
                    .set_message("not a callable object")
                    .into(),
            })
            .trace()?;

        let none_v = self.rt.singletons.none_v;

        match callable {
            PyValue::Dynamic(obj) => obj.call(CallCx::new(self, arguments, none_v)),
            PyValue::Callable(func) => self.call_any_func(&func, arguments),
            PyValue::Function { body, params, .. } => {
                let (module, seq_id) =
                    patma!((m, s), AnyFunc::Code { module: m, seq_id: s} in body).unwrap();

                let seq = &module.sequences()[seq_id];
                let mut frame = FrameState::new(None, Some(module.mref));

                for (n, ((p, _), a)) in params.iter().zip(arguments.iter()).enumerate() {
                    self.define(&mut frame, p.group(), *a).trace()?;
                    frame.values.insert(n, *a);
                }

                self.exec_seq_with_frame(seq, frame)
            }

            _ => unreachable!(),
        }
    }

    fn call_method_object(
        &mut self,
        object: ObjectId,
        method: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let obj = self.rt.objects.with_object(object, |this| match this {
            PyValue::Dynamic(obj) => obj.clone(),
            _ => todo!("call_method_object"),
        });

        obj.call_method(self, method, args).trace()
    }

    fn repr_object(&mut self, object: ObjectId) -> PyResult<ObjectId> {
        let obj = self.rt.objects.with_object(object, |this| match this {
            PyValue::Dynamic(obj) => Err(obj.clone()),
            PyValue::Int(n) => Ok(format!("{}", n)),
            PyValue::Float(n) => Ok(format!("{}", n)),

            PyValue::Bool(true) => Ok(format!("True")),
            PyValue::Bool(false) => Ok(format!("False")),

            PyValue::None => Ok(format!("None")),
            PyValue::Ellipsis => Ok(format!("...")),
            PyValue::Bytes(bytes) => Ok(format!("b'{:x?}'", bytes.as_slice())),
            PyValue::Str(st) => Ok(format!("{:?}", st)),
            PyValue::List(_) => todo!(),
            PyValue::Dict(_) => todo!(),
            PyValue::Module { .. } => todo!(),
            PyValue::Any(_) => todo!(),
            PyValue::Function { .. } => todo!(),
            PyValue::Class { .. } => todo!(),
            PyValue::Callable(_) => todo!(),
        });

        let repr = self.new_string("__repr__")?;

        match obj {
            Err(obj) => obj.call_method(self, repr, &[]).trace(),
            Ok(repr) => self.new_string(&repr),
        }
    }

    fn getattr_object(&mut self, object: ObjectId, _attr: ObjectId) -> PyResult<ObjectId> {
        self.rt.objects.with_object(object, |this| match this {
            PyValue::Any(_) => todo!(),
            PyValue::Int(_) => todo!(),
            PyValue::Float(_) => todo!(),
            PyValue::Bool(_) => todo!(),
            PyValue::None => todo!(),
            PyValue::Ellipsis => todo!(),
            PyValue::Bytes(_) => todo!(),
            PyValue::Str(_) => todo!(),
            PyValue::List(_) => todo!(),
            PyValue::Dict(_) => todo!(),
            PyValue::Callable(_) => todo!(),
            PyValue::Module { .. } => todo!(),
            PyValue::Function { .. } => todo!(),
            PyValue::Class { .. } => todo!(),
            PyValue::Dynamic(_) => todo!(),
        })
    }

    fn self_as_dyn<'a>(&'a mut self) -> &'a mut dyn EvalGlue {
        self
    }

    fn runtime_host_mut(&mut self) -> &mut dyn RuntimeHost {
        self.host
    }

    fn runtime(&self) -> &Runtime {
        &self.rt
    }

    fn runtime_host(&self) -> &dyn RuntimeHost {
        &*self.host
    }
}

impl<'rt, 'host, H> InstExec for EvaluationContext<'rt, 'host, H>
where
    H: RuntimeHostExt,
{
    fn ref_as_str(&mut self, frame: &mut FrameState, r: SpanRef) -> InstResult {
        let st = self.host.spanref_to_str(r).to_owned();
        let st = self.new_string(st.as_str()).trace()?;

        let _ = frame.values.insert(frame.current_inst_ix, st);

        Ok(frame.next_inst())
    }

    fn class(&mut self, frame: &mut FrameState, name: SpanRef) -> InstResult {
        let class_obj = self.rt.objects.insert(PyValue::Class {
            name: Some(name),
            parent: frame.frame_object,
            inner: Default::default(),
        });

        self.define(frame, name.group(), class_obj)?;

        frame.values.insert(frame.current_inst_ix, class_obj);

        Ok(frame.next_inst())
    }

    fn call(&mut self, frame: &mut FrameState, callable: usize, arguments: &[usize]) -> InstResult {
        let callable = frame.values[&callable];
        let arguments = arguments
            .iter()
            .map(|val| frame.values[val])
            .collect::<Vec<_>>();

        let rv = self.call_object(callable, arguments.as_ref())?;

        frame.values.insert(frame.current_inst_ix, rv);

        Ok(frame.next_inst())
    }

    fn set_attribute(
        &mut self,
        _frame: &mut FrameState,
        _object: usize,
        _attr: usize,
        _value: usize,
    ) -> InstResult {
        todo!()
    }

    fn get_attribute(&mut self, frame: &mut FrameState, object: usize, attr: usize) -> InstResult {
        let attr = frame.values[&attr];
        let object = frame.values[&object];

        let attr_value = self.getattr(object, &[attr])?;

        frame.values.insert(frame.current_inst_ix, attr_value);

        Ok(frame.next_inst())
    }

    fn phi_jump(&mut self, frame: &mut FrameState, recv: usize, value: usize) -> InstResult {
        let value = frame.values[&value];
        frame.values.insert(recv, value);
        Ok(recv)
    }

    fn if_(
        &mut self,
        frame: &mut FrameState,
        test: usize,
        truthy: Option<usize>,
        falsey: Option<usize>,
    ) -> InstResult {
        let test = frame.values[&test];
        let inst_ix = frame.current_inst_ix;

        let case = if test == self.rt.singletons.true_v {
            truthy
        } else {
            assert_eq!(test, self.rt.singletons.false_v);

            falsey
        };

        let inst_ix = if let Some(dst) = case {
            dst
        } else {
            inst_ix + 1
        };

        Ok(inst_ix)
    }

    fn define_fn(
        &mut self,
        frame: &mut FrameState,
        name: SpanRef,
        params: &[(SpanRef, Option<usize>)],
        returns: Option<usize>,
        seq_id: usize,
    ) -> InstResult {
        let module = Rc::clone(&self.state.code);

        let returns = returns.map(|ix| frame.values[&ix]);

        let params = params
            .iter()
            .map(|(var, ann)| (var.clone(), ann.map(|ix| frame.values[&ix])))
            .collect();

        let func_obj = self
            .rt
            .new_function(AnyFunc::Code { module, seq_id }, params, returns);

        self.define(frame, name.group(), func_obj)?;
        frame.values.insert(frame.current_inst_ix, func_obj);

        Ok(frame.next_inst())
    }

    fn set_var(&mut self, frame: &mut FrameState, var: SpanRef, value: usize) -> InstResult {
        let value = frame.values[&value];

        self.define(frame, var.group(), value)?;

        Ok(frame.next_inst())
    }

    fn use_var(&mut self, frame: &mut FrameState, var: SpanRef) -> InstResult {
        let val = self.lookup(frame, &var)?;

        frame.values.insert(frame.current_inst_ix, val);

        Ok(frame.next_inst())
    }

    fn import(&mut self, frame: &mut FrameState, path: &[SpanRef], relative: usize) -> InstResult {
        let mref = frame.mref.clone().unwrap_or(self.state.mref);
        let module = self.import_module(mref, path, relative).trace()?;

        let _ = frame.values.insert(frame.current_inst_ix, module);

        Ok(frame.next_inst())
    }

    fn return_(&mut self, frame: &mut FrameState, value: usize) -> InstResult {
        let object = frame.values[&value];

        Err(PyException::return_(object))
    }

    fn const_(&mut self, frame: &mut FrameState, cst: &Constant) -> InstResult {
        let slot = frame.values.entry(frame.current_inst_ix);

        let value = match cst {
            Constant::None => self.rt.singletons.none_v,
            Constant::Ellipsis => self.rt.singletons.ellipsis_v,

            Constant::Int(n) => self.rt.new_int(*n),

            Constant::Float(_f) => {
                todo!(); /*self.rt.new_float(*f)} */
            }

            Constant::Bool(b) => {
                if *b {
                    self.rt.singletons.true_v
                } else {
                    self.rt.singletons.false_v
                }
            }

            Constant::String(st) => {
                let st = self.host.spanref_to_str(*st);
                self.rt.new_string(st)
            }
        };

        match slot {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
            }

            Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }

        Ok(frame.next_inst())
    }

    fn set_dunder(
        &mut self,
        frame: &mut FrameState,
        object: usize,
        dunder: Dunder,
        value: usize,
    ) -> InstResult {
        let object = frame.values[&object];
        let value = frame.values[&value];

        match dunder {
            Dunder::Unary(_) => todo!(),
            Dunder::Infix(_) => todo!(),

            Dunder::DocComment => {
                let __doc__ = self.rt.new_string("__doc__");
                self.setattr(object, __doc__, value).trace()?
            }

            Dunder::GetItem => todo!(),
            Dunder::SetItem => todo!(),
            Dunder::AsBool => todo!(),
        }

        Ok(frame.next_inst())
    }

    fn build_class(&mut self, frame: &mut FrameState, sequence: usize, class: usize) -> InstResult {
        let klass = frame.values[&class];

        let seq = self.state.code.sequences()[sequence].clone();
        assert_eq!(seq.kind, SequenceType::Class);

        let klass_frame = FrameState::new(Some(klass), frame.mref);
        self.exec_seq_with_frame(&seq, klass_frame)?;

        Ok(frame.next_inst())
    }
}

impl<'rt, 'host, H> EvaluationContext<'rt, 'host, H>
where
    H: RuntimeHostExt,
{
    #[inline]
    pub(super) fn define(
        &mut self,
        frame: &mut FrameState,
        var: u32,
        value: ObjectId,
    ) -> Result<Option<ObjectId>, PyException> {
        log::trace!(
            "[EvaluationContext::define] defining variable({:?}) := {:?}",
            var,
            value
        );

        if let Some(object) = frame.frame_object.as_ref() {
            let k = self.host.spangroup_to_str(var);
            let k = self.rt.new_string(k);

            self.setattr(*object, k, value)?;
        }

        Ok(frame.locals.insert(var, value))
    }

    #[inline]
    pub(super) fn lookup(&mut self, frame: &mut FrameState, var: &SpanRef) -> PyResult<ObjectId> {
        let var_group = var.group();

        match frame.locals.get(&var_group) {
            Some(obj) => return Ok(*obj),
            None => (),
        }

        let var = *var;

        let frame_object = frame
            .frame_object
            .clone()
            .unwrap_or(self.rt.singletons.builtins);

        let mut this = frame_object;

        let attr = self.rt.hash(self.host.spanref_to_str(var));

        let f = |val: &PyValue, this: ObjectId| -> PyResult<ObjectId> {
            match val {
                // all scope chains eventually resolve to a module object.
                PyValue::Class { parent: None, .. } | PyValue::Module { .. } => {
                    return Err(PyException::name_error(var))
                }

                PyValue::Class {
                    parent: Some(parent),
                    inner: _,
                    ..
                } => {
                    if this == *parent {
                        // The parent of `this` is itself? that's a cycle
                        // and also probably a bug either way its being treated as a NameError.

                        return Err(PyException::name_error(var));
                    }

                    let next = Ok(if this == frame_object { *parent } else { this });

                    return next;
                }

                _ => unreachable!(),
            }
        };

        loop {
            this = self.rt.objects.with_object(this, |v| f(v, this))?;

            if let Ok((_, v)) = self.getattr_direct_hash(this, attr) {
                return Ok(v);
            }
        }
    }

    fn call_any_func(&mut self, func: &AnyFunc, arguments: &[ObjectId]) -> PyResult<ObjectId> {
        log::trace!("[call_any_func] {:?}", func);

        let none_v = self.rt.singletons.none_v;

        match func {
            AnyFunc::Native { inner: f, hook } => {
                let hook_call_cx = CallCx::new(self, arguments, none_v);
                let globals = if let Some(hook) = hook {
                    match hook {
                        GlobalsHook::Globals(glob) => *glob,
                        GlobalsHook::ComputeWith(hook) => hook(hook_call_cx).trace()?,
                    }
                } else {
                    none_v
                };

                let call_cx = CallCx::new(self, arguments, globals);

                f(call_cx)
            }

            AnyFunc::Boxed { inner: f, hook } => {
                let hook_call_cx = CallCx::new(self, arguments, none_v);
                let globals = if let Some(hook) = hook {
                    match hook {
                        GlobalsHook::Globals(glob) => *glob,
                        GlobalsHook::ComputeWith(hook) => hook(hook_call_cx).trace()?,
                    }
                } else {
                    none_v
                };

                let call_cx = CallCx::new(self, arguments, globals);

                (f)(call_cx)
            }

            AnyFunc::Code { module, seq_id } => {
                let seq = &module.sequences()[*seq_id];
                let frame = FrameState::new(None, Some(module.mref));

                self.exec_seq_with_frame(seq, frame)
            }
        }
    }

    pub fn exec_seq_with_frame(
        &mut self,
        seq: &montyc_flatcode::FlatSeq,
        mut frame: FrameState,
    ) -> PyResult<ObjectId> {
        while let Some(inst) = seq.inst().get(frame.current_inst_ix) {
            frame.current_inst_ix = match self.exec_inst(&mut frame, inst) {
                Ok(ip) => ip,

                Err(PyException {
                    inner: InnerExc::Return(rv),
                    ..
                }) => return Ok(rv),

                Err(exc) => return Err(exc),
            };

            self.tick()?;
        }

        Ok(self.rt.singletons.none_v)
    }

    #[inline]
    fn tick(&mut self) -> Result<(), PyException> {
        match self.n_ticks.map(NonZeroU64::get) {
            Some(1) => return Err(PyException::tick()),
            Some(n) => {
                self.n_ticks = NonZeroU64::new(n.saturating_sub(1));
            }

            None => (),
        };

        Ok(())
    }

    pub(crate) fn import_module(
        &mut self,
        mref: ModuleRef,
        path: &[SpanRef],
        relative: usize,
    ) -> PyResult<ObjectId> {
        let import = self.host.import_module_spec(mref, path, relative);
        let spec = (import)(self).trace()?;

        let module = self.rt.singletons.module_class;

        if self.rt.class_of(spec) == module {
            assert_ne!(spec, ObjectId::default());
            return Ok(spec);
        } else {
            self.compile_from_spec(spec)
        }
    }

    pub(crate) fn compile_from_spec(&mut self, spec: ObjectId) -> PyResult<ObjectId> {
        let mut inner = RawObject::default();

        let name = self.getattr(spec, &"name").trace()?;
        let loader = self.getattr(spec, &"loader").trace()?;
        let get_data = self.new_string("get_data").trace()?;

        let source = self.call_method_object(loader, get_data, &[]).trace()?;

        let source = self
            .runtime_mut()
            .objects
            .with_object(source, |this| match this {
                PyValue::Str(st) => st.clone(),
                _ => unreachable!(),
            });

        let code = self
            .host
            .as_accept_input()
            .accept_input(&source)
            .map(Rc::new)
            .unwrap();

        let mref = code.mref;

        let module = {
            let none_v = self.rt.singletons.none_v;

            let module_init_dict = [
                self.rt.make_kv_pair("__name__", |_| name),
                self.rt.make_kv_pair("__doc__", |_| none_v),
                // self.rt.make_kv_pair("__package__", |_| none_v),
                self.rt.make_kv_pair("__loader__", |_| loader),
                self.rt.make_kv_pair("__spec__", |_| spec),
            ];

            for (h, k, v) in module_init_dict {
                inner.__dict__.insert(h, (k, v));
            }

            inner.__class__ = self.rt.singletons.module_class;

            // rt.objects.with_object_mut(sys_modules, |this| match this {
            //     PyValue::Dict(map) => map.insert(name_hash, (name, none_v)),
            //     val => todo!("{:?}", val),
            // });

            self.rt.objects.insert_with(|_| PyValue::Module {
                mkey: ModuleKey::User(mref),
                inner,
            })
        };

        let meta = ModuleMetadata {
            mref,
            alloc: module,
            code: Some(Rc::clone(&code)),
        };

        self.rt.module_objects.insert(mref, meta);

        Ok(module)
    }
}

impl<'rt, 'host, H> EvaluationContext<'rt, 'host, H>
where
    H: RuntimeHostExt,
{
    /// Unbind the current context from its runtime and host letting you defer processing of it.
    pub fn unbind(self) -> UnboundEvaluationContext {
        let Self { state, .. } = self;

        state
    }

    /// Set the upper "tick" bound of this evalutaion, or None if unbounded.
    ///
    /// Every instruction accounts for one "tick", if you set it to `None` then
    /// the runtime is unbounded. If the computed ticks reach 1 during the execution
    /// of code then an `ExceededTickError` exception will be thrown.
    ///
    pub fn set_upper_tick_bound(mut self, n_ticks: Option<NonZeroU64>) -> Self {
        self.n_ticks = n_ticks;
        self
    }

    pub fn from_module_import<I, S>(mut self, module: ModuleRef, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        self.preamble.push((
            module,
            names
                .into_iter()
                .map(|s| s.as_ref().to_string().into_boxed_str())
                .collect(),
        ));

        self
    }

    /// Run the given code to the completion of sequence 0 (the module sequence.)
    pub fn run_until_complete(mut self) -> PyResult<ObjectId> {
        let code = Rc::clone(&self.state.code);
        let mref = code.mref();

        let module = self
            .rt
            .module_objects
            .get(&mref)
            .ok_or_else(|| todo!("Module does not exist..."))?
            .alloc;

        let (module_seq, _) = match code.sequences() {
            [] => return Ok(module),
            [module_seq, rest @ ..] => (module_seq, rest),
        };

        let mut frame = FrameState::new(Some(module), Some(mref));

        let mut preamble = vec![];

        std::mem::swap(&mut preamble, &mut self.preamble);

        if !self.rt.singletons.none_v.is_uninit() {
            const BUILTINS: &[&str] = &["int", "bool", "str"];

            if self.rt.singletons.builtins.is_uninit() {
                let builtin_values = [
                    self.rt.singletons.int_class,
                    self.rt.singletons.bool_class,
                    self.rt.singletons.string_class,
                ];

                for (name, value) in BUILTINS.iter().zip(builtin_values) {
                    if value.is_uninit() {
                        continue;
                    }

                    let hash = self.rt.hash(name);

                    let name = self.host.spangroup_of_hash(hash, name);

                    self.define(&mut frame, name, value)?;
                }
            } else if self.rt.singletons.builtins != self.rt.singletons.none_v {
                for name in BUILTINS {
                    let hash = self.rt.hash(&name);

                    let (_, value) = self
                        .getattr_direct_hash(self.rt.singletons.builtins, hash)
                        .unwrap();

                    let name = self.host.spangroup_of_hash(hash, &name);

                    self.define(&mut frame, name, value)?;
                }
            }
        }

        for (mref, names) in preamble {
            let module = self
                .rt
                .module_objects
                .get(&mref)
                .ok_or_else(|| todo!("Module does not exist..."))?;

            let module = module.alloc;

            for name in names {
                let hash = self.rt.hash(&name);

                let (_, value) = self.getattr_direct_hash(module, hash).unwrap();
                let name = self.host.spangroup_of_hash(hash, &name);

                self.define(&mut frame, name, value)?;
            }
        }

        while let Some(inst) = module_seq.inst().get(frame.current_inst_ix) {
            frame.current_inst_ix = match self.exec_inst(&mut frame, inst) {
                Ok(ip) => ip,

                Err(PyException {
                    inner: InnerExc::Return(_obj),
                    ..
                }) => unreachable!("Return in module body."),

                Err(exc) => return Err(exc),
            };

            self.tick()?;
        }

        Ok(module)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::{object::ObjectBuilder, test::*};

    #[test]
    pub fn eval_getattr() {
        let (mut rt, mut host) = setup();

        #[derive(Debug)]
        struct N;

        impl PyObject for N {
            unsafe fn std_type_id(&self) -> std::any::TypeId {
                std::any::TypeId::of::<Self>()
            }

            fn get_attribute(&self, ecx: &mut dyn EvalGlue, _attr: ObjectId) -> PyResult<ObjectId> {
                ecx.new_int(0)
            }
        }

        let module =
            ObjectBuilder::<{ montyc_core::MODULE }>::new().setattr("foo", SharedObject::new(N));

        let mref = host.mrefs.into();

        host.mrefs += 1;

        let _module = rt.synthesise_module(mref, module).unwrap();

        let result = rt
            .eval(&mut host, "a = foo.bar")
            .unwrap()
            .from_module_import(mref, ["foo"])
            .run_until_complete()
            .unwrap();

        let result_dict = rt.objects.with_object(result, |this| match this {
            PyValue::Module { inner, .. } => inner.__dict__.clone(),
            _ => unimplemented!(),
        });

        let a = result_dict.get(rt.hash("a")).unwrap().1;
        rt.objects.with_object(a, |this| match this {
            PyValue::Int(n) => assert_eq!(*n, 0),
            _ => unimplemented!(),
        });
    }
}
