pub mod ceval;
pub mod scope_graph;

use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::{BuildHasher, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use ahash::AHashMap;
use montyc_core::patma;
use montyc_core::{utils::SSAMap, value, ModuleRef};
use montyc_parser::AstObject;
use petgraph::graph::NodeIndex;

use self::scope_graph::{ScopeGraph, ScopeIndex};

use super::object::ToValue;
use super::{
    object::{
        self, alloc::ObjAllocId, class::ClassObj, int::IntObj, string::StrObj, PyObject, RawObject,
    },
    PyDictRaw, PyResult,
};
use crate::{
    flatcode::FlatCode, interpreter::runtime::ceval::ConstEvalContext,
    value_store::GlobalValueStore, HostGlue, ModuleData, ModuleObject, Value, ValueGraphIx,
};

pub(in crate::interpreter) type SharedMutAnyObject = Rc<RefCell<Rc<dyn PyObject>>>;
pub(in crate::interpreter) type ObjectMap = SSAMap<ObjAllocId, SharedMutAnyObject>;

#[derive(Debug, Default, Clone)]
pub(in crate::interpreter) struct Singletons {
    // Modules
    pub(in crate::interpreter) builtins: ObjAllocId,
    pub(in crate::interpreter) monty: ObjAllocId,

    // Classes
    pub(in crate::interpreter) function_class: ObjAllocId,
    pub(in crate::interpreter) module_class: ObjAllocId,
    pub(in crate::interpreter) string_class: ObjAllocId,

    // Constants
    pub(in crate::interpreter) none_v: ObjAllocId,
    pub(in crate::interpreter) false_v: ObjAllocId,
    pub(in crate::interpreter) true_v: ObjAllocId,
}

/// An interpreter runtime capable of executing some Python.
#[derive(Debug)]
pub struct Runtime {
    /// A budget that gets spent everytime the runtime executes some code.
    op_ticks: usize,

    /// A map that contains every object.
    pub(in crate::interpreter) objects: ObjectMap,

    /// per-Runtime state that allows producing multiple hashers with the same seed.
    hash_state: ahash::RandomState,

    /// per-Runtime global singletons.
    internals: ObjAllocId,

    /// A reference to the `__builtins__` module.
    builtins: ObjAllocId,

    /// A map of the imported modules <=> their module object.
    modules: AHashMap<ModuleRef, ObjAllocId>,

    /// A graph of all the objects that act like as namespaces i.e. modules, classes, functions.
    pub(in crate::interpreter) scope_graph: ScopeGraph,

    /// The scope index for the builtins module.
    pub(in crate::interpreter) builtins_scope: ScopeIndex,

    /// Easy access global singletons.
    pub(in crate::interpreter) singletons: Singletons,

    /// A reference to the shared global store where produced values are kept.
    value_store: Rc<RefCell<GlobalValueStore>>,

    /// A map for interned strings.
    strings: AHashMap<u64, ObjAllocId>,
}

impl Runtime {
    /// Start creating a `Runtime` with a maximum tick count of `op_ticks`.
    ///
    /// The `new` constructor is cheap and will return a second stage initialization
    /// constructor which works to initialize the `__monty` module and prepare everything
    /// for proper evaluation.
    ///
    #[inline]
    pub fn new(op_ticks: usize, value_store: Rc<RefCell<GlobalValueStore>>) -> Self {
        let mut objects = SSAMap::new();
        let mut internals = objects.reserve();

        let mut runtime = Self {
            op_ticks,
            internals,
            builtins: internals,
            objects,
            value_store,
            hash_state: ahash::RandomState::new(),
            modules: Default::default(),
            singletons: Default::default(),
            scope_graph: Default::default(),
            builtins_scope: Default::default(),
            strings: Default::default(),
        };

        let internals_module_object = Rc::new(RawObject {
            alloc_id: internals,
            __dict__: Default::default(),
            __class__: internals,
        });

        let internals_module_object_ptr = Rc::as_ptr(&internals_module_object);

        runtime
            .objects
            .try_set_value(
                internals,
                Rc::new(RefCell::new(internals_module_object as Rc<dyn PyObject>)),
            )
            .unwrap();

        macro_rules! static_class {
            ($name:ident(): ...) => {{
                let class_name = stringify!($name);
                let class_object = runtime.define_new_static_class(class_name, &[]);

                let class_name_hash = runtime.hash(class_name);
                internals.set_attribute_direct(
                    &mut runtime,
                    class_name_hash,
                    class_object,
                    class_object,
                );

                class_object
            }};

            ($name:ident($parent:ident): ...) => {{
                let class_name = stringify!($name);
                let class_object = runtime.define_new_static_class(class_name, &[$parent]);

                let class_name_hash = runtime.hash(class_name);
                internals.set_attribute_direct(
                    &mut runtime,
                    class_name_hash,
                    class_object,
                    class_object,
                );

                class_object
            }};
        }

        // Allocate the priomoridal classes.

        let type_class = static_class!(type(): ...);
        let object_class = static_class!(object(type_class): ...);
        let module_class = static_class!(module(object_class): ...);

        runtime.singletons.module_class = module_class;

        {
            /// SAFETY: Nothing else accesses `internals_module_object` while we hot-patch the class slot.
            let internals_object_mut =
                unsafe { &mut *(internals_module_object_ptr as *mut RawObject) };

            internals_object_mut.__class__ = module_class;
        }

        let int_class = static_class!(int(object_class): ...);
        let bool_class = static_class!(bool(int_class): ...);

        let none_class = static_class!(NoneType(object_class): ...);
        let none = runtime.new_object_from_class(none_class);
        runtime.singletons.none_v = none;

        let true_v = runtime.new_object_from_class(bool_class);
        runtime.singletons.true_v = true_v;

        let false_v = runtime.new_object_from_class(bool_class);
        runtime.singletons.false_v = false_v;

        runtime.singletons.string_class = static_class!(str(object_class): ...);

        static_class!(float(object_class): ...);
        static_class!(dict(object_class): ...);
        static_class!(function(object_class): ...);

        // Allocate the special `__monty` module.
        let __monty = runtime.new_object_from_class(module_class);
        runtime.singletons.monty = __monty;
        runtime.modules.insert(ModuleRef(0), __monty);

        runtime
    }

    /// Get the runtimes hash state.
    pub fn hash_state(&self) -> ahash::RandomState {
        self.hash_state.clone()
    }

    /// Initialize the runtime with the builtin `__monty` module.
    pub fn initialize_monty_module(&mut self, host: &mut dyn HostGlue) {
        let mut __monty = self.singletons.monty;

        if __monty.getattr_static(self, "initialized").is_none() {
            {
                let module_object = host.module_data(ModuleRef(0)).unwrap();
                let code = FlatCode::default();
                let mut cx = ConstEvalContext::new(self, host, &code, __monty, &module_object);

                let (initialized_st, hash) = cx.string("initialized").unwrap();
                __monty.set_attribute_direct(
                    cx.runtime,
                    hash,
                    initialized_st,
                    cx.runtime.singletons.true_v,
                );

                let extern_func = cx
                    .runtime
                    .new_object_from_class(cx.runtime.singletons.function_class);

                let (extern_st, hash) = cx.string("extern").unwrap();
                __monty.set_attribute_direct(self, hash, extern_st, extern_func);
            }

            let mut value_store = self.value_store.borrow_mut();

            let __monty_ix = {
                let index = value_store.value_graph.add_node(Value::Module {
                    mref: ModuleRef(0),
                    properties: Default::default(),
                });

                value_store.alloc_data.insert(__monty, index);
                value_store.module_data.insert(ModuleRef(0), index);

                value_store.metadata(index).alloc_id.replace(__monty);

                index
            };

            let mut properties = {
                let mut raw_value =
                    <_ as ToValue>::into_raw_value(&(&*self, &__monty), &*value_store);

                <_ as ToValue>::refine_value(
                    &(&*self, &__monty),
                    &mut raw_value,
                    &mut value_store,
                    __monty_ix,
                );

                patma!(o.properties, Value::Object(o) in raw_value).unwrap()
            };

            match value_store.value_graph.node_weight_mut(__monty_ix).unwrap() {
                Value::Module {
                    mref,
                    properties: slot,
                } => std::mem::swap(&mut properties, slot),

                _ => unreachable!(),
            }
        }
    }

    fn define_new_static_class(&mut self, name: impl ToString, bases: &[ObjAllocId]) -> ObjAllocId {
        let class_alloc_id = self.objects.reserve();
        let base_class_id = bases.get(0).cloned().unwrap_or(class_alloc_id);

        let class_object = RawObject {
            alloc_id: class_alloc_id,
            __dict__: Default::default(),
            __class__: base_class_id,
        };

        let class_object = ClassObj {
            header: class_object,
            name: name.to_string(),
        };

        let class_object = Rc::new(RefCell::new(Rc::new(class_object) as Rc<dyn PyObject>));

        self.objects
            .try_set_value(class_alloc_id, class_object)
            .unwrap();

        class_alloc_id
    }

    fn new_object_from_class(&mut self, class: ObjAllocId) -> ObjAllocId {
        let object_alloc_id = self.objects.reserve();

        let object = RawObject {
            alloc_id: object_alloc_id,
            __dict__: Default::default(),
            __class__: class,
        };

        self.objects.try_set_value(object_alloc_id, object).unwrap();

        object_alloc_id
    }

    #[inline]
    pub(super) fn get_object<'this>(&'this self, alloc_id: ObjAllocId) -> Option<Rc<dyn PyObject>> {
        Some(self.objects.get(alloc_id)?.borrow().clone())
    }

    /// Hash any hashable using the runtime hash state.
    #[inline]
    pub fn hash(&self, thing: impl std::hash::Hash) -> super::HashKeyT {
        let mut hasher = self.hash_state.build_hasher();

        thing.hash(&mut hasher);

        hasher.finish()
    }

    #[inline]
    #[allow(dead_code)] // TODO: Remove when actually used.
    pub(in crate::interpreter) fn try_as_int_value(&self, alloc_id: ObjAllocId) -> PyResult<i64> {
        let obj = self.objects.get(alloc_id).unwrap().clone();
        let obj = &*obj.borrow();

        if let Some(int) = obj.as_any().downcast_ref::<IntObj>() {
            Ok(int.value)
        } else {
            todo!("TypeError: not an integer");
        }
    }

    #[inline]
    pub(in crate::interpreter) fn try_as_str_value(
        &self,
        alloc_id: ObjAllocId,
    ) -> PyResult<String> {
        let obj = self.objects.get(alloc_id).unwrap().clone();
        let obj = &*obj.borrow();

        if let Some(st) = obj.as_any().downcast_ref::<StrObj>() {
            Ok(st.value.clone())
        } else {
            todo!("TypeError: not a string");
        }
    }

    /// Const-evaluate a module.
    pub fn consteval<'global, 'module>(
        &'global mut self,
        mref: ModuleRef,
        gcx: &'global mut dyn HostGlue,
    ) -> PyResult<(ValueGraphIx, FlatCode)> {
        let module = gcx
            .module_data(mref)
            .expect("Modules should always have associated data.");

        let mut code = FlatCode::new();
        module.ast.visit_with(&mut code);

        let mut value_store = self.value_store.borrow_mut();

        if let Some(index) = self
            .modules
            .get(&mref)
            .and_then(|alloc| value_store.alloc_data.get(alloc))
        {
            return Ok((*index, code));
        }

        let mut module_object = self.objects.reserve();

        if self.builtins == self.internals {
            // INVARIANT: assuming the first module consteval'd is actually the builtins module.
            self.singletons.builtins = module_object;
            self.builtins = module_object;
            self.builtins_scope = self.scope_graph.insert(module_object);
        } else {
            let module_scope = self.scope_graph.insert(module_object);
            self.scope_graph.nest(module_scope, self.builtins_scope);
        }

        let module_index = value_store.value_graph.add_node(Value::Module {
            mref,
            properties: Default::default(),
        });

        value_store.alloc_data.insert(module_object, module_index);
        value_store.module_data.insert(mref, module_index);

        let module_type = self.singletons.module_class;

        self.objects
            .try_set_value(
                module_object,
                RawObject {
                    __class__: module_object,
                    __dict__: Default::default(),
                    alloc_id: module_object,
                },
            )
            .unwrap();

        std::mem::drop(value_store);

        let _ = ConstEvalContext::new(self, gcx, &code, module_object, &module)
            .eval()
            .unwrap();

        let mut value_store = self.value_store.borrow_mut();

        value_store.alloc_data.insert(module_object, module_index);

        let raw_value = {
            let mut raw_value =
                <_ as ToValue>::into_raw_value(&(&*self, &module_object), &*value_store);

            {
                let m = self.objects.get(module_object).unwrap().borrow();
                let m = m.as_any();
                let m = m.downcast_ref::<RawObject>().unwrap();

                for (_, (key, _)) in m.__dict__.iter() {
                    assert_eq!(key.class_alloc_id(self), self.singletons.string_class);
                }

                <_ as ToValue>::refine_value(
                    &(&*self, m),
                    &mut raw_value,
                    &mut value_store,
                    module_index,
                );
            }

            // <_ as ToValue>::refine_value(
            //     &(&*self, &module_object),
            //     &mut raw_value,
            //     &mut value_store,
            //     module_index,
            // );

            raw_value
        };

        if let Some(Value::Module {
            properties: blank_properties,
            ..
        }) = value_store.value_graph.node_weight_mut(module_index)
        {
            let mut properties = patma!(o.properties, Value::Object(o) in raw_value).unwrap();

            std::mem::swap(blank_properties, &mut properties);
        }

        Ok((module_index, code))
    }
}
