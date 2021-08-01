pub mod ceval;
pub mod object_graph;
pub mod scope_graph;

use std::{
    cell::RefCell,
    hash::{BuildHasher, Hasher},
    rc::Rc,
};

use montyc_core::{utils::SSAMap, ModuleRef};
use montyc_parser::AstObject;

use self::scope_graph::{ScopeGraph, ScopeIndex};

use super::{
    object::{
        alloc::ObjAllocId, class::ClassObj, int::IntObj, string::StrObj, PyObject, RawObject,
    },
    PyResult,
};
use crate::{
    flatcode::FlatCode, interpreter::runtime::ceval::ConstEvalContext, HostGlue, ModuleObject,
    ObjectGraph, ObjectGraphIndex, Value,
};

pub(in crate::interpreter) type SharedMutAnyObject = Rc<RefCell<Rc<dyn PyObject>>>;

/// An interpreter runtime capable of executing some Python.
#[derive(Debug)]
pub struct Runtime {
    /// A budget that gets spent everytime the runtime executes some code.
    op_ticks: usize,

    /// A map that contains every object.
    pub(super) objects: SSAMap<ObjAllocId, SharedMutAnyObject>,

    /// per-Runtime state that allows producing multiple hashers with the same seed.
    hash_state: ahash::RandomState,

    /// per-Runtime global singleton builtins.
    builtins: ObjAllocId,

    /// A map of the imported modules <=> their module object.
    modules: ahash::AHashMap<ModuleRef, ObjAllocId>,

    /// A graph of all the objects that act like as namespaces i.e. modules, classes, functions.
    pub(in crate::interpreter) scope_graph: ScopeGraph,

    /// The scope index for the builtins module.
    pub(in crate::interpreter) builtins_scope: ScopeIndex,

    /// A single output object graph is maintained in the runtime.
    pub object_graph: ObjectGraph,

    /// A map for interned strings.
    strings: ahash::AHashMap<u64, ObjAllocId>,
}

impl Runtime {
    /// Start creating a `Runtime` with a maximum tick count of `op_ticks`.
    ///
    /// The `new` constructor is cheap and will return a second stage initialization
    /// constructor which works to initialize the `__monty` module and prepare everything
    /// for proper evaluation.
    ///
    #[inline]
    pub fn new(op_ticks: usize) -> Self {
        let mut objects = SSAMap::new();
        let mut builtins = objects.reserve();

        let mut this = Self {
            op_ticks,
            builtins,
            objects,
            object_graph: Default::default(),
            modules: Default::default(),
            hash_state: ahash::RandomState::new(),
            scope_graph: Default::default(),
            builtins_scope: ScopeIndex::default(),
            strings: Default::default(),
        };

        // class type(Self): ...
        let type_class = this.define_new_static_class("type", &[]);

        // class object(type): ...
        let object_class = this.define_new_static_class("object", &[type_class]);

        // class ModuleType(object): ...
        let module_class = this.define_new_static_class("module", &[object_class]);

        // __builtins__ = ModuleType()
        this.objects
            .try_set_value(
                builtins,
                RawObject {
                    alloc_id: builtins,
                    __dict__: Default::default(),
                    __class__: module_class,
                },
            )
            .unwrap();

        builtins.setattr_static(&mut this, "_module_type_", module_class);
        builtins.setattr_static(&mut this, "_object_type_", object_class);

        // class str(object): ...
        let str_class = this.define_new_static_class("str", &[object_class]);

        builtins.setattr_static(&mut this, "str", str_class);

        // class int(object): ...
        let int_class = this.define_new_static_class("int", &[object_class]);

        builtins.setattr_static(&mut this, "int", int_class);

        // class dict(object): ...
        let dict_class = this.define_new_static_class("dict", &[object_class]);

        builtins.setattr_static(&mut this, "dict", dict_class);

        // class function(object): ...
        let func_class = this.define_new_static_class("func", &[object_class]);

        builtins.setattr_static(&mut this, "_function_type_", func_class);

        // __monty = ModuleType()
        let __monty = this.new_object_from_class(module_class);
        this.modules.insert(ModuleRef(0), __monty);

        this
    }

    /// Get the runtimes hash state.
    pub fn hash_state(&self) -> ahash::RandomState {
        self.hash_state.clone()
    }

    /// Initialize the runtime with the builtin `__monty` module.
    pub fn initialize_monty_module(&mut self, _: &dyn HostGlue) {
        let mut __monty = self
            .modules
            .get(&ModuleRef(0))
            .cloned()
            .expect("`__monty` module should exist.");

        if __monty
            .get_attribute_direct(self, self.hash("initialized"), __monty)
            .is_none()
        {
            // let extern_func = ex.function_from_closure("extern", |_ex, _args| todo!())?;

            let extern_func = self.new_object_from_class(
                self.builtins
                    .get_attribute_direct(self, self.hash("_function_type_"), __monty)
                    .unwrap(),
            );

            __monty.setattr_static(self, "extern", extern_func);

            // __monty.set_attribute_direct(self, self.hash("initialized"), __monty, __monty);
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

    /// "tick" the global runtime, subtracting one from the execution budget.
    ///
    /// When `op_ticks` reaches zero the function will return `true`
    /// and its recommended that an immediete exception should be raised;
    /// otherwise `false` will be returned.
    ///
    #[inline]
    fn tick(&mut self) -> bool {
        if self.op_ticks == 0 {
            return true;
        } else {
            self.op_ticks -= 1;
            return false;
        }
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
        gcx: &'global dyn HostGlue,
    ) -> PyResult<ObjectGraphIndex> {
        let mut module_object = self.objects.reserve();

        let module_index = self.object_graph.insert_node_traced(
            module_object,
            |_, _| Value::Module {
                mref,
                properties: Default::default(),
            },
            |_, _| (),
        );

        let mut eval = |module: &ModuleObject| {
            let mut code = FlatCode::new();
            module.ast.visit_with(&mut code);

            let cx = ConstEvalContext::new(self, gcx, &code, module_object);

            self.object_graph
                .alloc_to_idx
                .insert(module_object, module_index);

            let mut properties = Default::default();

            module_object.properties_into_values(&mut self.object_graph, &mut properties);

            if let Some(Value::Module {
                properties: blank_properties,
                ..
            }) = self.object_graph.node_weight_mut(module_index)
            {
                std::mem::swap(blank_properties, &mut properties);
            }

            Ok(())
        };

        gcx.with_module(mref, &mut eval);

        Ok(module_index)
    }
}
