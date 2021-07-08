use std::{
    cell::RefCell,
    hash::{BuildHasher, Hasher},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use montyc_core::{utils::SSAMap, ModuleRef};

use petgraph::{EdgeDirection::Outgoing, graph::NodeIndex, visit::NodeIndexable};

use crate::{
    interpreter::{
        object::{int::IntObj, string::StrObj, PyObject},
        runtime::eval::AstExecutor,
    },
    ObjectGraph, ObjectGraphIndex,
};

use super::{
    object::{self, class::ClassObj, RawObject},
    HostGlue, ObjAllocId, PyDictRaw, PyResult,
};

#[derive(Debug, Default)]
pub(in crate::interpreter) struct ScopeGraph(petgraph::graph::DiGraph<ObjAllocId, ()>);

impl DerefMut for ScopeGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Deref for ScopeGraph {
    type Target = petgraph::graph::DiGraph<ObjAllocId, ()>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ScopeGraph {
    #[inline]
    pub fn insert(&mut self, node: ObjAllocId) -> NodeIndex<u32> {
        self.0.add_node(node)
    }

    #[inline]
    pub fn nest(&mut self, child: NodeIndex<u32>, parent: NodeIndex<u32>) {
        self.0.add_edge(child, parent, ());
    }

    #[inline]
    pub(in crate::interpreter) fn parent_of(&self, idx: NodeIndex<u32>) -> Option<ObjAllocId> {
        self.0
            .neighbors_directed(idx, Outgoing)
            .next()
            .and_then(|idx| self.0.node_weight(idx))
            .cloned()
    }

    #[inline]
    pub fn search(
        &self,
        start: ObjAllocId,
        f: impl Fn(ObjAllocId) -> Option<ObjAllocId>,
    ) -> Option<ObjAllocId> {
        let nodes = self.0.raw_nodes();
        let index = {
            nodes
                .iter()
                .enumerate()
                .find(|(_, node)| node.weight == start)?
                .0
        };

        let mut node = nodes[index].weight;
        let mut index = self.0.from_index(index);

        loop {
            if let node @ Some(_) = f(node) {
                break node;
            }

            let parent = self
                .0
                .neighbors_directed(index, petgraph::EdgeDirection::Outgoing)
                .next()?;

            index = parent;
            node = nodes[index.index()].weight;
        }
    }
}

/// An interpreter runtime capable of executing some Python.
#[derive(Debug)]
pub struct Runtime {
    /// A budget that gets spent everytime the runtime executes some code.
    op_ticks: u64,

    /// A map that contains every object.
    pub(super) objects: SSAMap<ObjAllocId, Rc<RefCell<Rc<dyn PyObject>>>>,

    /// per-Runtime state that allows producing multiple hashers with the same seed.
    hash_state: ahash::RandomState,

    /// per-Runtime global singleton builtins.
    builtins: ObjAllocId,

    /// A map of the imported modules <=> their module object.
    modules: ahash::AHashMap<ModuleRef, ObjAllocId>,

    /// A graph of all the objects that act like as namespaces i.e. modules, classes, functions.
    pub(in crate::interpreter) scope_graph: ScopeGraph,

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
    pub fn new(op_ticks: u64) -> Self {
        let mut objects = SSAMap::new();
        let mut builtins = objects.reserve();

        let mut this = Self {
            op_ticks,
            builtins,
            objects,
            modules: Default::default(),
            hash_state: ahash::RandomState::new(),
            scope_graph: Default::default(),
            strings: Default::default(),
        };

        // class type(Self): ...
        let type_class = this.define_new_static_class(&[]);

        // class object(type): ...
        let object_class = this.define_new_static_class(&[type_class]);

        // class ModuleType(object): ...
        let module_class = this.define_new_static_class(&[object_class]);

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
        let str_class = this.define_new_static_class(&[object_class]);

        builtins.setattr_static(&mut this, "str", str_class);

        // class int(object): ...
        let int_class = this.define_new_static_class(&[object_class]);

        builtins.setattr_static(&mut this, "int", int_class);

        // class dict(object): ...
        let dict_class = this.define_new_static_class(&[object_class]);

        builtins.setattr_static(&mut this, "dict", dict_class);

        // class function(object): ...
        let func_class = this.define_new_static_class(&[object_class]);

        builtins.setattr_static(&mut this, "_function_type_", func_class);

        // __monty = ModuleType()
        let __monty = this.new_object_from_class(module_class);
        this.modules.insert(ModuleRef(0), __monty);

        this
    }

    pub fn hash_state(&self) -> ahash::RandomState {
        self.hash_state.clone()
    }

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

    fn define_new_static_class(&mut self, bases: &[ObjAllocId]) -> ObjAllocId {
        let class_alloc_id = self.objects.reserve();
        let base_class_id = bases.get(0).cloned().unwrap_or(class_alloc_id);

        let class_object = RawObject {
            alloc_id: class_alloc_id,
            __dict__: Default::default(),
            __class__: base_class_id,
        };

        let class_object = ClassObj {
            header: class_object,
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

    pub fn consteval<'global, 'module>(
        &'global mut self,
        mref: ModuleRef,
        gcx: &'global dyn HostGlue,
    ) -> PyResult<(ObjectGraphIndex, ObjectGraph)> {
        let mut module_object = super::object::alloc::ObjAllocId(usize::MAX);
        let mut object_graph: crate::ObjectGraph = Default::default();

        let module_index = object_graph.add_node(crate::Value::Module {
            mref,
            properties: Default::default(),
        });

        let mut properties: PyDictRaw<_> = Default::default();

        gcx.with_module(mref, &mut |module| {
            module_object = AstExecutor::new_with_module(self, module, gcx).run_until_complete()?;

            object_graph
                .alloc_to_idx
                .insert(module_object.alloc_id(), module_index);

            module_object.for_each(self, &mut |_, hash, key, value| {
                let key = key.into_value(self, &mut object_graph);
                let key = object_graph.add_string_node(
                    if let crate::Value::String(st) = &key {
                        self.hash(st)
                    } else {
                        unreachable!()
                    },
                    key,
                );

                let value_alloc_id = value.alloc_id();
                let value = value.into_value(self, &mut object_graph);
                let value = object_graph.add_node_traced(value, value_alloc_id);

                properties.insert(hash, (key, value));
            });

            Ok(())
        })?;

        let module_properties = match object_graph.node_weight_mut(module_index).unwrap() {
            crate::Value::Module { properties, .. } => properties,
            _ => unreachable!(),
        };

        std::mem::swap(module_properties, &mut properties);

        Ok((module_index, object_graph))
    }
}

pub(super) mod eval;
