use std::{
    cell::{Ref, RefCell},
    hash::{BuildHasher, Hasher},
};

use montyc_core::{utils::SSAMap, ModuleRef};
use petgraph::graph::{Node, NodeIndex};

use crate::{interpreter::object::PyObject, typing::TypingContext, ModuleObject};

use super::{
    object::{self, RawObject},
    HostGlue, ObjAllocId, PyResult,
};

#[derive(Debug, Default)]
struct LookupGraph(petgraph::graph::DiGraph<ObjAllocId, ()>);

impl LookupGraph {
    #[inline]
    pub fn insert(&mut self, node: ObjAllocId) -> NodeIndex<u32> {
        self.0.add_node(node)
    }

    #[inline]
    pub fn nest(&mut self, child: NodeIndex<u32>, parent: NodeIndex<u32>) {
        self.0.add_edge(child, parent, ());
    }

    #[inline]
    pub fn search(
        &mut self,
        start: NodeIndex<u32>,
        f: impl Fn(ObjAllocId) -> bool,
    ) -> Option<ObjAllocId> {
        let nodes = self.0.raw_nodes();
        let mut index = start;
        let mut node = nodes[index.index()].weight;

        'outer: loop {
            if f(node) {
                break Some(node);
            }

            for neighbour in self
                .0
                .neighbors_directed(index, petgraph::EdgeDirection::Outgoing)
            {
                index = neighbour;
                node = nodes[index.index()].weight;
                continue 'outer;
            }

            break None;
        }
    }
}

/// An interpreter runtime capable of executing some Python.
#[derive(Debug)]
pub struct Runtime {
    /// A budget that gets spent everytime the runtime executes some code.
    op_ticks: u64,

    /// A map that contains every object.
    pub(super) objects: SSAMap<ObjAllocId, RefCell<Box<dyn PyObject>>>,

    /// per-Runtime state that allows producing multiple hashers with the same seed.
    hash_state: ahash::RandomState,

    /// per-Runtime global singleton builtins.
    builtins: ObjAllocId,

    /// A map of the imported modules <=> their module object.
    modules: ahash::AHashMap<ModuleRef, ObjAllocId>,

    /// A graph of all the objects that act like as namespaces i.e. modules, classes, functions.
    scope_graph: LookupGraph,
}

impl Runtime {
    /// Create a new `Runtime` with a maximum tick count of `op_ticks`.
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
        };

        // class type(Self): ...
        let type_class = this.define_new_class(&[]);

        // class object(type): ...
        let object_class = this.define_new_class(&[type_class]);

        // class ModuleType(object): ...
        let module_class = this.define_new_class(&[object_class]);

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
        let str_class = this.define_new_class(&[object_class]);

        builtins.setattr_static(&mut this, "str", str_class);

        // class int(object): ...
        let int_class = this.define_new_class(&[object_class]);

        builtins.setattr_static(&mut this, "int", int_class);

        // class function(object): ...
        let func_class = this.define_new_class(&[object_class]);

        builtins.setattr_static(&mut this, "_function_type_", func_class);

        // __monty = ModuleType()
        let __monty = this.new_object_from_class(module_class);
        this.modules.insert(ModuleRef(0), __monty);

        this
    }

    fn define_new_class(&mut self, bases: &[ObjAllocId]) -> ObjAllocId {
        let class_alloc_id = self.objects.reserve();
        let base_class_id = bases.get(0).cloned().unwrap_or(class_alloc_id);

        let class_object = RawObject {
            alloc_id: class_alloc_id,
            __dict__: Default::default(),
            __class__: base_class_id,
        };

        let class_object = RefCell::new(Box::new(class_object) as Box<_>);

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

        let object = RefCell::new(Box::new(object) as Box<dyn PyObject>);

        self.objects.try_set_value(object_alloc_id, object).unwrap();

        object_alloc_id
    }

    #[inline]
    pub(super) fn get_object<'this>(
        &'this self,
        alloc_id: ObjAllocId,
    ) -> Option<&RefCell<Box<dyn PyObject>>> {
        self.objects.get(alloc_id)
    }

    /// Hash any hashable using the runtime hash state.
    #[inline]
    pub(super) fn hash(&self, thing: impl std::hash::Hash) -> super::HashKeyT {
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

    pub fn consteval<'global, 'module>(
        &'global mut self,
        mref: ModuleRef,
        gcx: &'global dyn HostGlue,
    ) -> PyResult<crate::Object> {
        let mut module_object = super::object::alloc::ObjAllocId(usize::MAX);

        gcx.with_module(mref, &mut |module| {
            module_object = eval::ModuleExecutor::new(self, module, gcx).run_until_complete()?;
            Ok(())
        })?;

        let mut module_object = crate::Object {
            type_id: TypingContext::Module,
            properties: Default::default(),
        };

        Ok(module_object)
    }
}

pub(super) mod eval;
