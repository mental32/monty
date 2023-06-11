use std::cell::RefCell;
use std::hash::{BuildHasher, Hash, Hasher};
use std::rc::Rc;

use dashmap::DashMap;
use montyc_parser::ast::{AstNode, Constant};
use petgraph::graph::{DiGraph, NodeIndex};

use montyc_core::{Function, MapT, ModuleRef, TaggedValueId, TypeId, Value, ValueId};
use montyc_flatcode::{FlatCode, FlatSeq};
use montyc_hlirt::ObjectId;

pub type RawValueGraph = DiGraph<Value, ()>;

pub trait GVKey {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<ValueId>;
}

impl GVKey for TypeId {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<ValueId> {
        store.get_by_assoc(*self)?.resolve(store)
    }
}

impl GVKey for ValueId {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<ValueId> {
        let ix = NodeIndex::from(self.0);

        store
            .value_graph
            .borrow()
            .node_weight(ix)
            .map(|_| self.clone())
    }
}

impl<const TAG: usize> GVKey for TaggedValueId<TAG> {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<ValueId> {
        self.0.resolve(store)
    }
}

impl GVKey for ModuleRef {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<ValueId> {
        store.get_by_assoc(*self)?.resolve(store)
    }
}

impl GVKey for ObjectId {
    fn resolve<'a>(&self, store: &'a GlobalValueStore) -> Option<ValueId> {
        store.get_by_assoc(*self)?.resolve(store)
    }
}

#[derive(Debug, Default)]
pub struct ValueMetadata {
    /// The value index which this metadata applies to.
    pub value_ix: ValueId,

    /// A module reference.
    pub module_ref: Option<ModuleRef>,

    /// A type id slot.
    pub type_id: Option<TypeId>,

    /// An alloc slot.
    pub object_id: Option<ObjectId>,

    /// A rib for the value.
    pub rib: Option<MapT<u32, TypeId>>,

    /// The class instance of this value.
    pub class: Option<ValueId>,

    /// The parent scope of this value.
    pub parent_scope: Option<ValueId>,

    /// Associated code as a function.
    pub function: Option<Function>,

    /// Associated AST node.
    pub ast: Option<AstNode>,

    /// Associated flatcode.
    pub flatcode: Option<(FlatSeq, Option<Rc<FlatCode>>)>,

    /// References to other values held internally within this one.
    pub internal_refs: Option<Vec<ValueId>>,

    /// CFG for Codegen.
    pub cg_flowgraph: Option<montyc_core::codegen::CgBlockCFG<Constant>>,
}

#[derive(Debug, Default)]
pub struct GlobalValueStore {
    hasher: ahash::RandomState,

    /// The global "object" graph.
    value_graph: RefCell<RawValueGraph>,

    /// Metadata for every object.
    metadata: DashMap<ValueId, ValueMetadata>,

    /// Association cache used to optionally assocaite TypeId's or ModuleRef's with values.
    assoc_cache: DashMap<u64, ValueId>,
}

impl GlobalValueStore {
    fn hash_assoc_key<K>(&self, key: K) -> ((std::any::TypeId, u64), K)
    where
        K: std::any::Any + std::hash::Hash,
    {
        let ty = std::any::TypeId::of::<K>();
        let hash = {
            let mut hasher = self.hasher.build_hasher();
            ty.hash(&mut hasher);
            key.hash(&mut hasher);
            hasher.finish()
        };

        ((ty, hash), key)
    }
}

impl GlobalValueStore {
    /// Check if a key is contained within the store.
    #[inline]
    pub fn contains(&self, key: impl GVKey) -> bool {
        key.resolve(self).is_some()
    }

    /// "associates" a given key with the ValueId.
    #[inline]
    pub fn get_by_assoc<K>(&self, key: K) -> Option<ValueId>
    where
        K: std::any::Any + std::hash::Hash + 'static,
    {
        let ((_, hash), _) = self.hash_assoc_key(key);

        let value = self.assoc_cache.get(&hash)?.value().clone();

        Some(value)
    }

    /// "associates" a given key with the ValueId.
    #[inline]
    pub fn assoc<K>(&self, key: K, value: ValueId)
    where
        K: std::any::Any + std::hash::Hash + 'static,
    {
        let ((_, hash), _) = self.hash_assoc_key(key);
        self.assoc_cache.entry(hash).or_insert_with(|| value);
    }

    #[inline]
    pub(crate) fn insert(&self, value: Value) -> ValueId {
        let index = self.value_graph.borrow_mut().add_node(value.clone());
        let id = ValueId(index.index() as u32);

        self.metadata.entry(id).or_insert_with(|| ValueMetadata {
            value_ix: id,
            ..Default::default()
        });

        index.into()
    }
}

impl GlobalValueStore {
    pub fn with_value<T>(&self, key: impl GVKey, f: impl FnOnce(&Value) -> T) -> Option<T> {
        let value_id = key.resolve(self)?;
        let graph = self.value_graph.borrow();
        let val = graph.node_weight(NodeIndex::from(value_id.0))?;

        Some(f(val))
    }

    pub fn with_value_mut<T>(&self, key: impl GVKey, f: impl FnOnce(&mut Value) -> T) -> Option<T> {
        let value_id = key.resolve(self)?;
        let mut graph = self.value_graph.borrow_mut();
        let val = graph.node_weight_mut(NodeIndex::from(value_id.0))?;

        Some(f(val))
    }

    pub fn with_metadata<T>(
        &self,
        key: impl GVKey,
        f: impl FnOnce(&ValueMetadata) -> T,
    ) -> Option<T> {
        let value = key.resolve(self)?;
        let get = self.metadata.get(&value)?;
        let meta = get.value();

        Some(f(meta))
    }

    pub fn with_metadata_mut<T>(
        &self,
        key: impl GVKey,
        f: impl FnOnce(&mut ValueMetadata) -> T,
    ) -> Option<T> {
        let value = key.resolve(self)?;
        let mut meta = self.metadata.get_mut(&value)?;

        Some(f(meta.value_mut()))
    }

    #[inline]
    pub fn alloc_id_of(&self, ix: ValueId) -> Option<ObjectId> {
        self.metadata.get(&ix)?.object_id
    }

    #[inline]
    pub fn type_id_of(&self, ix: ValueId) -> Option<TypeId> {
        self.metadata.get(&ix)?.type_id
    }
}
