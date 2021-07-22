use ahash::AHashMap;
use montyc_core::TypeId;

pub type DefScope = AHashMap<u32, TypeId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Builtins,
    Global,
    Local,
}

#[derive(Debug, Clone)]
pub(crate) struct DefStack(Vec<(DefKind, DefScope)>);

impl DefStack {
    #[inline]
    pub fn new(initial: Option<DefScope>, kind: Option<DefKind>) -> Self {
        let initial = initial
            .map(|r| vec![(kind.unwrap_or(DefKind::Global), r)])
            .unwrap_or_default();

        Self(initial)
    }

    /// Associate a name's span group with a type.
    #[inline]
    pub fn add(&mut self, key: u32, value: TypeId) {
        log::trace!("[DefStack::add] adding unqiue key={:?} as {:?}", key, value);

        self.0.push({
            let mut rib = AHashMap::new();
            rib.insert(key, value);
            (DefKind::Local, rib)
        });
    }

    /// Extend by adding multiple entries into one level.
    #[inline]
    pub fn extend(&mut self, it: impl Iterator<Item = (u32, TypeId)>) {
        let rib: AHashMap<_, _> = it.collect();
        log::trace!("[DefStack::add] extending rib={:?}", rib);

        self.0.push((DefKind::Local, rib));
    }

    /// Get the type associated with a name's span group.
    #[inline]
    pub fn get(&self, key: u32) -> Option<(TypeId, DefKind)> {
        self.0
            .iter()
            .rev()
            .find_map(|(kind, data)| data.get(&key).map(|v| (*v, *kind)))
    }
}
