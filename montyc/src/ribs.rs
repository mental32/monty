use ahash::AHashMap;
use montyc_core::TypeId;

pub type RibData = AHashMap<u32, TypeId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RibType {
    Builtins,
    Global,
    Local,
}

#[derive(Debug, Clone)]
pub(crate) struct Ribs(Vec<(RibType, RibData)>);

impl Ribs {
    #[inline]
    pub fn new(initial: Option<RibData>, kind: Option<RibType>) -> Self {
        let initial = initial
            .map(|r| vec![(kind.unwrap_or(RibType::Global), r)])
            .unwrap_or_default();

        Self(initial)
    }

    /// Associate a name's span group with a type.
    #[inline]
    pub fn add(&mut self, key: u32, value: TypeId) {
        log::trace!("[Ribs::add] adding unqiue key={:?} as {:?}", key, value);

        self.0.push({
            let mut rib = AHashMap::new();
            rib.insert(key, value);
            (RibType::Local, rib)
        });
    }

    /// Extend by adding multiple entries into one level.
    #[inline]
    pub fn extend(&mut self, it: impl Iterator<Item = (u32, TypeId)>) {
        let rib: AHashMap<_, _> = it.collect();
        log::trace!("[Ribs::add] extending rib={:?}", rib);

        self.0.push((RibType::Local, rib));
    }

    /// Get the type associated with a name's span group.
    #[inline]
    pub fn get(&self, key: u32) -> Option<(TypeId, RibType)> {
        self.0
            .iter()
            .rev()
            .find_map(|(kind, data)| data.get(&key).map(|v| (*v, *kind)))
    }
}
