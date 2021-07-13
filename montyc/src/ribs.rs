use ahash::AHashMap;
use montyc_core::TypeId;

pub type RibData = AHashMap<u32, TypeId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RibType {
    Global,
    Local,
}

#[derive(Debug)]
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
        self.0.push({
            let mut rib = AHashMap::new();
            rib.insert(key, value);
            (RibType::Local, rib)
        });
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
