use ahash::AHashMap;
use montyc_core::TypeId;

pub type DefScope = AHashMap<u32, TypeId>;

// -- DefKind

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Empty,
    Builtins,
    Global,
    Parameters,
    Local,
}

impl Default for DefKind {
    fn default() -> Self {
        Self::Builtins
    }
}

// -- ScopeChainIter

pub struct ScopeChainIter<'a> {
    name: u32,
    kind: DefKind,
    source: &'a ScopeChain,
}

impl<'a> Iterator for ScopeChainIter<'a> {
    type Item = (TypeId, DefKind);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.kind;

        let namespace = match kind {
            DefKind::Empty => return None,
            DefKind::Builtins => &self.source.builtins,
            DefKind::Global => &self.source.globals,
            DefKind::Parameters => &self.source.parameters,
            DefKind::Local => &self.source.locals,
        };

        self.kind = match kind {
            DefKind::Empty => unreachable!(),
            DefKind::Builtins => DefKind::Empty,
            DefKind::Global => DefKind::Builtins,
            DefKind::Parameters => DefKind::Global,
            DefKind::Local => DefKind::Parameters,
        };

        match namespace.get(&self.name) {
            Some(type_id) => {
                return Some((*type_id, kind));
            }

            None => self.next(),
        }
    }
}

// -- ScopeChain

#[derive(Debug, Clone, Default)]
pub(crate) struct ScopeChain {
    pub builtins: DefScope,
    pub globals: DefScope,
    pub parameters: DefScope,
    pub locals: DefScope,
}

impl ScopeChain {
    #[inline]
    pub fn new(builtins: DefScope) -> Self {
        Self {
            builtins,
            ..Default::default()
        }
    }

    #[inline]
    pub fn lookup<'a>(&'a self, name: u32) -> impl Iterator<Item = (TypeId, DefKind)> + 'a {
        ScopeChainIter {
            name,
            source: self,
            kind: DefKind::Local,
        }
    }
}
