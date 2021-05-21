use std::{any::Any, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{typing::LocalTypeId};

#[derive(Clone, Debug)]
pub enum MutCell<T> {
    Immutable(T),
    Mutable(T),
}

impl<T> MutCell<T> {
    pub fn into_inner(self) -> T {
        match self {
            MutCell::Immutable(t) => t,
            MutCell::Mutable(t) => t,
        }
    }
}

pub struct Object {
    /// The type of this object.
    pub(super) type_id: LocalTypeId,

    /// Basically like `__dict__`
    pub(super) members: DashMap<(NonZeroUsize, NonZeroUsize), MutCell<Rc<dyn Any>>>,

    /// Reference to the class instance of this object.
    pub(super) prototype: Option<Rc<Object>>,
}

impl Object {
    pub fn get_member(&self, name: (NonZeroUsize, NonZeroUsize)) -> Option<MutCell<Rc<dyn Any>>> {
        let refm = self.members.get(&name)?;
        Some(refm.value().clone())
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Object").finish()
    }
}
