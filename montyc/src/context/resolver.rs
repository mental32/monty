use std::{cell::RefCell, rc::Rc};

use dashmap::DashMap;

use crate::{
    parser::{SpanEntry, SpanRef},
    typing::TypeMap,
};

use super::ModuleRef;

#[derive(Debug)]
pub struct InternalResolver {
    pub span_ref: Rc<RefCell<SpanRef>>,
    pub sources: DashMap<ModuleRef, Rc<str>>,
    pub type_map: Rc<TypeMap>,
}

impl InternalResolver {
    pub fn resolve(&self, mref: ModuleRef, name: impl Into<SpanEntry>) -> Option<String> {
        let reference = name.into();
        let source = self
            .sources
            .get(&mref)
            .expect("module does not exist!")
            .clone();

        self.span_ref
            .borrow()
            .resolve_ref(reference, source.as_ref())
            .map(ToString::to_string)
    }
}
