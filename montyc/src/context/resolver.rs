use std::{cell::RefCell, collections::HashMap, rc::Rc};

use dashmap::DashMap;

use crate::{parser::{SpanEntry, SpanRef}, typing::{LocalTypeId, TypeMap}};

use super::ModuleRef;


#[derive(Debug)]
pub struct InternalResolver {
    pub span_ref: Rc<RefCell<SpanRef>>,
    pub sources: DashMap<ModuleRef, Rc<str>>,
    pub type_map: Rc<TypeMap>,
}

impl InternalResolver {
    pub fn resolve_type(&self, type_id: LocalTypeId) -> Option<String> {
        let type_map = &self.type_map;
        let type_desc = type_map.get(type_id)?.value().clone();

        Some(format!("{}", type_desc))
    }

    pub fn resolve(&self, mref: ModuleRef, name: impl Into<SpanEntry>) -> Option<String> {
        let reference = name.into();
        let source = self.sources.get(&mref).unwrap().clone();

        self.span_ref
            .borrow()
            .resolve_ref(reference, source.as_ref())
            .map(ToString::to_string)
    }
}
