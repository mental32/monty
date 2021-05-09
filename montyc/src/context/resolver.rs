use std::{cell::RefCell, rc::Rc};

use dashmap::DashMap;

use crate::{parser::{SpanInterner, SpanRef}};

use super::ModuleRef;

#[derive(Debug)]
pub struct InternalResolver {
    pub span_ref: Rc<RefCell<SpanInterner>>,
    pub sources: DashMap<ModuleRef, Rc<str>>,
    // pub type_map: Rc<TypeMap>,
}

impl InternalResolver {
    pub fn resolve(&self, mref: ModuleRef, name: SpanRef) -> Option<String> {
        let reference = name.into();

        let source = self
            .sources
            .get(&mref)
            .expect("module does not exist!")
            .clone();

        let range = self.span_ref.borrow().get(reference).unwrap();

        source.get(range).map(|st| st.to_string())
    }
}
