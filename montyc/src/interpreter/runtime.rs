use std::{any::Any, num::NonZeroUsize, rc::Rc};

use dashmap::DashMap;

use crate::{
    ast::{stmt::Statement, Spanned},
    context::{GlobalContext, ModuleRef},
    typing::TypeMap,
};

use super::{object::{MutCell, Object}, scope::DynamicScope};

#[derive(Debug)]
pub struct Singletons {
    int_class: Rc<Object>,
    str_class: Rc<Object>,
    bool_class: Rc<Object>,
    func_class: Rc<Object>,
    none: Rc<Object>,
}

impl Singletons {
    pub fn new() -> Self {
        Self {
            int_class: Rc::new(Object { type_id: TypeMap::TYPE, members: DashMap::new(), prototype: None }),
            str_class: Rc::new(Object { type_id: TypeMap::TYPE, members: DashMap::new(), prototype: None }),
            bool_class: Rc::new(Object { type_id: TypeMap::TYPE, members: DashMap::new(), prototype: None }),
            func_class: Rc::new(Object { type_id: TypeMap::TYPE, members: DashMap::new(), prototype: None }),
            none: Rc::new(Object { type_id: TypeMap::NONE_TYPE, members: DashMap::new(), prototype: None }),
        }
    }
}

pub struct RuntimeContext<'a> {
    global_context: &'a GlobalContext,
    singletons: Singletons,
    value_name: (NonZeroUsize, NonZeroUsize),
    pub(super) stack_frames: Vec<Rc<DynamicScope>>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(global_context: &'a GlobalContext) -> Self {
        Self {
            global_context,
            singletons: Singletons::new(),
            value_name: global_context.magical_name_of("__value").unwrap(),
            stack_frames: vec![],
        }
    }

    pub fn scope(&self) -> Rc<DynamicScope> {
        self.stack_frames.last().unwrap().clone()
    }

    pub fn is_truthy(&self, v: Rc<Object>) -> bool {
        match v.type_id {
            TypeMap::INTEGER => v.get_member(self.value_name).unwrap().into_inner().downcast_ref::<Rc<isize>>().map(|c| *c.as_ref() == 1_isize).unwrap_or(false),
            TypeMap::BOOL => v.get_member(self.value_name).unwrap().into_inner().downcast_ref::<Rc<bool>>().map(|r| *r.as_ref() == true).unwrap_or(false),
            _ => unimplemented!(),
        }
    }

    pub fn integer(&self, value: isize) -> Rc<Object> {
        Rc::new(Object {
            type_id: TypeMap::INTEGER,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.value_name.clone(),
                    MutCell::Immutable(Rc::new(value) as Rc<dyn Any>),
                );

                members
            },

            prototype: Some(self.singletons.func_class.clone()),
        })
    }

    pub fn boolean(&self, b: bool) -> Rc<Object> {
        Rc::new(Object {
            type_id: TypeMap::BOOL,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.value_name.clone(),
                    MutCell::Immutable(Rc::new(b) as Rc<dyn Any>),
                );

                members
            },

            prototype: Some(self.singletons.func_class.clone()),
        })
    }

    pub fn string(&self, st: NonZeroUsize, _mref: ModuleRef) -> Rc<Object> {
        Rc::new(Object {
            type_id: TypeMap::STRING,
            members: {
                let members = DashMap::new();

                members.insert(
                    self.value_name.clone(),
                    MutCell::Immutable(Rc::new(st) as Rc<dyn Any>),
                );

                members
            },

            prototype: Some(self.singletons.func_class.clone()),
        })
    }

    pub fn none(&self) -> Rc<Object> {
        self.singletons.none.clone()
    }

    pub fn function(&self, def: Rc<Spanned<Statement>>) -> Rc<Object> {
        assert_matches!(&def.inner, Statement::FnDef(_));

        Rc::new(Object {
            type_id: TypeMap::DYN_FUNC,
            members: DashMap::new(),
            prototype: Some(self.singletons.func_class.clone()),
        })
    }
}
