use std::{rc::Rc};

use crate::{context::LocalContext, typing::{TypeMap, TypedObject}};

use super::{atom::Atom, AstObject, ObjectIter, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    Atomic(Atom),

    /// `<value:primary>[<index?>]`
    Subscript {
        value: Rc<Spanned<Primary>>,
        index: Option<Rc<Spanned<Primary>>>,
    },

    /// `<func:primary>(<args?>)`
    Call {
        func: Rc<Spanned<Primary>>,
        args: Option<Rc<Spanned<Primary>>>,
    },

    /// `<primary> DOT(.) <atom>`
    Attribute {
        left: Rc<Spanned<Primary>>,
        attr: Spanned<Atom>,
    },

    /// `(await +)+<primary>`
    Await(Rc<Spanned<Primary>>),
}

impl AstObject for Primary {
    fn walk<'a>(&'a self) -> Option<ObjectIter> {
        let it = match self {
            Primary::Atomic(_) => return None,
            Primary::Await(inner) => return inner.walk(),

            Primary::Subscript { value, index } => {
                let mut v = vec![value.clone() as Rc<dyn AstObject>];

                if index.is_some() {
                    v.push(Rc::new(index.clone()) as Rc<dyn AstObject>)
                }

                v
            }

            Primary::Call { func, args } => {
                let mut v = vec![func.clone() as Rc<dyn AstObject>];

                if args.is_some() {
                    v.push(Rc::new(args.clone()) as Rc<dyn AstObject>);
                }

                v
            }

            Primary::Attribute { left, attr } => {
                vec![
                    left.clone() as Rc<dyn AstObject>,
                    Rc::new(attr.clone()) as Rc<dyn AstObject>,
                ]
            }
        };

        Some(Box::new(it.into_iter()))
    }

    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }
}

impl TypedObject for Primary {
    fn infer_type<'a>(
        &self,
        ctx: &LocalContext<'a>,
    ) -> Option<crate::typing::LocalTypeId> {
        match self {
            Primary::Atomic(at) => at.infer_type(ctx),
            Primary::Subscript { value: _, index: _ } => None,
            Primary::Call { func: _, args: _ } => None,
            Primary::Attribute { left: _, attr: _ } => None,
            Primary::Await(_) => None,
        }
    }

    fn typecheck<'a>(
        &self,
        ctx: LocalContext<'a>,
    ) {
        match self {
            Primary::Atomic(at) => at.typecheck(ctx),
            Primary::Subscript { value, index } => {}
            Primary::Call { func, args } => {}
            Primary::Attribute { left, attr } => {}
            Primary::Await(_) => {}
        }
    }
}
