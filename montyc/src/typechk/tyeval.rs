//! Given the AST of a function, semantically evaluate it to reveal the types of the nodes.
//!

use std::{cell::RefCell, rc::Rc};

use montyc_core::{ModuleRef, TypeId};
use montyc_hlir::{typing::TypingContext, ObjectGraph};
use montyc_parser::{AstNode, AstObject, ast::{Atom, Expr}};

use crate::{global::value_context::ValueContext, prelude::GlobalContext, ribs::Ribs};

use super::Typecheck;

#[derive(Debug, Clone)]
pub(crate) struct TypeEvalContext<'gcx, 'this> {
    pub value_cx: &'this ValueContext<'this, 'gcx>,
    pub expected_return_value: TypeId,
    pub ribs: Rc<RefCell<Ribs>>,
}

impl<'gcx, 'this> Typecheck<TypeEvalContext<'gcx, 'this>, Option<TypeId>> for AstNode {
    fn typecheck(&self, cx: TypeEvalContext) -> montyc_core::MontyResult<Option<TypeId>> {
        match self {
            AstNode::Int(_) => Ok(Some(TypingContext::Int)),
            AstNode::Str(_) => Ok(Some(TypingContext::Str)),
            AstNode::Bool(_) => Ok(Some(TypingContext::Bool)),
            AstNode::Float(_) => Ok(Some(TypingContext::Float)),
            AstNode::None(_) => Ok(Some(TypingContext::None)),
            AstNode::Pass => Ok(None),

            AstNode::Comment(_) => panic!("Can't type evaluate a comment."),

            AstNode::ClassDef(_) => todo!(),
            AstNode::FuncDef(_) => todo!(),
            AstNode::Import(_) => todo!(),
            AstNode::If(_) => todo!(),

            AstNode::Assign(asn) => {
                let name = asn.name.inner.as_name().unwrap();
                let value_type = asn
                    .value
                    .into_ast_node()
                    .typecheck(cx.clone())?
                    .expect("Assignment expressions should always produce a typed value");

                if let Some(expected) = &asn.kind {
                    let expected_type = expected
                        .into_ast_node()
                        .typecheck(cx.clone())?
                        .expect("Expected type of assignment should always produce a typed value");

                    if value_type != expected_type {
                        todo!("Assignment expression type checking failed {:?} != {:?}", value_type, expected_type);
                    }
                }

                log::trace!("[TypeEvalContext::typecheck] Assigning name={:?} as type={:?}", name, value_type);

                cx.ribs.borrow_mut().add(name.group(), value_type);

                Ok(None)
            }

            AstNode::Tuple(Atom::Tuple(elems)) => {
                let mut elements = vec![];

                for elem in elems {
                    let elem_t = elem.into_ast_node().typecheck(cx.clone())?.expect("Tuple elements should always produce a typed value");
                    elements.push(elem_t);
                }

                cx.value_cx.gcx.typing_context.borrow_mut().tuple(elements);

                Ok(None)
            },

            AstNode::Name(name) => {
                let sref = name.clone().unwrap_name();
                let (type_id, rib_type) = cx.ribs.borrow().get(sref.group()).expect("NameError");

                log::trace!("[TypeEvalContext::typecheck] Performing name lookup {:?} -> {:?} (rib_type={:?})", sref, type_id, rib_type);

                Ok(Some(type_id))
            }

            AstNode::BinOp(Expr::BinOp { left: _, op: _, right: _ }) => {

                Ok(todo!())
            },

            AstNode::IfExpr(Expr::If { test: _, body: left, orelse: right }) => {
                let left = left.into_ast_node().typecheck(cx.clone())?.expect("If expression left side should always produce a typed value");
                let right = right.into_ast_node().typecheck(cx.clone())?.expect("If expression right side should always produce a typed value");

                let type_id = if left != right {
                    // Synthesize a new union type for the two sides.
                    todo!("cx.gcx.typing_context.borrow_mut().union(left, right)");
                } else {
                    left
                };

                Ok(Some(type_id))
            },

            AstNode::Unary(_) => todo!(),
            AstNode::NamedExpr(_) => todo!(),

            AstNode::Ellipsis(_) => todo!(),
            AstNode::Subscript(_) => todo!(),

            AstNode::Call(_) => todo!(),

            AstNode::Ret(ret) => {
                let ret_t = ret
                    .value
                    .as_ref()
                    .map(|expr| expr.into_ast_node().typecheck(cx.clone()))
                    .unwrap_or(Ok(Some(TypingContext::None)))?
                    .expect("return value expression should have a type.");

                log::trace!("[TypeEvalContext::typecheck] Checking return value types {:?} == {:?}", ret_t, cx.expected_return_value);

                if cx.expected_return_value != ret_t {
                    todo!(
                        "mismatched return value {:?} != {:?}",
                        ret_t,
                        cx.expected_return_value
                    );
                }

                Ok(None)
            }

            // truly unreachable patterns.
            AstNode::IfExpr(_)
            | AstNode::BinOp(_)
            | AstNode::Tuple(_) => unreachable!("should've already been handled above. {:?}", self),
        }
    }
}
