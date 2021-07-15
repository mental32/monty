//! Given the AST of a function, semantically evaluate it to reveal the types of the nodes.
//!

use std::{cell::RefCell, ops::Range, rc::Rc};

use montyc_core::{MontyError, TypeError, TypeId};
use montyc_hlir::{HostGlue, Value, typing::TypingContext};
use montyc_parser::{
    ast::{Atom, Expr, FunctionDef, Primary},
    spanned::Spanned,
    AstNode, AstObject,
};

use crate::{global::value_context::ValueContext, ribs::Ribs};

use super::Typecheck;

#[derive(Debug, Clone)]
pub(crate) struct TypeEvalContext<'gcx, 'this> {
    pub value_cx: &'this ValueContext<'this, 'gcx>,
    pub expected_return_value: TypeId,
    pub ribs: Rc<RefCell<Ribs>>,
}

impl<'gcx, 'this, A> Typecheck<TypeEvalContext<'gcx, 'this>, Option<TypeId>> for Spanned<A>
where
    A: AstObject,
{
    fn typecheck(&self, cx: TypeEvalContext) -> montyc_core::MontyResult<Option<TypeId>> {
        let Spanned { span, inner } = self;
        let node = inner.into_ast_node();

        (node, span.clone()).typecheck(cx)
    }
}

impl<'gcx, 'this> Typecheck<TypeEvalContext<'gcx, 'this>, Option<TypeId>>
    for (AstNode, Range<usize>)
{
    fn typecheck(&self, cx: TypeEvalContext) -> montyc_core::MontyResult<Option<TypeId>> {
        let (node, span) = self;

        match node {
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
                    .typecheck(cx.clone())?
                    .expect("Assignment expressions should always produce a typed value");

                if let Some(expected) = &asn.kind {
                    let expected_type = expected
                        .typecheck(cx.clone())?
                        .expect("Expected type of assignment should always produce a typed value");

                    if value_type != expected_type {
                        todo!(
                            "Assignment expression type checking failed {:?} != {:?}",
                            value_type,
                            expected_type
                        );
                    }
                }

                log::trace!(
                    "[TypeEvalContext::typecheck] Assigning name={:?} as type={:?}",
                    name,
                    value_type
                );

                cx.ribs.borrow_mut().add(name.group(), value_type);

                Ok(None)
            }

            AstNode::Tuple(Atom::Tuple(elems)) => {
                let mut elements = vec![];

                for elem in elems {
                    let elem_t = elem
                        .typecheck(cx.clone())?
                        .expect("Tuple elements should always produce a typed value");
                    elements.push(elem_t);
                }

                cx.value_cx.gcx.typing_context.borrow_mut().tuple(elements);

                Ok(None)
            }

            AstNode::Name(name) => {
                let sref = name.clone().unwrap_name();
                let (type_id, rib_type) = match cx.ribs.borrow().get(sref.group()) {
                    Some(i) => dbg!(i),
                    None => {
                        return Err(MontyError::TypeError {
                            module: cx.value_cx.mref,
                            error: TypeError::UndefinedVariable { sref: cx.value_cx.gcx.spanref_to_str(sref).to_string() },
                        })
                    }
                };

                if type_id == TypingContext::Unknown {
                    return Err(MontyError::TypeError {
                        module: cx.value_cx.mref,
                        error: TypeError::UnknownType { sref: cx.value_cx.gcx.spanref_to_str(sref).to_string() },
                    });
                }

                log::trace!("[TypeEvalContext::typecheck] Performing name lookup {:?} -> {:?} (rib_type={:?})", sref, type_id, rib_type);

                Ok(Some(type_id))
            }

            AstNode::BinOp(Expr::BinOp { left, op: _, right }) => {
                let left_type = left.typecheck(cx.clone())?.expect(
                    "Left-hand side of binary operation should always produce a typed value",
                );

                let value_store = cx.value_cx.gcx.value_store.borrow();

                let left_class = value_store.class_of(left_type);
                log::trace!("[TypeEvalContext::typecheck] Getting class of type_id={:?} -> {:?}", left_type, left_class);

                let _right_type = right.typecheck(cx.clone())?.expect(
                    "Right-hand side of binary operation should always produce a typed value",
                );

                Ok(todo!("{:?}", left_class))
            }

            AstNode::IfExpr(Expr::If {
                test: _,
                body: left,
                orelse: right,
            }) => {
                let left = left
                    .typecheck(cx.clone())?
                    .expect("If expression left side should always produce a typed value");

                let right = right
                    .typecheck(cx.clone())?
                    .expect("If expression right side should always produce a typed value");

                let type_id = if left != right {
                    // Synthesize a new union type for the two sides.
                    todo!("cx.gcx.typing_context.borrow_mut().union(left, right)");
                } else {
                    left
                };

                Ok(Some(type_id))
            }

            AstNode::Unary(_) => todo!(),
            AstNode::NamedExpr(_) => todo!(),

            AstNode::Ellipsis(_) => todo!(),
            AstNode::Subscript(_) => todo!(),

            AstNode::Call(Primary::Call { func, args: _ }) => {
                let _func = func
                    .typecheck(cx.clone())?
                    .expect("Call expression function should always produce a typed value");

                todo!();
            }

            AstNode::Ret(ret) => {
                let ret_t = ret
                    .value
                    .as_ref()
                    .map(|expr| expr.typecheck(cx.clone()))
                    .unwrap_or(Ok(Some(TypingContext::None)))?
                    .expect("return value expression should have a type.");

                log::trace!(
                    "[TypeEvalContext::typecheck] Checking return value types {:?} == {:?}",
                    ret_t,
                    cx.expected_return_value
                );

                if cx.expected_return_value != ret_t {
                    let funcdef = if let Value::Function {
                        defsite: Some(defsite),
                        ..
                    } = cx.value_cx.value
                    {
                        cx.value_cx.get_node_from_module_body(*defsite).unwrap()
                    } else {
                        unreachable!();
                    };

                    let def_node = match funcdef {
                        AstNode::FuncDef(FunctionDef { name, returns, .. }) => {
                            name.span.start
                                ..returns.map(|ret| ret.span.end).unwrap_or(name.span.end)
                        }
                        _ => unreachable!(),
                    };

                    return Err(MontyError::TypeError {
                        module: cx.value_cx.mref,
                        error: TypeError::BadReturnType {
                            expected: cx.expected_return_value,
                            actual: ret_t,
                            ret_node: span.clone(),
                            def_node,
                        },
                    });
                }

                Ok(None)
            }

            // truly unreachable patterns.
            AstNode::IfExpr(_) | AstNode::BinOp(_) | AstNode::Call(_) | AstNode::Tuple(_) => {
                unreachable!("should've already been handled above. {:?}", self)
            }
        }
    }
}
