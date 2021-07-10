use ahash::AHashMap;
use montyc_core::{ModuleRef, SpanRef, TypeId};
use montyc_hlir::{typing::TypingContext, ObjectGraph};
use montyc_parser::{ast::Atom, AstNode, AstObject};

use crate::prelude::GlobalContext;

use super::Typecheck;

#[derive(Debug, Clone)]
pub struct TypeEvalContext<'gcx, 'this> {
    pub mref: ModuleRef,
    pub gcx: &'gcx GlobalContext,
    pub object_graph: &'this ObjectGraph,
    pub expected_return_value: TypeId,
    pub names: AHashMap<u32, TypeId>,
}

impl<'gcx, 'this> Typecheck<TypeEvalContext<'gcx, 'this>, Option<TypeId>> for AstNode {
    fn typecheck(&self, cx: TypeEvalContext) -> montyc_core::MontyResult<Option<TypeId>> {
        match self {
            AstNode::Import(_) => todo!(),

            AstNode::ClassDef(_) => todo!(),
            AstNode::FuncDef(_) => todo!(),

            AstNode::If(_) => todo!(),
            AstNode::Comment(_) => todo!(),
            AstNode::Assign(_) => todo!(),

            AstNode::Int(_) => Ok(Some(TypingContext::Int)),
            AstNode::Str(_) => Ok(Some(TypingContext::Str)),
            AstNode::Bool(_) => Ok(Some(TypingContext::Bool)),
            AstNode::Float(_) => Ok(Some(TypingContext::Float)),
            AstNode::None(_) => Ok(Some(TypingContext::None)),

            AstNode::Tuple(_) => todo!(),

            AstNode::Name(name) => {
                let sref = name.clone().unwrap_name();
                let ty = cx.names.get(&sref.group()).unwrap().clone();

                Ok(Some(ty))
            },

            AstNode::BinOp(_) => todo!(),
            AstNode::IfExpr(_) => todo!(),
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

                if cx.expected_return_value != ret_t {
                    todo!("mismatched return value {:?} != {:?}", ret_t, cx.expected_return_value);
                }

                Ok(None)
            }

            AstNode::Pass => todo!(),
        }
    }
}
