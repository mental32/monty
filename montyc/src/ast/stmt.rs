use std::rc::Rc;

use super::{AstObject, assign::Assign, class::ClassDef, expr::Expr, funcdef::FunctionDef, ifelif::IfChain, import::Import, retrn::Return, while_::While};

use crate::{
    context::codegen::CodegenLowerArg, parser::comb::stmt::statement_unspanned, prelude::*,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expr),
    FnDef(FunctionDef),
    Ret(Return),
    Asn(Assign),
    Import(Import),
    Class(ClassDef),
    If(IfChain),
    While(While),
    Pass,
}

impl Statement {
    pub fn inner_as_object(&self) -> Rc<dyn AstObject> {
        match self.clone() {
            Statement::Expression(e) => Rc::new(e),
            Statement::FnDef(f) => Rc::new(f),
            Statement::Ret(r) => Rc::new(r),
            Statement::Asn(a) => Rc::new(a),
            Statement::Import(i) => Rc::new(i),
            Statement::Class(c) => Rc::new(c),
            Statement::If(f) => Rc::new(f),
            Statement::While(w) => Rc::new(w),
            Statement::Pass => todo!(),
        }
    }
}

impl AstObject for Statement {
    fn span(&self) -> Option<logos::Span> {
        match self {
            Statement::Expression(e) => e.span(),
            Statement::FnDef(f) => f.span(),
            Statement::Ret(r) => r.span(),
            Statement::Asn(a) => a.span(),
            Statement::Import(i) => i.span(),
            Statement::Class(c) => c.span(),
            Statement::If(f) => f.span(),
            Statement::While(w) => w.span(),
            Statement::Pass => None,
        }
    }

    fn unspanned(&self) -> std::rc::Rc<dyn AstObject> {
        match self {
            Statement::Expression(e) => e.unspanned(),
            Statement::FnDef(f) => f.unspanned(),
            Statement::Ret(r) => r.unspanned(),
            Statement::Asn(a) => a.unspanned(),
            Statement::Import(i) => i.unspanned(),
            Statement::Class(c) => c.unspanned(),
            Statement::If(f) => f.unspanned(),
            Statement::While(w) => w.unspanned(),
            Statement::Pass => Rc::new(Self::Pass),
        }
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        match self {
            Statement::Expression(e) => e.walk(),
            Statement::FnDef(f) => f.walk(),
            Statement::Ret(r) => r.walk(),
            Statement::Asn(a) => a.walk(),
            Statement::Import(i) => i.walk(),
            Statement::Class(c) => c.walk(),
            Statement::If(f) => f.walk(),
            Statement::While(w) => w.walk(),
            Statement::Pass => None,
        }
    }
}

impl TypedObject for Statement {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId> {
        match self {
            Statement::Expression(e) => e.infer_type(ctx),
            Statement::FnDef(f) => f.infer_type(ctx),
            Statement::Ret(r) => r.infer_type(ctx),
            Statement::Asn(a) => a.infer_type(ctx),
            Statement::Import(i) => i.infer_type(ctx),
            Statement::Class(c) => c.infer_type(ctx),
            Statement::While(w) => w.infer_type(ctx),
            Statement::If(_) => Ok(TypeMap::NONE_TYPE),
            Statement::Pass => Ok(TypeMap::NONE_TYPE),
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        match self {
            Statement::Expression(e) => e.typecheck(ctx),
            Statement::FnDef(f) => f.typecheck(ctx),
            Statement::Ret(r) => r.typecheck(ctx),
            Statement::Asn(a) => a.typecheck(ctx),
            Statement::Import(i) => i.typecheck(ctx),
            Statement::Class(c) => c.typecheck(ctx),
            Statement::If(f) => f.typecheck(ctx),
            Statement::While(w) => w.typecheck(ctx),
            Statement::Pass => Ok(()),
        }
    }
}

impl Parseable for Statement {
    const PARSER: ParserT<Self> = statement_unspanned;
}

impl LookupTarget for Statement {
    fn is_named(&self, target: crate::parser::SpanEntry) -> bool {
        match self {
            Statement::Expression(e) => e.is_named(target),
            Statement::FnDef(e) => e.is_named(target),
            Statement::Ret(e) => e.is_named(target),
            Statement::Asn(e) => e.is_named(target),
            Statement::Import(e) => e.is_named(target),
            Statement::Class(e) => e.is_named(target),
            Statement::If(f) => f.is_named(target),
            Statement::While(w) => w.is_named(target),
            Statement::Pass => false,
        }
    }

    fn name(&self) -> crate::parser::SpanEntry {
        match self {
            Statement::Expression(e) => e.name(),
            Statement::FnDef(e) => e.name(),
            Statement::Ret(e) => e.name(),
            Statement::Asn(e) => e.name(),
            Statement::Import(e) => e.name(),
            Statement::Class(e) => e.name(),
            Statement::If(f) => f.name(),
            Statement::While(w) => w.name(),
            Statement::Pass => None,
        }
    }
}

impl<'a, 'b> LowerWith<CodegenLowerArg<'a, 'b>, Option<bool>> for Statement {
    fn lower_with(&self, ctx: CodegenLowerArg<'a, 'b>) -> Option<bool> {
        use cranelift_codegen::ir::InstBuilder;

        match self {
            Statement::Expression(e) => {
                let _ = e.lower_with(ctx.clone());
            }

            Statement::FnDef(_) => todo!(),

            Statement::Ret(r) => {
                if let Some(e) = &r.value {
                    let value = e.inner.lower_with(ctx.clone());

                    ctx.builder.borrow_mut().ins().return_(&[value]);
                } else {
                    ctx.builder.borrow_mut().ins().return_(&[]);
                };

                return Some(false); // this sets "implicit return" to false in codegen.
            }

            Statement::Asn(asn) => {
                let ss = ctx.vars.get(&asn.name().unwrap()).unwrap().value().clone();

                let value = asn.value.inner.lower_with(ctx.clone());

                ctx.builder.borrow_mut().ins().stack_store(value, ss, 0);
            }

            Statement::Import(_) => todo!(),
            Statement::Class(_) => todo!(),
            Statement::Pass => {
                ctx.builder.borrow_mut().ins().nop();
            }

            Statement::If(ifstmt) => {
                let global_escape_block = ctx.builder.borrow_mut().create_block();

                let branch_blocks: Vec<_> = ifstmt
                    .branches
                    .iter()
                    .map(|_| {
                        let mut builder = ctx.builder.borrow_mut();
                        (builder.create_block(), builder.create_block(), builder.create_block())  // (head, body, escape)
                    })
                    .collect();

                for (branch_blocks_idx, ifstmt) in ifstmt.branches.iter().enumerate() {
                    let (head_block, body_block, local_escape_block) = branch_blocks[branch_blocks_idx];

                    ctx.builder.borrow_mut().ins().jump(head_block, &[]);

                    ctx.builder.borrow_mut().switch_to_block(head_block);

                    let cc = ifstmt.inner.test.inner.lower_with(ctx.clone());

                    ctx.builder.borrow_mut().ins().brnz(cc, body_block, &[]);
                    ctx.builder.borrow_mut().ins().jump(local_escape_block, &[]);

                    {
                        ctx.builder.borrow_mut().switch_to_block(body_block);

                        for part in ifstmt.inner.body.iter() {
                            part.inner.lower_with(ctx.clone());
                        }

                        if !ctx.builder.borrow().is_filled() {
                            ctx.builder.borrow_mut().ins().jump(local_escape_block, &[]);
                        }
                    }

                    ctx.builder.borrow_mut().switch_to_block(local_escape_block);
                }

                ctx.builder.borrow_mut().ins().jump(global_escape_block, &[]);

                ctx.builder.borrow_mut().switch_to_block(global_escape_block);

                if let Some(orelse) = &ifstmt.orelse {
                    for stmt in orelse {
                        stmt.inner.lower_with(ctx.clone());
                    }
                }
            }

            Statement::While(w) => {
                let (head, body, escape) = {
                    let mut builder = ctx.builder.borrow_mut();

                    (builder.create_block(), builder.create_block(), builder.create_block())
                };

                ctx.builder.borrow_mut().ins().jump(head, &[]);

                ctx.builder.borrow_mut().switch_to_block(head);

                let cc = w.test.inner.lower_with(ctx.clone());

                ctx.builder.borrow_mut().ins().brnz(cc, body, &[]);
                ctx.builder.borrow_mut().ins().jump(escape, &[]);

                ctx.builder.borrow_mut().switch_to_block(body);

                let mut ret = None;

                for part in w.body.iter() {
                    ret = part.inner.lower_with(ctx.clone());
                }

                if !ctx.builder.borrow().is_filled() {
                    ctx.builder.borrow_mut().ins().jump(head, &[]);
                    ctx.builder.borrow_mut().switch_to_block(escape);
                } else {
                    ctx.builder.borrow_mut().switch_to_block(escape);
                    ctx.builder.borrow_mut().ins().jump(head, &[]);
                }

                return ret;
            },
        }

        None
    }
}
