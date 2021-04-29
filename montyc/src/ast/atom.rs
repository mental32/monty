use std::rc::Rc;

use crate::{context::codegen::CodegenLowerArg, prelude::*, scope::LookupOrder};

use super::{assign::Assign, stmt::Statement, AstObject};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Atom {
    None,
    Ellipsis,
    Int(isize),
    Str(SpanEntry),
    Bool(bool),
    Float(f64),
    Comment(SpanEntry),
    Name(SpanEntry),
}

impl Parseable for Atom {
    const PARSER: ParserT<Self> = crate::parser::comb::atom_unspanned;
}

impl From<PyToken> for Atom {
    fn from(value: PyToken) -> Self {
        match value {
            PyToken::Ellipsis => Self::Ellipsis,
            PyToken::None => Self::None,
            PyToken::True => Self::Bool(true),
            PyToken::False => Self::Bool(false),
            PyToken::Digits(n) => Self::Int(n),
            PyToken::CommentRef(n) => Self::Comment(n),
            PyToken::StringRef(n) => Self::Str(n),
            PyToken::Ident(n) => Self::Name(n),
            _ => unreachable!("{:?}", value),
        }
    }
}

impl AstObject for Atom {
    fn span(&self) -> Option<logos::Span> {
        None
    }

    fn walk(&self) -> Option<super::ObjectIter> {
        None
    }

    fn unspanned(&self) -> Rc<dyn AstObject> {
        Rc::new(self.clone())
    }
}

impl TypedObject for Atom {
    fn infer_type<'a>(&self, ctx: &LocalContext<'_>) -> crate::Result<LocalTypeId> {
        match self {
            Atom::None => Ok(TypeMap::NONE_TYPE),
            Atom::Ellipsis => Ok(TypeMap::ELLIPSIS),
            Atom::Int(_) => Ok(TypeMap::INTEGER),
            Atom::Str(_) => Ok(TypeMap::STRING),
            Atom::Bool(_) => Ok(TypeMap::BOOL),
            Atom::Float(_) => Ok(TypeMap::FLOAT),
            Atom::Comment(_) => todo!(),
            Atom::Name(None) => unreachable!(),
            Atom::Name(target) => {
                log::trace!("infer_type: performing name lookup on atom {:?}", self);

                let results = ctx
                    .scope
                    .lookup_def(
                        target.clone(),
                        &ctx.global_context,
                        LookupOrder::ControlFlowSensitive(ctx.this.clone().unwrap()),
                    )
                    .unwrap_or_compiler_error(ctx);

                if results.is_empty() {
                    ctx.exit_with_error(MontyError::UndefinedVariable {
                        node: ctx.this.clone().unwrap(),
                    });
                }

                for top in results {
                    if let Some(asn) = crate::isinstance!(top.as_ref(), Assign).or_else(
                        || crate::isinstance!(top.as_ref(), Statement, Statement::Asn(n) => n),
                    ) {
                        match asn.value.inner.infer_type(ctx) {
                            Ok(i) => return Ok(i),
                            Err(err) => ctx.exit_with_error(err),
                        }
                    } else {
                        log::trace!("infer_type: lookup successfull! top={:?}", top.name());
                        let mut ctx = ctx.clone();
                        ctx.this = Some(top.clone());

                        match top.infer_type(&ctx) {
                            Err(err) => ctx.exit_with_error(err),
                            Ok(i) => return Ok(i),
                        }
                    }
                }

                ctx.exit_with_error(MontyError::UndefinedVariable {
                    node: ctx.this.clone().unwrap(),
                });
            }
        }
    }

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()> {
        log::trace!("typecheck: {:?}", self);

        if let Self::Name(_target) = self {
            let _ = self.infer_type(ctx)?;
        } else {
            log::trace!("Skipping typecheck: {:?}", self);
        }

        Ok(())
    }
}

impl LookupTarget for Atom {
    fn is_named(&self, target: SpanEntry) -> bool {
        let target = match target {
            Some(n) => n,
            None => return false,
        };

        match self {
            Self::Name(Some(n)) => *n == target,
            _ => false,
        }
    }

    fn name(&self) -> SpanEntry {
        match self {
            Self::Name(n) => n.clone(),
            _ => None,
        }
    }
}

impl<'a, 'b> LowerWith<CodegenLowerArg<'a, 'b>, cranelift_codegen::ir::Value> for Atom {
    fn lower_with(&self, ctx: CodegenLowerArg<'a, 'b>) -> cranelift_codegen::ir::Value {
        use cranelift_codegen::ir::InstBuilder;

        match self {
            Atom::None => todo!(),
            Atom::Ellipsis => todo!(),
            Atom::Int(n) => ctx.builder.borrow_mut().ins().iconst(
                cranelift_codegen::ir::types::I64,
                cranelift_codegen::ir::immediates::Imm64::new(
                    *n as i64,
                ),
            ),
            Atom::Str(_) => todo!(),
            Atom::Bool(b) => {
                ctx.builder.borrow_mut().ins().iconst(ctx.codegen_backend.types[&TypeMap::INTEGER], *b as i64)
            },

            Atom::Float(_) => todo!(),
            Atom::Comment(_) => todo!(),
            Atom::Name(n) => {
                let ss = ctx.vars.get(&n.unwrap()).unwrap();
                let ty = ctx.func
                    .vars
                    .get(n)
                    .map(|r| r.value().0)
                    .unwrap();

                let ty = ctx.codegen_backend.types[&ty];

                ctx.builder.borrow_mut().ins().stack_load(ty, ss.clone(), 0)
            }
        }
    }
}
