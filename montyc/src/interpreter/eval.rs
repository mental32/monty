use std::rc::Rc;

use crate::{
    ast::{atom::Atom, expr::Expr, module::Module, primary::Primary, stmt::Statement, Spanned},
    scope::Scope,
};

use super::{object::Object, runtime::RuntimeContext, Eval};

impl Eval for Atom {
    fn eval(&self, rt: &mut RuntimeContext, _module: &mut Module) -> Option<Rc<Object>> {
        match self {
            Atom::None => Some(rt.none()),
            Atom::Ellipsis => todo!(),
            Atom::Int(n) => Some(rt.integer(*n)),
            Atom::Str(s) => Some(rt.string(*s, rt.scope().module_ref())),
            Atom::Bool(b) => Some(rt.boolean(*b)),
            Atom::Float(_) => todo!(),
            Atom::Tuple(_) => todo!(),
            Atom::Comment(_) => todo!(),
            Atom::Name(_) => todo!(),
        }
    }
}

impl Eval for Primary {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> Option<Rc<Object>> {
        match self {
            Primary::Atomic(at) => at.inner.eval(rt, module),
            Primary::Subscript { value: _, index: _ } => todo!(),
            Primary::Call { func: _, args: _ } => todo!(),
            Primary::Attribute { left: _, attr: _ } => todo!(),
            Primary::Await(_) => todo!(),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> Option<Rc<Object>> {
        match self {
            Expr::If {
                test: _,
                body: _,
                orelse: _,
            } => todo!(),
            Expr::BinOp {
                left: _,
                op: _,
                right: _,
            } => todo!(),
            Expr::Unary { op: _, value: _ } => todo!(),
            Expr::Named {
                target: _,
                value: _,
            } => todo!(),
            Expr::Primary(p) => p.inner.eval(rt, module),
        }
    }
}

impl Eval for Rc<Spanned<Statement>> {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> Option<Rc<Object>> {
        match &self.inner {
            Statement::Expression(e) => e.eval(rt, module),

            Statement::FnDef(fndef) => {
                rt.scope().define(
                    fndef.name.inner.as_name().unwrap(),
                    &rt.function(self.clone()),
                );

                if !fndef.is_dynamically_typed() {
                    module.body.push(self.clone());
                }

                None
            }

            Statement::Ret(_) => todo!(),

            Statement::Asn(asn) => {
                rt.scope().define(
                    asn.name.inner.as_name().unwrap(),
                    &asn.value.inner.eval(rt, module).unwrap(),
                );

                module.body.push(self.clone());

                None
            }

            Statement::Import(_) => todo!(),
            Statement::Class(_) => todo!(),

            Statement::If(ifstmt) => {
                for branch in ifstmt.branches.iter() {
                    let v = branch.inner.test.inner.eval(rt, module).unwrap();

                    if rt.is_truthy(v) {
                        for stmt in branch.inner.body.iter() {
                            stmt.eval(rt, module);
                        }

                        return None;
                    }
                }

                if let Some(orelse) = &ifstmt.orelse {
                    for stmt in orelse.iter() {
                        stmt.eval(rt, module);
                    }
                }

                None
            }

            Statement::While(_) => todo!(),
            Statement::Pass => None,
        }
    }
}
