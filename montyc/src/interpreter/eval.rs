use std::rc::Rc;

use crate::{
    ast::{
        assign::Assign, atom::Atom, expr::Expr, module::Module, primary::Primary, stmt::Statement,
        Spanned,
    },
    exception,
    scope::{Scope, ScopeRoot},
};

use super::{object::Object, runtime::RuntimeContext, Eval, PyObject, PyResult, ToAst};

impl Eval for Atom {
    fn eval(&self, rt: &mut RuntimeContext, _module: &mut Module) -> PyResult<Option<PyObject>> {
        match self {
            Atom::None => Ok(Some(rt.none())),
            Atom::Ellipsis => todo!(),
            Atom::Int(n) => Ok(Some(rt.integer(*n))),
            Atom::Str(s) => Ok(Some(rt.string(*s, rt.scope().module_ref()))),
            Atom::Bool(b) => Ok(Some(rt.boolean(*b))),
            Atom::Float(_) => todo!(),
            Atom::Tuple(_) => todo!(),
            Atom::Comment(_) => todo!(),
            Atom::Name(name) => match rt.lookup(name.clone()) {
                obj @ Some(_) => Ok(obj),
                None => exception!("Not found"),
            },
        }
    }
}

impl Eval for Primary {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>> {
        match self {
            Primary::Atomic(at) => at.inner.eval(rt, module),
            Primary::Subscript { value: _, index: _ } => todo!(),

            Primary::Call { func, args } => {
                let f = func.inner.eval(rt, module)?.unwrap();

                let args = match args {
                    Some(args) => {
                        let mut r = vec![];

                        for arg_v in args.iter() {
                            let arg_v = arg_v.inner.eval(rt, module)?.unwrap();
                            r.push(arg_v);
                        }

                        Some(rt.tuple(r.as_slice()))
                    }

                    None => None,
                };

                f.call(rt, args, None).map(Some)
            }

            Primary::Attribute { left: _, attr: _ } => todo!(),
            Primary::Await(_) => todo!(),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>> {
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
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>> {
        match &self.inner {
            Statement::Expression(e) => e.eval(rt, module),

            Statement::FnDef(fndef) => {
                rt.scope().define(
                    fndef.name.inner.as_name().unwrap(),
                    &rt.function(self.clone(), rt.scope().module_ref()),
                );

                if !fndef.is_dynamically_typed() {
                    module.body.push(self.clone());
                }

                Ok(None)
            }

            Statement::Ret(ret) => {
                let value = match &ret.value {
                    Some(e) => e.inner.eval(rt, module)?.unwrap(),
                    None => rt.none(),
                };

                return Err(rt.return_exc(value));
            }

            Statement::Asn(asn) => {
                let val = asn
                    .value
                    .inner
                    .eval(rt, module)?
                    .expect("Assignment value must produce an object.");

                rt.scope().define(asn.name.inner.as_name().unwrap(), &val);

                if matches!(rt.scope().root.clone(), ScopeRoot::AstObject(object) if object.as_ref().downcast_ref::<Module>().is_some())
                {
                    let assign = Spanned {
                        span: self.span.clone(),
                        inner: Statement::Asn(Assign {
                            name: asn.name.clone(),
                            value: Rc::new(Spanned {
                                span: self.span.clone(),
                                inner: val.to_expr(rt, self.span.clone()),
                            }),
                            kind: asn.kind.clone(),
                        }),
                    };

                    module.body.push(Rc::new(assign));
                }

                Ok(None)
            }

            Statement::Import(_) => todo!(),
            Statement::Class(_) => todo!(),

            Statement::If(ifstmt) => {
                for branch in ifstmt.branches.iter() {
                    let v = branch.inner.test.inner.eval(rt, module)?.unwrap();

                    if rt.is_truthy(v) {
                        for stmt in branch.inner.body.iter() {
                            stmt.eval(rt, module)?;
                        }

                        return Ok(None);
                    }
                }

                if let Some(orelse) = &ifstmt.orelse {
                    for stmt in orelse.iter() {
                        stmt.eval(rt, module)?;
                    }
                }

                Ok(None)
            }

            Statement::While(_) => todo!(),
            Statement::Pass => Ok(None),
        }
    }
}
