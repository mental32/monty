use std::rc::Rc;

use crate::{
    ast::{
        assign::Assign,
        atom::Atom,
        expr::{Expr, InfixOp, UnaryOp},
        module::Module,
        primary::Primary,
        stmt::Statement,
        Spanned,
    },
    exception,
    interpreter::PyErr,
    scope::{Scope, ScopeRoot},
};

use super::{object::Object, runtime::RuntimeContext, Eval, PyObject, PyResult, ToAst};

impl Eval for Atom {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>> {
        match self {
            Atom::None => Ok(Some(rt.none())),
            Atom::Ellipsis => todo!(),
            Atom::Int(n) => Ok(Some(rt.integer(*n))),
            Atom::Str(s) => Ok(Some(rt.string(*s, rt.scope().module_ref()))),
            Atom::Bool(b) => Ok(Some(rt.boolean(*b))),
            Atom::Float(f) => Ok(Some(rt.float(*f))),
            Atom::Tuple(elem) => {
                let mut objs = vec![];

                for expr in elem.iter() {
                    let value = expr.inner.eval(rt, module)?.unwrap();
                    objs.push(value);
                }

                Ok(Some(rt.tuple(&*objs)))
            }

            Atom::Comment(_) => Ok(None),
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

            Primary::Subscript { value, index } => {
                let obj = value.inner.eval(rt, module)?.unwrap();
                let index = index.inner.eval(rt, module)?.unwrap();

                obj.call_method(rt.names.__getitem__, rt, Some(index), None)
                    .map(Some)
            }

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

            Primary::Attribute { left, attr } => {
                let obj = left.inner.eval(rt, module)?.unwrap();
                let attr = attr.inner.as_name().unwrap();

                match obj.get_attribute(&attr) {
                    Some(cell) => Ok(cell.into_inner().downcast_ref::<PyObject>().cloned()),
                    None => exception!("attribute error"),
                }
            }

            Primary::Await(_) => todo!(),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>> {
        match self {
            Expr::If { test, body, orelse } => {
                let test = test.inner.eval(rt, module)?.unwrap();

                if rt.is_truthy(test) {
                    body.inner.eval(rt, module)
                } else {
                    orelse.inner.eval(rt, module)
                }
            }

            Expr::BinOp { left, op, right } => {
                let left = left.inner.eval(rt, module)?.unwrap();
                let right = right.inner.eval(rt, module)?.unwrap();

                log::trace!("interpreter:eval Performing BinaryOp: {:?}", op);

                let dunder = match op {
                    InfixOp::Eq => rt.global_context.magical_name_of("__eq__").unwrap(),
                    op => rt
                        .global_context
                        .magical_name_of(&format!("__{}__", op.as_ref()))
                        .unwrap(),
                };

                left.call_method(dunder, rt, Some(right), None).map(Some)
            }

            Expr::Unary { op, value } => {
                let name = match op {
                    UnaryOp::Invert => rt.names.__invert__,
                    UnaryOp::Not => todo!(),
                    UnaryOp::Add => rt.names.__pos__,
                    UnaryOp::Sub => rt.names.__neg__,
                };

                let value = value.inner.eval(rt, module)?.unwrap();

                value.call_method(name, rt, None, None).map(Some)
            }

            Expr::Named {
                target,
                value,
            } => {
                let value = value
                    .inner
                    .eval(rt, module)?
                    .unwrap();

                rt.scope().define(target.inner.as_name().unwrap(), &value);

                Ok(Some(value))
            },

            Expr::Primary(p) => p.inner.eval(rt, module),
        }
    }
}

impl Eval for Rc<Spanned<Statement>> {
    fn eval(&self, rt: &mut RuntimeContext, module: &mut Module) -> PyResult<Option<PyObject>> {
        match &self.inner {
            Statement::Expr(e) => e.eval(rt, module),

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

            Statement::While(while_) => {
                while {
                    let test = while_.test.inner.eval(rt, module)?.unwrap();
                    rt.is_truthy(test)
                } {
                    for stmt in while_.body.iter() {
                        if let Err(exc) = stmt.eval(rt, module) {
                            if let PyErr::Break = exc {
                                break;
                            } else {
                                return Err(exc);
                            }
                        }
                    }
                }

                Ok(None)
            }

            Statement::Pass => Ok(None),
        }
    }
}
