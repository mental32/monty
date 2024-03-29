use montyc_core::Span;
use montyc_core::{codegen::CgInst, patma, Function, Property, TaggedValueId, TypeId, ValueId};
use montyc_hlirt::object::AnyFunc;
use montyc_parser::ast::{AstNode, AstObject, Constant};

use super::*;

impl Queries for SessionContext {
    fn tcx<'a>(&'a self) -> &'a dyn montyc_core::TypingContext {
        &self.typing_context
    }

    fn get_type_of(&self, val: ValueId) -> MontyResult<TypeId> {
        match self.value_store.with_metadata(val, |m| m.type_id) {
            Some(Some(ty)) => Ok(ty),
            Some(None) => {
                let ty = self.compute_type_of(&*Rc::clone(&self.const_runtime).borrow(), val)?;

                tracing::debug!(
                    "[<SessionContext as Queries>::get_type_of] computed type of {:?} = {:?}",
                    val,
                    ty
                );

                if ty == TypingConstants::Type {
                    let object_id = self
                        .value_store
                        .with_metadata(val, |m| m.object_id)
                        .unwrap()
                        .unwrap();

                    let ty = Type::new_class(object_id, None);

                    for (_, (k, _)) in self
                        .const_runtime
                        .borrow()
                        .objects
                        .with_object(object_id, |val| match val {
                            PyValue::Class { inner, .. } => inner.__dict__.clone(),
                            _ => unimplemented!(),
                        })
                        .iter()
                    {
                        let _attr_name = self
                            .const_runtime
                            .borrow()
                            .objects
                            .with_object(*k, |m| m.as_str().unwrap().to_string());

                        let (type_id, property_value) = todo!();

                        let property = Property::new(type_id, property_value);

                        ty.properties.insert(_attr_name, property);
                    }

                    return Ok(self.typing_context.insert(ty));
                }

                self.value_store.with_metadata_mut(val, |m| {
                    m.type_id.replace(ty);
                });

                Ok(ty)
            }

            None => Err(MontyError::None),
        }
    }

    fn get_layout_of(&self, val: ValueId) -> MontyResult<std::alloc::Layout> {
        let type_id = self.get_type_of(val)?;
        let ty = self
            .tcx()
            .get_python_type_of(type_id)
            .ok_or(MontyError::None)?;

        let layout = match dbg!(ty) {
            PythonType::Builtin {
                inner: BuiltinType::Type,
            } => self
                .tcx()
                .layout_of(self.tcx().tuple(vec![TypingConstants::I64])),

            _ => self.tcx().layout_of(type_id),
        };

        Ok(layout)
    }

    fn get_module_data(&self, mref: ModuleRef) -> MontyResult<ModuleData> {
        let modules = self.modules.lock();

        Ok(modules.get(mref).unwrap().as_ref().clone())
    }

    fn get_module_flatcode(&self, mref: ModuleRef) -> MontyResult<montyc_flatcode::FlatCode> {
        fn ast_to_flatcode<T: AstObject>(mref: ModuleRef, ast: &T, span: Span) -> FlatCode {
            let mut code = montyc_flatcode::FlatCode::new((mref.clone(), span.clone()));

            ast.visit_with(&mut code, Some(span));

            code
        }

        match self.value_store.contains(mref) {
            true => self
                .value_store
                .with_metadata(mref, |metadata| {
                    if let Some((_, Some(code))) = &metadata.flatcode {
                        Ok(code.as_ref().clone())
                    } else if let Some(ast) = &metadata.ast {
                        let code = ast_to_flatcode(mref, ast, 0..0);

                        Ok(code)
                    } else {
                        panic!("module_flatcode Mref has no associated ast/flatcode metadata.")
                    }
                })
                .unwrap(),

            false => {
                let ast = self.module_asts.get(&mref).unwrap();
                let code = ast_to_flatcode(mref, ast.as_ref(), ast.span.clone());

                Ok(code)
            }
        }
    }

    fn get_function_flatcode(
        &self,
        fid: TaggedValueId<{ montyc_core::FUNCTION }>,
    ) -> MontyResult<montyc_flatcode::FlatSeq> {
        let (seq, _) = self
            .value_store
            .with_metadata(fid.0, |m| m.flatcode.clone())
            .ok_or(MontyError::ValueDoesNotExist(fid.0))?
            .ok_or(MontyError::None)?;

        Ok(seq)
    }

    fn get_function_cg_cfg(
        &self,
        fid: TaggedValueId<FUNCTION>,
    ) -> MontyResult<montyc_core::codegen::CgBlockCFG<Constant>> {
        match self
            .value_store
            .with_metadata(fid.0, |m| m.cg_flowgraph.clone())
            .unwrap()
        {
            Some(cg_flowgraph) => Ok(cg_flowgraph),
            None => {
                let _ = self.get_function(fid.0)?;
                let cg_flowgraph = self
                    .value_store
                    .with_metadata(fid.0, |m| {
                        let mut function = m.function.clone().unwrap();
                        crate::typeck::typecheck(self, &mut function)
                    })
                    .unwrap()?;

                self.value_store.with_metadata_mut(fid.0, |m| {
                    m.cg_flowgraph.replace(cg_flowgraph.clone());

                    let refs = match m.internal_refs.as_mut() {
                        Some(refs) => refs,
                        None => {
                            m.internal_refs.replace(vec![]);
                            m.internal_refs.as_mut().unwrap()
                        }
                    };

                    for value in cg_flowgraph.raw_nodes().iter().flat_map(|n| {
                        n.weight.iter().filter_map(|inst| match inst {
                            CgInst::Use { value, .. } | CgInst::Call { value, .. } => Some(*value),
                            _ => None,
                        })
                    }) {
                        if !refs.contains(&value) {
                            tracing::trace!("[<SessionContext as Queries>::get_function_cg_cfg] Adding internal ref {:?}", value);
                            refs.push(value);
                        }
                    }
                });

                Ok(cg_flowgraph)
            }
        }
    }

    fn get_function(&self, value_id: ValueId) -> MontyResult<montyc_core::Function> {
        let (func, obj) = self
            .value_store
            .with_metadata(value_id, |m| (m.function.clone(), m.object_id.clone()))
            .ok_or(MontyError::ValueDoesNotExist(value_id))?;

        if let Some(func) = func {
            return Ok(func);
        }

        let fn_obj = obj.unwrap();
        let rt_ref = self.const_runtime.borrow();

        let (fn_body, fn_dict) = rt_ref.objects.with_object(fn_obj, |val| match val {
            PyValue::Function { body, inner, .. } => (body.clone(), inner.__dict__.clone()),
            _ => unreachable!(),
        });

        let func = match fn_body {
            AnyFunc::Code { module, seq_id } => {
                let flatcode = &module.sequences()[seq_id];

                let name = patma!(def, AstNode::FuncDef(ref def) in flatcode.ast.as_ref().unwrap())
                    .unwrap()
                    .name
                    .inner
                    .as_name()
                    .unwrap();

                let type_id = self.get_type_of(value_id)?;

                self.value_store.with_metadata_mut(value_id, |m| {
                    m.flatcode
                        .replace((flatcode.clone(), Some(Rc::clone(&module))))
                });

                let extern_slot_hash = rt_ref.hash("__extern__");
                let is_extern = fn_dict.get(extern_slot_hash).is_some();

                Function {
                    value_id: TaggedValueId::func(value_id),
                    type_id,

                    mref: module.mref,
                    name,
                    is_extern,
                }
            }

            _ => panic!("get_function on function with native body."),
        };

        self.value_store
            .with_metadata_mut(value_id, |m| m.function.replace(func.clone()));

        Ok(func)
    }

    fn spanref_to_str(&self, sref: SpanRef) -> MontyResult<String> {
        self.resolve_sref_as_str(sref)
            .ok_or_else(|| MontyError::None)
    }

    fn str_to_spanref(&self, st: &str) -> SpanRef {
        self.spanner.str_to_spanref::<0>(st).unwrap()
    }

    /// From a given `entry` path, recursively lower all used functions into HLIR code.
    #[inline]
    fn call_graph_of(
        &self,
        entry_path: &str,
    ) -> MontyResult<(Vec<TaggedValueId<{ FUNCTION }>>, ValueId)> {
        let SessionOpts { input, .. } = &self.opts;

        if !self
            .modules
            .lock()
            .iter()
            .any(|(_, data)| data.path == *input)
        {
            self.include_module(input, "__main__")?;
        }

        let entry_func = self.get_func_from_path(entry_path)?;
        let entry_func_t = self.get_type_of(entry_func.0)?;

        let tcx = self.tcx();

        {
            match tcx.get_python_type_of(entry_func_t).unwrap() {
                PythonType::Callable { params: args, ret } => match (args, ret.clone()) {
                    (Some(_), _) => todo!("main function can not accept arguments."),
                    (None, TypingConstants::Int) | (None, TypingConstants::None) => (),
                    (None, actual) => todo!(
                        "main must return either None or int. {}",
                        tcx.display_type(actual, &|v| self.value_store.type_id_of(v))
                            .unwrap_or_else(|| String::from("<unknown>"))
                    ),
                },

                t => unimplemented!("{:?}", t),
            }
        }

        let mut collected_functions = MapT::with_capacity(1024);
        let mut functions_to_process = vec![entry_func];

        loop {
            let func_ix = match functions_to_process.pop() {
                Some(func_ix) => func_ix,
                None => break,
            };

            let func_type_id = self.get_type_of(func_ix.0)?;

            if let Some(PythonType::Builtin {
                inner: BuiltinType::UntypedFunc,
            }) = tcx.get_python_type_of(func_type_id)
            {
                panic!("untyped function in call graph {:#?}", func_type_id);
            }

            let _ = self.get_function_cg_cfg(func_ix)?;
            let refs = self
                .value_store
                .with_metadata(func_ix.0, |m| m.internal_refs.clone())
                .unwrap()
                .unwrap_or_default();

            for value_ref in refs {
                let value_t = self.get_type_of(value_ref)?;

                match self.tcx().get_python_type_of(value_t).unwrap() {
                    PythonType::Callable { .. } => (),

                    PythonType::Builtin { inner } => match inner {
                        BuiltinType::UntypedFunc => {
                            panic!("untyped function in call graph");
                        }

                        _ => continue,
                    },

                    _ => continue,
                }

                tracing::debug!("adding function value {:?}", value_ref);

                let ix = TaggedValueId(value_ref);

                if !collected_functions.contains_key(&ix) {
                    functions_to_process.push(ix);
                }
            }

            collected_functions.insert(func_ix, func_type_id);
        }

        collected_functions.insert(entry_func, self.get_type_of(entry_func.0)?);
        collected_functions.shrink_to_fit();

        let funcs = collected_functions.into_iter().map(|(id, _)| id).collect();

        Ok((funcs, entry_func.0))
    }
}
