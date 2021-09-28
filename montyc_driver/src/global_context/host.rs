use super::*;

impl RuntimeHost for &SessionContext {
    fn spanref_to_str(&self, sref: SpanRef) -> &str {
        self.resolve_sref_as_str(sref).unwrap()
    }

    fn spangroup_to_str(&self, group: u32) -> &str {
        for sref in self.spanner.spanrefs_of_group(group).unwrap() {
            if let Some(st) = self.resolve_sref_as_str(sref) {
                return st;
            }
        }

        unreachable!()
    }

    fn spangroup_of_hash(&mut self, hash: u64, st: &str) -> u32 {
        if let Some(group) = self.spanner.spangroup_of_hash(hash) {
            return group;
        }

        self.spanner
            .get(st, ModuleRef(0))
            .unwrap()
            .insert(0..st.len())
            .group()
    }

    fn try_read_file(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn try_read_directory(&self, path: &Path) -> io::Result<Box<[io::Result<std::fs::DirEntry>]>> {
        path.read_dir().map(|dir| dir.collect())
    }

    fn try_get_cwd(&self) -> io::Result<PathBuf> {
        std::env::current_dir()
    }
}

impl RuntimeHostExt for &SessionContext {
    type Error = montyc_core::MontyError;

    fn as_accept_input<'a, 'b, 'c>(
        &'a mut self,
    ) -> &'b mut dyn AcceptInput<&'c str, FlatCode, Error = Self::Error>
    where
        'a: 'b,
        'b: 'c,
    {
        self
    }

    fn import_module_spec(
        &mut self,
        _mref: ModuleRef,
        path: &[SpanRef],
        relative: usize,
    ) -> Box<
        dyn FnOnce(
            &mut montyc_hlirt::ctx::EvaluationContext<'_, '_, Self>,
        ) -> montyc_hlirt::PyResult<montyc_hlirt::ObjectId>,
    >
    where
        Self: Sized,
    {
        let name = path
            .iter()
            .map(|sr| self.resolve_sref_as_str(*sr).unwrap())
            .collect::<Vec<_>>()
            .join(".");

        if relative != 0 {
            match path {
                // ignore the relative part of an import that looks at __monty.
                [head, ..] if self.static_names[&head.group()] == "__monty" => (),

                _ => {
                    return Box::new(move |_| {
                        PyException::import_error()
                            .set_message("relative imports are not supported.")
                            .into()
                    });
                }
            }
        }

        if name == "__monty" {
            return Box::new(
                |ecx: &mut montyc_hlirt::ctx::EvaluationContext<'_, '_, Self>| {
                    Ok(ecx.rt.singletons.monty)
                },
            );
        }

        fn find_spec(
            ecx: &mut montyc_hlirt::ctx::EvaluationContext<&SessionContext>,
            name: String,
        ) -> PyResult<Option<ObjectId>> {
            let sys = ecx.rt.singletons.sys;
            let none_v = ecx.rt.singletons.none_v;

            let sys_dict = ecx.rt.objects.with_object(sys, |sys| match sys {
                PyValue::Module { inner, .. } => inner.__dict__.clone(),
                _ => unreachable!(),
            });

            let sys_meta_path = sys_dict.get(ecx.rt.hash("meta_path")).unwrap().1;
            let mut meta_path = ecx.iter_object(sys_meta_path);

            let find_spec = ecx.new_string("find_spec").trace()?;
            let name_s = ecx.new_string(&name).trace()?;

            while let Some(elem) = meta_path.next(ecx) {
                let elem = elem.trace()?;

                let spec = ecx
                    .call_method_object(elem, find_spec, &[name_s, none_v, none_v])
                    .trace()?;

                if spec == none_v {
                    continue;
                }

                return Ok(Some(spec));
            }

            return Ok(None);
        }

        fn gcd_import<S>(
            name: S,
        ) -> impl FnOnce(&mut montyc_hlirt::ctx::EvaluationContext<&SessionContext>) -> PyResult<ObjectId>
        where
            S: ToString,
        {
            let name = name.to_string();

            move |ecx| {
                let sys = ecx.rt.singletons.sys;

                let sys_dict = ecx.rt.objects.with_object(sys, |sys| match sys {
                    PyValue::Module { inner, .. } => inner.__dict__.clone(),
                    _ => unreachable!(),
                });

                let sys_modules = sys_dict.get(ecx.rt.hash("modules")).unwrap().1;

                match name.rsplit_once(".") {
                    Some((parent, _child)) => {
                        // import the parent module first.

                        let parent_in_sys_modules = {
                            let rt = ecx.runtime();
                            let parent_h = rt.hash(parent);

                            rt.objects
                                .with_object(sys_modules, |this| match this {
                                    PyValue::Dict(dct) => dct.get(parent_h),
                                    _ => unreachable!(),
                                })
                                .map(|kv| kv.1)
                        };

                        let _parent_module = match parent_in_sys_modules {
                            Some(module) => module,
                            None => gcd_import(parent)(ecx).trace()?,
                        };

                        {
                            let rt = ecx.runtime();
                            let name_h = rt.hash(name);

                            if let Some(module) = rt
                                .objects
                                .with_object(sys_modules, |this| match this {
                                    PyValue::Dict(dct) => dct.get(name_h),
                                    _ => None,
                                })
                                .map(|kv| kv.1)
                            {
                                return Ok(module);
                            }
                        }

                        todo!()
                    }

                    None => {
                        // no dots, no parent module.
                        match find_spec(ecx, name).trace()? {
                            Some(spec) => Ok(spec),
                            None => {
                                return PyException::import_error()
                                    .set_message("Module not found.")
                                    .into()
                            }
                        }
                    }
                }
            }
        }

        Box::new(gcd_import(name))
    }
}

impl AcceptInput<&str, FlatCode> for &SessionContext {
    fn accept_input(&mut self, input: &str) -> Result<FlatCode, Self::Error> {
        let mut modules = self.modules.lock();
        let mref = (modules.reserve() as u32).into();

        let module_ast = montyc_parser::parse(
            &input,
            montyc_parser::comb::module,
            Some(self.spanner.clone()),
            mref,
        );

        let _ = self
            .module_sources
            .insert(mref, input.to_string().into_boxed_str());

        let _ = self.module_asts.insert(mref, Rc::new(module_ast));

        self.accept_input(mref)
    }
}

impl AcceptInput<ModuleRef, FlatCode> for &SessionContext {
    fn accept_input(&mut self, input: ModuleRef) -> Result<FlatCode, Self::Error> {
        Queries::get_module_flatcode(*self, input)
    }
}
