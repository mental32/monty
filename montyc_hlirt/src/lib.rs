//! `montyc_hlirt` is the (H)igh (L)evel (I)nterpreter (R)untime for Monty.
#![deny(warnings)]

#[cfg(test)]
pub(crate) mod test {
    use ahash::AHashMap;
    use montyc_core::ModuleRef;
    use montyc_flatcode::FlatCode;
    use montyc_parser::{AstObject, SpanInterner};

    use crate::{
        eval::ctx::EvalGlue,
        exception::PyResult,
        object::PyIter,
        rt::{AcceptInput, Runtime, RuntimeHost, RuntimeHostExt},
        ObjectId,
    };

    #[derive(Debug, Default)]
    pub struct TestHost {
        pub sources: AHashMap<ModuleRef, String>,
        pub sint: SpanInterner,
        pub mrefs: u32,
        pub objs: u64,
    }

    impl RuntimeHostExt for TestHost {
        type Error = ();

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
            path: &[montyc_core::SpanRef],
            relative: usize,
        ) -> Box<
            dyn FnOnce(
                &mut crate::eval::ctx::EvaluationContext<'_, '_, Self>,
            ) -> PyResult<ObjectId>,
        >
        where
            Self: Sized,
        {
            let path = path.to_owned();

            Box::new(move |ecx| crate::import::bootstrap::import_module(ecx, &*path, relative))
        }
    }

    impl RuntimeHost for TestHost {
        fn spanref_to_str(&self, sref: montyc_core::SpanRef) -> &str {
            self.sint
                .spanref_to_str(sref, |mref, range| {
                    self.sources.get(&mref).and_then(|st| st.get(range))
                })
                .unwrap()
        }

        fn spangroup_to_str(&self, group: u32) -> &str {
            let sref = self.sint.spanrefs_of_group(group).unwrap().next().unwrap();
            self.spanref_to_str(sref)
        }

        fn spangroup_of_hash(&mut self, hash: u64, st: &str) -> u32 {
            match self.sint.spangroup_of_hash(hash) {
                Some(n) => n,
                None => {
                    let mref = self.mrefs.into();
                    self.mrefs += 1;

                    let mut sint = self.sint.get(st, mref).unwrap();

                    let sref = sint.insert(0..st.len());

                    sref.group()
                }
            }
        }

        fn try_read_file(&self, path: &std::path::Path) -> std::io::Result<String> {
            std::fs::read_to_string(path)
        }

        fn try_read_directory(
            &self,
            path: &std::path::Path,
        ) -> std::io::Result<Box<[std::io::Result<std::fs::DirEntry>]>> {
            Ok(path.read_dir()?.collect::<Vec<_>>().into_boxed_slice())
        }

        fn try_get_cwd(&self) -> std::io::Result<std::path::PathBuf> {
            std::env::current_dir()
        }
    }

    impl AcceptInput<&str, FlatCode> for TestHost {
        fn accept_input(&mut self, input: &str) -> Result<montyc_flatcode::FlatCode, Self::Error> {
            let mref = self.mrefs.into();
            let module = montyc_parser::parse(
                input,
                montyc_parser::comb::module,
                Some(self.sint.clone()),
                mref,
            );

            self.mrefs += 1;
            self.sources.insert(mref, input.to_string());

            let mut code = FlatCode::new((mref, module.span().unwrap_or(0..0)));

            module.visit_with(&mut code, None);

            Ok(code)
        }
    }

    impl EvalGlue for Runtime {
        fn runtime_mut<'a>(&'a mut self) -> &'a mut Runtime {
            self
        }

        fn self_as_dyn<'a>(&'a mut self) -> &'a mut dyn EvalGlue {
            self
        }

        fn call_object(
            &mut self,
            _callable: ObjectId,
            _arguments: &[ObjectId],
        ) -> PyResult<ObjectId> {
            unimplemented!()
        }

        fn hash_object(&mut self, _object: ObjectId) -> PyResult<u64> {
            unimplemented!()
        }

        fn iter_object(&mut self, _object: ObjectId) -> PyIter {
            unimplemented!()
        }

        fn call_method_object(
            &mut self,
            _object: ObjectId,
            _method: ObjectId,
            _args: &[ObjectId],
        ) -> PyResult<ObjectId> {
            unimplemented!()
        }

        fn repr_object(&mut self, _object: ObjectId) -> PyResult<ObjectId> {
            unimplemented!()
        }

        fn getattr_object(&mut self, _object: ObjectId, _attr: ObjectId) -> PyResult<ObjectId> {
            unimplemented!()
        }

        fn runtime_host_mut<'a>(&'a mut self) -> &'a mut dyn RuntimeHost {
            unimplemented!()
        }

        fn runtime<'a>(&'a self) -> &'a Runtime {
            self
        }

        fn runtime_host<'a>(&'a self) -> &'a dyn RuntimeHost {
            unimplemented!()
        }
    }

    pub fn setup() -> (Runtime, TestHost) {
        let _ = env_logger::try_init();

        let rt = Runtime::new_uninit();
        let host = TestHost::default();

        (rt, host)
    }
}

pub(crate) mod eval;
pub(crate) mod exception;
pub(crate) mod storage;

pub mod argparse;
pub mod import;
pub mod object;
pub mod rt;

pub use {
    eval::ctx,
    exception::{PyException, PyResult, PyResultExt},
    object::ObjectId,
    storage::ObjectSpace,
};
