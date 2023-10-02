use std::sync::atomic::{AtomicUsize, Ordering};

use super::*;
use crate::{
    eval::ctx::EvalGlue,
    exception::InnerExc,
    object::{PyObject, SharedObject},
    test::setup,
};

#[test]
pub fn basic() {
    let (mut rt, mut host) = setup();

    rt.eval(&mut host, "pass")
        .unwrap()
        .run_until_complete()
        .unwrap();

    rt.eval(&mut host, "1")
        .unwrap()
        .run_until_complete()
        .unwrap();

    match rt.eval(&mut host, "a").unwrap().run_until_complete() {
        Err(PyException {
            inner: InnerExc::NameError(sref),
            ..
        }) => assert_eq!(host.spanref_to_str(sref), "a"),

        val => panic!("Expected an NameError instead got {:?}", val),
    }
}

#[test]
pub fn name_error() {
    let (mut rt, mut host) = setup();

    match rt.eval(&mut host, "a").unwrap().run_until_complete() {
        Err(PyException {
            inner: InnerExc::NameError(sref),
            ..
        }) => assert_eq!(host.spanref_to_str(sref), "a"),

        val => panic!("Expected an NameError instead got {:?}", val),
    }
}

#[test]
#[should_panic(
    expected = "not yet implemented: sys module has not been initialized yet, sys.path is unavailable."
)]
pub fn import_with_uninit_sys() {
    let (mut rt, mut host) = setup();

    rt.eval(&mut host, "import abcdef")
        .unwrap()
        .run_until_complete()
        .unwrap();
}

#[test]
pub fn native_object() {
    let (mut rt, mut host) = setup();

    #[derive(Debug)]
    struct N(AtomicUsize);

    impl PyObject for N {
        fn call(&self, cx: crate::eval::ctx::CallCx) -> PyResult<ObjectId> {
            let n = self.0.fetch_add(1, Ordering::Relaxed);

            cx.ecx.new_int(i64::try_from(n).unwrap())
        }

        fn repr(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
            let st = format!("N({})", self.0.load(Ordering::Relaxed));

            ecx.new_string(&st)
        }

        unsafe fn std_type_id(&self) -> std::any::TypeId {
            std::any::TypeId::of::<N>()
        }
    }

    let module = ObjectBuilder::<{ MODULE }>::new().setattr("n", SharedObject::new(N(0.into())));

    host.mrefs += 1;

    let mref = ModuleRef(0);

    rt.synthesise_module(mref, module).unwrap();

    for _ in 0..2 {
        rt.eval(&mut host, "n()")
            .unwrap()
            .from_module_import(mref, ["n"])
            .run_until_complete()
            .unwrap();
    }

    let module = rt.module_objects.get(&mref).unwrap();
    let module = module.alloc;

    let n = rt.hash("n");
    let (_, n) = rt
        .objects
        .with_object(module, |val| match val {
            PyValue::Module { inner, .. } => inner.__dict__.get(n),
            _ => unreachable!(),
        })
        .unwrap();

    let obj = rt.objects.with_object(n, |n_ref| match n_ref {
        PyValue::Dynamic(n_rc) => n_rc.clone(),
        _ => unreachable!(),
    });

    let st = obj.repr(&mut rt).unwrap();

    rt.objects.with_object(st, |st| match st {
        PyValue::Str(st) => assert_eq!(st.as_ref(), "N(2)"),
        _ => unreachable!(),
    });
}

#[test]
#[ignore = "flaky"]
pub fn import_example_module_with_init_sys() {
    let (mut rt, mut host) = setup();

    rt.try_init(&mut host);

    let sys = rt.singletons.sys;
    let hash = rt.hash("path");

    let (_, sys_path) = rt.getattr_direct_hash(sys, hash).unwrap();

    let path = option_env!("IMPORT_TEST_DIR")
        .map(ToString::to_string)
        .unwrap_or(String::default());

    let test_dir = rt.new_string(path.as_str());

    rt.objects.with_object_mut(sys_path, |val| match val {
        PyValue::List(lst) => lst.push(test_dir),
        _ => unreachable!(),
    });

    rt.eval(&mut host, "import example_module")
        .unwrap()
        .run_until_complete()
        .unwrap();
}
