//! A direct re-implementation of `Lib/importlib/`s `_bootstrap.py` and `_bootstrap_external.py`.
//!
//! This implementation is probably borderline garbage, written over the course
//! of a month because i keep finding excuses not to finsih it.
//!
//! It is also only meant to ever be used directly from evalutation context code
//! and is the implementation of the `import` flatcode instruction.
//!
//! Only the `PathFinder`, `FileFinder` logic is implemented for source (.py)
//! files only. No namespace capabilities, and probably full of bugs.
//!

use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};

use dashmap::DashMap;

use montyc_core::{patma, MapT, SpanRef};

use crate::argparse::Parser;
use crate::eval::ctx::{CallCx, EvalGlue, EvaluationContext};
use crate::exception::{PyException, PyResult, PyResultExt};
use crate::object::{AnyFunc, GlobalsHook, IntoPyValue, PyObject, PyValue, SharedObject};
use crate::rt::{ModuleKey, Runtime, RuntimeHostExt};
use crate::storage::ObjectSpace;
use crate::{argparse, ObjectId};

// -- FileFinder

/// File-based finder.
///
/// Used to search and identify potential matches across the filesystem for some module import.
///
#[derive(Debug)]
pub(crate) struct FileFinder {
    base_path: PathBuf,

    loaders: MapT<String, ObjectId>,

    cache: DashMap<String, PathBuf>,
    cache_dirty_flag: AtomicBool,
}

impl FileFinder {
    pub fn new<'a>(base_path: &'a Path) -> io::Result<Self> {
        let base_path = PathBuf::from(base_path).canonicalize()?;
        let finder = Self {
            base_path,
            loaders: Default::default(),
            cache: Default::default(),
            cache_dirty_flag: true.into(),
        };

        Ok(finder)
    }

    fn fill_cache(&self) -> io::Result<()> {
        tracing::trace!("Sourcing cache from base_path={:?}", self.base_path);

        for entry in self.base_path.read_dir()? {
            if let Ok(entry) = entry {
                let path = entry.path();
                let name = path
                    .file_name()
                    .map(|name| name.to_string_lossy())
                    .unwrap_or_else(|| path.to_string_lossy())
                    .to_string();

                tracing::trace!("    Mapping {:?} -> {:?}", name, path);

                self.cache.insert(name, path);
            }
        }

        self.cache_dirty_flag.store(false, Ordering::SeqCst);

        Ok(())
    }

    fn find_spec(
        &self,
        ecx: &mut dyn EvalGlue,
        fullname: ObjectId,
        _path: Option<ObjectId>,
        _target: Option<ObjectId>,
    ) -> PyResult<ObjectId> {
        let none_v = ecx.runtime_mut().singletons.none_v;
        let mut _is_namespace = false;

        if self.cache_dirty_flag.load(Ordering::SeqCst) {
            if let Err(err) = self.fill_cache() {
                tracing::trace!("error during cache fill {:?}", err);

                self.cache_dirty_flag.store(true, Ordering::SeqCst);
                return Ok(none_v);
            }
        }

        let fullname_st = ecx
            .runtime_mut()
            .objects
            .with_object(fullname, |this| match this {
                PyValue::Str(st) => Ok(st.clone()),
                _ => PyException::type_error().into(),
            })?;

        let mut head = fullname_st.rsplit('.');
        let tail = head.next().ok_or_else(|| todo!("IndexError"))?;

        tracing::trace!("fullname={:?}, tail={:?}", fullname_st, tail);

        if let Some(path) = self.cache.get(tail) {
            tracing::trace!("found tail={:?} in cache", tail);

            // check if the module is the name of a directory and thus a package.
            let base_path = path.join(tail);

            tracing::trace!("base_path ++ tail = {:?}", base_path);

            for (suffix, _loader) in self.loaders.iter() {
                let init_filename = format!("__init__{}", suffix);
                let full_path = base_path.join(init_filename);

                if full_path.is_file() {
                    todo!(
                        "return self._get_spec(loader, fullname, {:?}, [base_path], target)",
                        full_path
                    );
                }
            }

            _is_namespace = self.base_path.is_dir();
        }

        for (suffix, loader) in self.loaders.iter() {
            tracing::trace!("Trying loader={:?} with suffix={:?}", loader, suffix);

            let partial = format!("{}{}", tail, suffix);

            if let Some(path) = self.cache.get(&partial) {
                tracing::trace!(" cache hit for path={:?}", path.value());

                let full_path = self.base_path.join(partial);
                let path = ecx
                    .new_string(full_path.to_string_lossy().as_ref())
                    .trace()?;

                let loader = ecx.call_object(*loader, &[fullname, path]).trace()?;
                let spec = ModuleSpec::from_file_location(fullname, loader);

                return Ok(ecx.runtime_mut().objects.insert(spec.into_py_val()));
            }
        }

        todo!("is_namespace");

        // if is_namespace {
        //     tracing::trace!(
        //         "Potential namespace package {}",
        //         fullname
        //     );

        //     let spec = ModuleSpec {
        //         paths: vec![self.base_path.clone()],
        //         loader: todo!(),
        //     };

        //     Some(spec)
        // } else {
        //     None
        // }
    }
}

impl PyObject for FileFinder {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<Self>()
    }

    fn call(&self, _cx: crate::eval::ctx::CallCx) -> PyResult<ObjectId> {
        todo!()
    }

    fn repr(&self, _ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
        todo!()
    }

    fn call_method(
        &self,
        ecx: &mut dyn EvalGlue,
        name: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let find_spec = ecx.new_string("find_spec")?;

        if name == find_spec {
            let (_, ([fullname], [path, target])) =
                argparse::args_opt_unboxed(["fullname"], ["path", "target"])(args).trace()?;

            return self.find_spec(ecx, fullname, path, target).trace();
        }

        self.get_attribute(ecx, name)
            .trace()
            .and_then(|method| ecx.call_object(method, args).trace())
    }

    fn set_attribute(
        &self,
        _ecx: &mut dyn EvalGlue,
        _attr: ObjectId,
        _value: ObjectId,
    ) -> PyResult<ObjectId> {
        todo!();
    }

    fn get_attribute(&self, _ecx: &mut dyn EvalGlue, _attr: ObjectId) -> PyResult<ObjectId> {
        todo!();
    }
}

// -- ModuleSpec

#[derive(Debug)]
pub(crate) struct ModuleSpec {
    name: ObjectId,
    loader: ObjectId,
}

impl ModuleSpec {
    pub fn from_file_location(name: ObjectId, loader: ObjectId) -> Self {
        Self { name, loader }
    }
}

impl IntoPyValue for ModuleSpec {
    fn into_py_val(self) -> PyValue {
        PyValue::Dynamic(SharedObject::new(self))
    }
}

impl PyObject for ModuleSpec {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        todo!()
    }

    fn call(&self, _cx: crate::eval::ctx::CallCx) -> PyResult<ObjectId> {
        todo!()
    }

    fn repr(&self, _ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
        todo!()
    }

    fn get_attribute(&self, ecx: &mut dyn EvalGlue, attr: ObjectId) -> PyResult<ObjectId> {
        let loader = ecx.new_string("loader").trace()?;
        let name = ecx.new_string("name").trace()?;

        if attr == loader {
            Ok(self.loader)
        } else if attr == name {
            Ok(self.name)
        } else {
            PyException::attribute_error(attr, 0).into()
        }
    }
}

// -- SourceFileLoader

#[derive(Debug)]
struct SourceFileLoader {
    path: ObjectId,
}

impl PyObject for SourceFileLoader {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        todo!()
    }

    fn call_method(
        &self,
        ecx: &mut dyn EvalGlue,
        name: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let get_data = ecx.new_string("get_data").trace()?;

        if name == get_data {
            let path = ecx
                .runtime_mut()
                .objects
                .with_object(self.path, |path| match path {
                    PyValue::Str(st) => PathBuf::from(st.to_string()),
                    val => unreachable!("{:?}", val),
                });

            let source_bytes = std::fs::read_to_string(path).unwrap();

            return ecx.new_string(&*source_bytes).trace();
        }

        self.get_attribute(ecx, name)
            .trace()
            .and_then(|method| ecx.call_object(method, args).trace())
    }
}

// -- PathFinder

#[derive(Debug, Default)]
pub(crate) struct PathFinder {
    path_importer_cache: DashMap<ObjectId, ObjectId>,
}

impl PathFinder {
    fn find_spec(&self, cx: crate::eval::ctx::CallCx) -> PyResult<ObjectId> {
        let none_v = cx.ecx.runtime_mut().singletons.none_v;

        tracing::trace!("find_spec with args={:?}", cx.args);

        let ([fullname], [mut path, target]) = cx
            .parse_args_with(argparse::args_opt_unboxed(["fullname"], ["path", "target"]))
            .trace()?;

        if matches!(path, Some(p) if p == none_v) {
            path.take();
        }

        let path = match path {
            Some(path) => path,
            None => {
                let sys = cx.ecx.runtime_mut().singletons.sys;

                cx.ecx.getattr(sys, &"path").trace()?
            }
        };

        let spec = self.get_spec(cx.ecx, fullname, path, target).trace()?;

        if spec == none_v {
            tracing::trace!("get_spec returned None");

            return Ok(none_v);
        }

        let loader = cx.ecx.getattr(spec, &"loader").trace()?;

        if loader == none_v {
            todo!("spec.loader is None, go do namespace logic")
            // namespace_path = spec.submodule_search_locations
            // if namespace_path:
            //     # We found at least one namespace path.  Return a spec which
            //     # can create the namespace package.
            //     spec.origin = None
            //     spec.submodule_search_locations = _NamespacePath(fullname, namespace_path, cls._get_spec)
            //     return spec
            // else:
            //     return None
        } else {
            return Ok(spec);
        }
    }

    fn get_spec(
        &self,
        ecx: &mut dyn EvalGlue,
        fullname: ObjectId,
        path: ObjectId,
        target: Option<ObjectId>,
    ) -> PyResult<ObjectId> {
        tracing::trace!(
            "get_spec(fullname={:?}, path={:?}, target={:?})",
            fullname,
            path,
            target,
        );

        let none_v = ecx.runtime_mut().singletons.none_v;
        let find_spec = ecx.new_string("find_spec").trace()?;

        let mut path_iter = ecx.iter_object(path);

        while let Some(entry) = path_iter.next(ecx) {
            tracing::trace!("  path_iter -> {:?}", entry);

            let entry = entry.trace()?;

            let finder = self.get_path_from_importer_cache(ecx, entry).trace()?;

            if finder == none_v {
                tracing::trace!(
                    "  no finder was produced for this path, skipping to the next one."
                );

                continue;
            }

            tracing::trace!(
                "  calling finder.find_spec(fullname={:?}, target={:?})",
                fullname,
                target
            );

            let spec = ecx
                .call_method_object(finder, find_spec, &[fullname, target.unwrap_or(none_v)])
                .trace()?;

            if spec == none_v {
                tracing::trace!("  finder.find_spec returned None, skipping to the next path.");

                continue;
            }

            tracing::trace!("  finder.find_spec returned a spec={:?}", spec);

            let spec_loader = ecx.getattr(spec, &"loader").trace()?;

            if spec_loader != none_v {
                tracing::trace!("    spec even has associated loader={:?}", spec_loader);

                return Ok(spec);
            }

            // todo!();

            // let spec_portions = ecx.getattr(spec, &"submodule_search_locations").trace()?;

            // if spec_portions == none_v {
            //     return PyException::import_error()
            //         .set_message("spec missing loader")
            //         .into();
            // }

            todo!("namespace_path.extend(portions)");
        }

        todo!("spec = _bootstrap.ModuleSpec(fullname, None)")
    }

    /// Get the finder for the path entry from sys.path_importer_cache
    ///
    /// If the path entry iss not in the cache, find the appropriate finder and cache it.
    /// If no finder is available, store None.
    ///
    fn get_path_from_importer_cache(
        &self,
        ecx: &mut dyn EvalGlue,
        mut path: ObjectId,
    ) -> PyResult<ObjectId> {
        let none_v = ecx.runtime_mut().singletons.none_v;

        tracing::trace!("path={:?}", path);

        if path == ecx.new_string("").trace()? {
            tracing::trace!("path is an empty string!");

            path = match ecx
                .runtime_host_mut()
                .try_get_cwd()
                .and_then(|p| p.canonicalize())
            {
                Ok(path) => {
                    let path = format!("{}", path.display());

                    tracing::trace!("path is set to cwd={:?}", path);

                    ecx.new_string(path.as_str()).trace()?
                }

                Err(_) => none_v,
            };
        }

        match self.path_importer_cache.get(&path) {
            Some(kv) => Ok(kv.value().clone()),
            None => {
                tracing::trace!("cache miss! running path_hooks for {:?}", path);

                let finder = self.path_hooks(ecx, path).trace()?;

                tracing::trace!("hook produced finder={:?}", finder);

                self.path_importer_cache.insert(path, finder);

                Ok(finder)
            }
        }
    }

    /// Search sys.path_hooks for a finder for some `path`.
    fn path_hooks(&self, ecx: &mut dyn EvalGlue, path: ObjectId) -> PyResult<ObjectId> {
        let none_v = ecx.runtime_mut().singletons.none_v;
        let sys = ecx.runtime_mut().singletons.sys;

        tracing::trace!("finding path hook for {:?}", path);

        let sys_path_hooks = ecx.getattr(sys, &"path_hooks").trace()?;
        let mut sys_path_hooks_iter = ecx.iter_object(sys_path_hooks);

        while let Some(hook) = sys_path_hooks_iter.next(ecx) {
            let hook = hook.trace()?;

            tracing::trace!("calling hook {:?}", hook,);

            let finder = ecx.call_object(hook, &[path]);

            if matches!(&finder, Err(exc) if exc.is_import_error()) {
                tracing::trace!(" hook rejected path.",);

                continue;
            }

            return finder.trace();
        }

        tracing::trace!(" no hook found, returning None.",);

        Ok(none_v)
    }
}

impl PyObject for PathFinder {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<Self>()
    }

    fn repr(&self, ecx: &mut dyn EvalGlue) -> PyResult<ObjectId> {
        ecx.new_string("PathFinder")
    }

    fn call_method(
        &self,
        ecx: &mut dyn EvalGlue,
        name: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let find_spec = ecx.new_string("find_spec")?;

        if name == find_spec {
            let none_v = ecx.runtime_mut().singletons.none_v;
            let cx = CallCx::new(ecx, args, none_v);

            return self.find_spec(cx).trace();
        }

        self.get_attribute(ecx, name)
            .trace()
            .and_then(|method| ecx.call_object(method, args).trace())
    }
}

// -- top level utilities

fn find_spec<H>(
    ecx: &mut EvaluationContext<'_, '_, H>,
    name: ObjectId,
    path: ObjectId,
) -> PyResult<ObjectId>
where
    H: RuntimeHostExt,
{
    tracing::trace!(
        "trying to find the spec for name={:?} with path={:?}",
        name,
        path,
    );

    let sys = ecx.rt.singletons.sys;

    let (_, meta_path) = ecx
        .getattr_direct_hash(sys, ecx.rt.hash("meta_path"))
        .unwrap();

    // let is_reload = {
    //     let (_, sys_modules) = ecx
    //         .getattr_direct_hash(sys, ecx.rt.hash("modules"))
    //         .unwrap();
    //     let rt = ecx.runtime_mut();

    //     let r = rt.objects.with_object(sys_modules, |val| match val {
    //         PyValue::List(lst) => Some(lst.iter().any(|e| *e == name)),
    //         val => None,
    //     });

    //     match r {
    //         Some(a) => a,
    //         None => {
    //             let (_, contains) = ecx
    //                 .getattr_direct_hash(sys_modules, ecx.rt.hash("__contains__"))
    //                 .unwrap();

    //             ecx.call_object(contains, &[name])? == ecx.rt.singletons.true_v
    //         }
    //     }
    // };

    let mut meta_path_iter = ecx.iter_object(meta_path);
    let find_spec_str = ecx.new_string("find_spec")?;

    let none_v = ecx.runtime_mut().singletons.none_v;

    while let Some(finder) = meta_path_iter.next(ecx) {
        tracing::trace!("Trying finder {:?}", finder);

        let finder = finder.trace()?;

        match ecx.call_method_object(finder, find_spec_str, &[name, path]) {
            Ok(spec) => {
                if spec == none_v {
                    continue;
                }

                return Ok(spec);
            }

            Err(exc) => {
                if exc.is_attribute_arror() {
                    continue;
                } else {
                    return Err(exc).trace();
                }
            }
        }
    }

    Ok(ecx.rt.singletons.none_v)
}

/// like `__import__` but returns a `ModuleSpec`
pub(crate) fn import_module<H>(
    ecx: &mut EvaluationContext<'_, '_, H>,
    path: &[SpanRef],
    relative: usize,
) -> PyResult<ObjectId>
where
    H: RuntimeHostExt,
{
    let sys = if ecx.rt.singletons.sys == ObjectId::default() {
        todo!("sys module has not been initialized yet, sys.path is unavailable.");
    } else {
        ecx.rt.singletons.sys
    };

    let (_, sys_modules) = ecx
        .getattr_direct_hash(sys, ecx.rt.hash("modules"))
        .unwrap();

    match path {
        [] => unreachable!(),

        [name] => {
            let name = ecx.host.spanref_to_str(*name).to_string();

            tracing::trace!("only one segment in import path {:?}", name);

            let name_obj = ecx.new_string(&name)?;
            let spec = find_spec(ecx, name_obj, ecx.rt.singletons.none_v).trace()?;

            if spec == ecx.rt.singletons.none_v {
                PyException::import_error()
                    .set_message(format!("No module named '{}'", name))
                    .into()
            } else {
                return Ok(spec);
            }
        }

        [parent, ..] => {
            let (_, contains) = ecx
                .getattr_direct_hash(sys_modules, ecx.rt.hash("__contains__"))
                .unwrap();

            let parent_as_str =
                ecx.new_string(ecx.host.spanref_to_str(*parent).to_string().as_str())?;

            let parent_module =
                if ecx.call_object(contains, &[parent_as_str])? == ecx.rt.singletons.false_v {
                    import_module(ecx, &[parent.clone()], relative).trace()?
                } else {
                    let name_as_str = path
                        .iter()
                        .map(|part| ecx.host.spanref_to_str(*part))
                        .collect::<Vec<_>>()
                        .join(".");

                    let name_as_str = ecx.new_string(&name_as_str)?;

                    if ecx.call_object(contains, &[name_as_str])? == ecx.rt.singletons.true_v {
                        // importing the parent module may have caused this import to be fulfilled...
                        // "crazy side-effects!"

                        let (_, get_item) =
                            ecx.getattr_direct_hash(sys_modules, ecx.rt.hash("__getitem__"))?;

                        return ecx.call_object(get_item, &[name_as_str]);
                    }

                    let (_, get_item) =
                        ecx.getattr_direct_hash(sys_modules, ecx.rt.hash("__getitem__"))?;

                    ecx.call_object(get_item, &[parent_as_str])?
                };

            let (_, _parent_module_path) =
                ecx.getattr_direct_hash(parent_module, ecx.rt.hash("__path__"))?;

            // let spec = import_module(ecx, parent_module__path__, todo!())?;

            todo!("spec")
        }
    }
}

/// Top-level initialization for import bootstrapping.
pub(crate) fn setup(
    rt: &mut Runtime,
    sys_meta_path: ObjectId,
    sys_path_hooks: ObjectId,
) -> PyResult<()> {
    let mut module = PyValue::Module {
        mkey: ModuleKey::Static("importlib._bootstrap"),
        inner: Default::default(),
    };

    let module_dict = patma!(inner, PyValue::Module { inner, .. } in &mut module).unwrap();

    let path_finder = rt.objects.insert_with(|_this| {
        let finder = PathFinder::default();

        SharedObject::new(finder).into_py_val()
    });

    {
        let sfl_hash = rt.hash("SourceFileLoader");
        let sfl_str = rt.new_string("SourceFileLoader");

        fn new_source_file_loader(cx: CallCx) -> PyResult<ObjectId> {
            let (cx, (_, path)) = argparse::arg("fullname")
                .then_arg("path")
                .parse_with_cx(cx)
                .unwrap();

            let [loader] = [SourceFileLoader { path }]
                .map(SharedObject::new)
                .map(PyValue::Dynamic);

            Ok(cx.ecx.runtime_mut().objects.insert(loader))
        }

        let sfl = rt.new_callable(new_source_file_loader);

        module_dict.__dict__.insert(sfl_hash, (sfl_str, sfl));
    }

    rt.objects
        .with_object_mut(sys_meta_path, |this| match this {
            PyValue::List(ref mut lst) => lst.extend_from_slice(&[path_finder]),
            obj => unreachable!("{:?}", obj),
        });

    let path_hooks_for_file_finder = {
        let inner = |cx: CallCx| {
            // let (args, _) = cx.parse_args_with(argparse::args_opt(["path"], []))?;
            // let [path] = args.as_ref().clone();
            let (cx, (path,)) = argparse::arg("path").parse_with_cx(cx).unwrap();

            let path = cx
                .ecx
                .runtime_mut()
                .objects
                .with_object(path, |path| match path {
                    PyValue::Str(st) => Ok(st.to_string()),
                    _ => PyException::type_error()
                        .set_message("path must be a string")
                        .into(),
                })
                .map(PathBuf::from)?;

            if !path.is_dir() {
                return PyException::import_error()
                    .set_message("path must be a directory")
                    .into();
            }

            let mut finder = FileFinder::new(&path).unwrap();

            let source_loader = match cx.ecx.getattr(cx.globals, &"SourceFileLoader").trace() {
                Ok(loader_class) => loader_class,

                Err(exc) if exc.is_attribute_arror() => {
                    todo!("construct SourceFileLoader in place")
                }
                err @ Err(_) => return err,
            };

            finder.loaders.insert(".py".into(), source_loader);

            let finder = cx
                .ecx
                .runtime_mut()
                .objects
                .insert(SharedObject::new(finder).into_py_val());

            Ok(finder)
        };

        fn hook(cx: CallCx) -> PyResult<ObjectId> {
            let search = cx
                .ecx
                .runtime_mut()
                .objects
                .filter_map(|id, val| match val {
                    PyValue::Module { mkey, .. }
                        if matches!(mkey, ModuleKey::Static("importlib._bootstrap")) =>
                    {
                        Some(id)
                    }

                    _ => None,
                });

            Ok(search.unwrap_or(cx.ecx.runtime_mut().singletons.none_v))
        }

        let any = AnyFunc::Native {
            inner,
            hook: Some(GlobalsHook::ComputeWith(hook)),
        };

        rt.new_callable(any)
    };

    rt.objects
        .with_object_mut(sys_path_hooks, |this| match this {
            PyValue::List(ref mut lst) => lst.extend_from_slice(&[path_hooks_for_file_finder]),
            obj => unreachable!("{:?}", obj),
        });

    // rt.objects.with_object_mut(sys_modules, |this| match this {
    //     PyValue::List(ref mut lst) => lst.extend_from_slice(&[path_hooks_for_file_finder]),
    //     obj => unreachable!("{:?}", obj),
    // });

    rt.objects.insert(module);

    Ok(())
}
