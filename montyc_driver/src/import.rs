use std::{
    cell::RefCell,
    io,
    path::{Path, PathBuf},
    rc::Rc,
};

use montyc_core::MapT;
use montyc_hlirt::{
    ctx::{CallCx, EvalGlue},
    object::{AnyFunc, GlobalsHook, IntoPyValue, PyObject, PyValue, RawObject, SharedObject},
    ObjectId, ObjectSpace, PyException, PyResult, PyResultExt,
};

use crate::session_context::SessionContext;

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
        std::any::TypeId::of::<Self>()
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

// -- FileFinder

#[derive(Debug)]
pub(crate) struct FileFinder {
    base_path: PathBuf,
    loaders: MapT<String, ObjectId>,
}

impl FileFinder {
    pub fn new<'a>(base_path: &'a Path) -> io::Result<Self> {
        let base_path = PathBuf::from(base_path).canonicalize()?;
        let finder = Self {
            base_path,
            loaders: Default::default(),
        };

        Ok(finder)
    }

    fn find_spec(
        &self,
        ecx: &mut dyn EvalGlue,
        fullname: ObjectId,
        _path: Option<ObjectId>,
        _target: Option<ObjectId>,
    ) -> PyResult<ObjectId> {
        let mut _is_namespace = false;

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

        let mut cache = MapT::new();

        for entry in self.base_path.read_dir().map_err(|e| e.into())? {
            if let Ok(entry) = entry {
                let path = entry.path();
                let name = path
                    .file_name()
                    .map(|name| name.to_string_lossy())
                    .unwrap_or_else(|| path.to_string_lossy())
                    .to_string();

                if name == tail {
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

                tracing::trace!("    Mapping {:?} -> {:?}", name, path);

                cache.insert(name, path);
            }
        }

        for (suffix, loader) in self.loaders.iter() {
            tracing::trace!("Trying loader={:?} with suffix={:?}", loader, suffix);

            let partial = format!("{}{}", tail, suffix);

            if let Some(path) = cache.get(&partial) {
                tracing::trace!(" cache hit for path={:?}", path);

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

    fn call_method(
        &self,
        ecx: &mut dyn EvalGlue,
        name: ObjectId,
        args: &[ObjectId],
    ) -> PyResult<ObjectId> {
        let find_spec = ecx.new_string("find_spec")?;

        if name == find_spec {
            let (_, ([fullname], [path, target])) =
                montyc_hlirt::argparse::args_opt_unboxed(["fullname"], ["path", "target"])(args)
                    .trace()?;

            return self.find_spec(ecx, fullname, path, target).trace();
        }

        self.get_attribute(ecx, name)
            .trace()
            .and_then(|method| ecx.call_object(method, args).trace())
    }
}

// -- SourceFileLoader

#[derive(Debug)]
struct SourceFileLoader {
    path: ObjectId,
}

impl SourceFileLoader {
    fn new_source_file_loader(cx: CallCx) -> PyResult<ObjectId> {
        let [_fullname, path] = cx
            .parse_args_with(montyc_hlirt::argparse::args_unboxed(
                ["fullname", "path"],
                // [],
            ))
            .trace()?;

        let [loader] = [SourceFileLoader { path }]
            .map(SharedObject::new)
            .map(PyValue::Dynamic);

        Ok(cx.ecx.runtime_mut().objects.insert(loader))
    }
}

impl PyObject for SourceFileLoader {
    unsafe fn std_type_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<Self>()
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

fn path_hooks_for_file_finder(cx: CallCx) -> PyResult<ObjectId> {
    let [path] = cx.parse_args_with(montyc_hlirt::argparse::args_unboxed(["path"]))?;

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
}

mod path_finder {
    use super::*;

    fn get_path_from_importer_cache(
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

        let finder = path_hooks(ecx, path).trace()?;

        tracing::trace!("hook produced finder={:?}", finder);

        Ok(finder)
    }

    /// Search sys.path_hooks for a finder for some `path`.
    fn path_hooks(ecx: &mut dyn EvalGlue, path: ObjectId) -> PyResult<ObjectId> {
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

    fn get_spec(
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

            let finder = get_path_from_importer_cache(ecx, entry).trace()?;

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

    pub fn find_spec(cx: CallCx) -> PyResult<ObjectId> {
        let none_v = cx.ecx.runtime_mut().singletons.none_v;

        tracing::trace!("find_spec with args={:?}", cx.args);

        let ([fullname], [mut path, target]) = cx
            .parse_args_with(montyc_hlirt::argparse::args_opt_unboxed(
                ["fullname"],
                ["path", "target"],
            ))
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

        let spec = get_spec(cx.ecx, fullname, path, target).trace()?;

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
}

/// Called by the session context to initialize the importlib._bootstrap module.
pub(crate) fn setup<'a>(
    rt: Rc<RefCell<montyc_hlirt::rt::Runtime>>,
    (_mref, module): (montyc_core::ModuleRef, montyc_hlirt::ObjectId),
) -> Result<(), montyc_hlirt::rt::RuntimeError<&'a SessionContext>> {
    let mut rt = rt.borrow_mut();

    let sys = rt.singletons.sys;
    let sys_dict = rt.objects.with_object(sys, |sys| match sys {
        PyValue::Module { inner, .. } => inner.__dict__.clone(),
        _ => unreachable!(),
    });

    let sys_modules = sys_dict.get(rt.hash("modules")).unwrap().1;

    // sys.modules["importlib._bootstrap"] = {module}
    {
        let (hash, a, b) = rt.make_kv_pair("importlib._bootstrap", |_| module);
        let bootstrap = (a, b);

        rt.objects
            .with_object_mut(sys_modules, move |this| match this {
                PyValue::Dict(dict) => dict.insert(hash, bootstrap),
                _ => unreachable!(),
            });
    }

    let mut module_dict = rt.objects.with_object(module, |this| match this {
        PyValue::Module { inner, .. } => inner.__dict__.clone(),
        _ => unreachable!(),
    });

    // importlib._bootstrap.SourceFileLoader = () -> SourceFileLoader
    {
        let (h, k, v) = rt.make_kv_pair("SourceFileLoader", |rt| {
            rt.new_callable(SourceFileLoader::new_source_file_loader)
        });

        module_dict.insert(h, (k, v));
    }

    let sys_meta_path = sys_dict.get(rt.hash("meta_path")).unwrap().1;
    let path_finder_class = module_dict.get(rt.hash("PathFinder")).unwrap().1;

    // sys.meta_path.append(PathFinder)
    rt.objects
        .with_object_mut(sys_meta_path, |this| match this {
            PyValue::List(ref mut lst) => lst.push(path_finder_class),
            _ => unreachable!(),
        });

    let sys_path_hooks = sys_dict.get(rt.hash("path_hooks")).unwrap().1;
    let path_hooks_for_file_finder = {
        let any = AnyFunc::Native {
            inner: path_hooks_for_file_finder,
            hook: Some(GlobalsHook::Globals(module)),
        };

        rt.new_callable(any)
    };

    // sys.path_hooks.append(path_hooks_for_file_finder)
    rt.objects
        .with_object_mut(sys_path_hooks, |this| match this {
            PyValue::List(ref mut lst) => lst.extend_from_slice(&[path_hooks_for_file_finder]),
            obj => unreachable!("{:?}", obj),
        });

    // PathFinder.find_spec = find_spec
    {
        let (h, k, v) = rt.make_kv_pair("find_spec", |rt| rt.new_callable(path_finder::find_spec));

        rt.objects
            .with_object_mut(path_finder_class, |this| match this {
                PyValue::Class { inner, .. } => inner.__dict__.insert(h, (k, v)),
                _ => unreachable!(),
            });
    }

    rt.objects.with_object_mut(module, |this| match this {
        PyValue::Module {
            inner: RawObject { __dict__, .. },
            ..
        } => std::mem::swap(&mut module_dict, __dict__),

        _ => unreachable!(),
    });

    Ok(())
}
