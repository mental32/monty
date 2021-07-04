//! An executor that walks over a module and evaluates them/their side effects.

use montyc_core::{ModuleRef, SpanRef};
use montyc_parser::{
    ast::{self, ClassDef},
    AstNode, AstObject, AstVisitor,
};

use petgraph::{graph::NodeIndex, visit::NodeIndexable};

use crate::{ObjectGraph, interpreter::{HashKeyT, HostGlue, ObjAllocId, PyDictRaw, PyResult, object::{PyObject, RawObject, class::ClassObj, dict, int::IntObj, string::StrObj}}};

use super::object::func;

#[derive(Debug, Clone, Copy)]
enum StackFrame {
    Module(ObjAllocId),
    Function(ObjAllocId),
}

macro_rules! patma {
    ($n:expr; $( $pattern:pat )|+ $( if ($guard: expr) )? $(,)? = $e:expr) => {
        match $e {
            $( $pattern )|+ $( if $guard )? => Some($n),
            #[allow(warnings)]
            _ => None,
        }
    };
}

/// Responsible for executing a single module.
pub(in crate::interpreter) struct ModuleExecutor<'global, 'module> {
    pub runtime: &'global mut super::Runtime,
    pub module: &'module crate::ModuleObject,
    pub host: &'global dyn HostGlue,

    node_index: NodeIndex<u32>,
    eval_stack: Vec<(NodeIndex<u32>, StackFrame)>,
}

impl<'global, 'module> ModuleExecutor<'global, 'module> {
    pub fn new(
        runtime: &'global mut super::Runtime,
        module: &'module crate::ModuleObject,
        host: &'global dyn HostGlue,
    ) -> Self {
        Self {
            runtime,
            module,
            host,
            node_index: module.body.from_index(0),
            eval_stack: vec![],
        }
    }

    pub fn define(&mut self, name: SpanRef, value: ObjAllocId) -> PyResult<()> {
        let (_, frame) = self.eval_stack.last().as_ref().unwrap();
        let mut frame =
            patma!(*o; StackFrame::Function(o) | StackFrame::Module(o) = frame).unwrap();

        let name = self.host.spanref_to_str(name);
        let (name, hash) = self.string(Some(name))?;

        frame.set_attribute_direct(self.runtime, hash, name, value);

        Ok(())
    }

    pub fn lookup(&mut self, name: SpanRef) -> Option<ObjAllocId> {
        let (mut ix, frame) = self
            .eval_stack
            .last()
            .as_ref()
            .expect("Must be running to perform lookups.");

        let mut scope_object =
            patma!(*o; StackFrame::Function(o) | StackFrame::Module(o) = frame).unwrap();

        let nodes = self.runtime.scope_graph.0.raw_nodes();
        let name = self.host.spanref_to_str(name);

        while {
            match scope_object.getattr_static(self.runtime, name) {
                Some(o) => return Some(o),
                None => true,
            }
        } {
            let parent = self
                .runtime
                .scope_graph
                .0
                .neighbors_directed(ix, petgraph::EdgeDirection::Outgoing)
                .next()?;

            ix = parent;
            scope_object = nodes[parent.index()].weight;
        }

        unreachable!()
    }

    fn insert_new_object(
        &mut self,
        f: impl FnOnce(&mut Self, ObjAllocId) -> PyResult<Box<dyn PyObject>>,
    ) -> PyResult<ObjAllocId> {
        let object_alloc_id = self.runtime.objects.reserve();
        let object = f(self, object_alloc_id)?;
        let _ = self.runtime.objects.try_set_value(object_alloc_id, object);
        Ok(object_alloc_id)
    }

    /// Create a new string object.
    pub(super) fn string(
        &mut self,
        initial: Option<impl AsRef<str>>,
    ) -> PyResult<(ObjAllocId, HashKeyT)> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "str")
            .unwrap();

        let initial = initial.map(|r| r.as_ref().to_owned()).unwrap_or_default();
        let st_hash = self.runtime.hash(initial.clone());

        if let Some(cached) = self.runtime.strings.get(&st_hash) {
            return Ok((*cached, st_hash));
        }

        let obj = self.insert_new_object(move |this, object_alloc_id| {
            this.runtime.strings.insert(st_hash, object_alloc_id);

            let object = RawObject {
                alloc_id: object_alloc_id,
                __dict__: Default::default(),
                __class__,
            };

            let string = StrObj {
                header: object,
                value: initial,
            };

            Ok(Box::new(string))
        })?;

        Ok((obj, st_hash))
    }

    /// Create a new integer object.
    #[inline]
    pub(super) fn integer(&mut self, n: i64) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "int")
            .unwrap();

        self.insert_new_object(|_, object_alloc_id| {
            let object = RawObject {
                alloc_id: object_alloc_id,
                __dict__: Default::default(),
                __class__,
            };

            let integer = IntObj {
                header: object,
                value: n,
            };

            Ok(Box::new(integer))
        })
    }

    fn advance_next_node(&mut self) {
        assert!(!self.runtime.tick());

        self.node_index = self
            .module
            .body
            .neighbors_directed(self.node_index, petgraph::EdgeDirection::Outgoing)
            .next()
            .unwrap_or(NodeIndex::end());
    }

    fn new_module_object(&mut self) -> PyResult<ObjAllocId> {
        let alloc_id = self.runtime.objects.reserve();
        let __class__ = self
            .runtime
            .builtins
            .get_attribute_direct(self.runtime, self.runtime.hash("_module_type_"), alloc_id)
            .unwrap()
            .alloc_id();

        let module = RawObject {
            alloc_id,
            __dict__: Default::default(),
            __class__,
        };

        self.runtime
            .objects
            .try_set_value(alloc_id, module)
            .unwrap();

        Ok(alloc_id)
    }

    #[inline]
    fn set_attribute(
        &mut self,
        object: &mut impl PyObject,
        key: &str,
        value: ObjAllocId,
    ) -> PyResult<()> {
        let (key, hash) = self.string(Some(key))?;
        object.set_attribute_direct(self.runtime, hash, key, value);
        Ok(())
    }

    #[inline]
    fn dict(&mut self) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "dict")
            .unwrap();

        self.insert_new_object(move |_, object_alloc_id| {
            let object = RawObject {
                alloc_id: object_alloc_id,
                __dict__: Default::default(),
                __class__,
            };

            let raw = PyDictRaw::default();

            let object = dict::PyDict { 
                header: object,
                data: raw
            };

            Ok(Box::new(object))
        })
    }

    #[inline]
    fn function_from_closure(&mut self, name: &str, f: func::NativeFn) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "_function_type_")
            .unwrap();

        self.insert_new_object(move |ex, object_alloc_id| {
            let mut object = RawObject {
                alloc_id: object_alloc_id,
                __dict__: Default::default(),
                __class__,
            };

            let value = ex.string(Some(name))?.0;
            ex.set_attribute(&mut object, "__name__", value)?;

            let value = ex.dict()?;
            ex.set_attribute(&mut object, "__annotations__", value)?;

            let object = func::Function {
                header: object,
                inner: func::Callable::Native(f),
                defsite: None,
            };

            Ok(Box::new(object))
        })
    }

    #[inline]
    fn function_from_funcdef_node(
        &mut self,
        name: &str,
        idx: NodeIndex<u32>,
    ) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "_function_type_")
            .unwrap();

        self.insert_new_object(move |ex, object_alloc_id| {
            let mut object = RawObject {
                alloc_id: object_alloc_id,
                __dict__: Default::default(),
                __class__,
            };

            let value = ex.string(Some(name))?.0;

            ex.set_attribute(&mut object, "__name__", value)?;

            let funcdef = &ex.module.body.raw_nodes().get(idx.index()).unwrap().weight;
            let funcdef = match funcdef {
                AstNode::FuncDef(fndef) => fndef,
                _ => unreachable!(),
            };

            let mut annotations = ex.dict()?;

            if let Some(args) = funcdef.args.as_ref() {
                for (arg, ann) in args.iter() {
                    if let Some(ann) = ann {
                        let arg = ex.host.spanref_to_str(*arg);
                        let (arg, _) = ex.string(Some(arg))?;

                        match ann.visit_with(ex)? {
                            Some(obj) => annotations.set_item(&ex.runtime, arg, obj),
                            None => todo!("NameError: argument annotation not found."),
                        };
                    }
                }

                if let Some(ret) = &funcdef.returns {
                    let (returns, hash) = ex.string(Some("return"))?;

                    dbg!(hash);

                    match ret.inner.visit_with(ex)? {
                        Some(obj) => annotations.set_item(&ex.runtime, returns, obj),
                        None => todo!("NameError: argument annotation not found."),
                    };
                }
            }

            ex.set_attribute(&mut object, "__annotations__", annotations)?;

            let object = func::Function {
                header: object,
                inner: func::Callable::SourceDef(idx),
                defsite: Some(idx),
            };

            Ok(Box::new(object))
        })
    }

    #[inline]
    fn new_class_object(&mut self, klass: &ClassDef) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "_object_type_")
            .unwrap();

        let name = klass.name.inner.as_name().unwrap();
        let name = self.host.spanref_to_str(name);

        self.insert_new_object(move |ex, object_alloc_id| {
            let mut object = RawObject {
                alloc_id: object_alloc_id,
                __dict__: Default::default(),
                __class__,
            };

            let value = ex.string(Some(name))?.0;

            ex.set_attribute(&mut object, "__name__", value)?;

            let object = ClassObj { header: object };

            Ok(Box::new(object))
        })
    }

    #[inline]
    pub fn name_to_spanref_hashed(&self, name: &str) -> (u64, SpanRef) {
        let sref = self.host.name_to_spanref(name);
        let hash = self.runtime.hash(sref);

        (hash, sref)
    }

    pub fn run_until_complete(mut self) -> PyResult<ObjAllocId> {
        let raw_nodes = self.module.body.raw_nodes();

        let mut __monty = self
            .runtime
            .modules
            .get(&ModuleRef(0))
            .cloned()
            .expect("`__monty` module should exist.");

        let initialized = self.runtime.hash("initialized");

        let ex = &mut self;

        if __monty
            .get_attribute_direct(ex.runtime, initialized, __monty)
            .is_none()
        {
            let extern_func = ex.function_from_closure("extern", |_ex, _args| todo!())?;

            __monty.setattr_static(ex.runtime, "extern", extern_func);
        }

        let module_object = ex.new_module_object()?;
        let namespace = ex.runtime.scope_graph.insert(module_object);

        ex.runtime.modules.insert(ex.module.mref, module_object);

        let frame = StackFrame::Module(module_object);

        ex.eval_stack.push((namespace, frame.clone()));

        loop {
            let node_index = ex.node_index.index();
            let node = if node_index == NodeIndex::<u32>::end().index() {
                return Ok(module_object);
            } else {
                &raw_nodes[node_index].weight
            };

            match node.visit_with(ex) {
                Ok(_) => continue,
                Err(_) => todo!(),
            }
        }
    }
}

type EvalResult = PyResult<Option<ObjAllocId>>;

impl<'global, 'module> AstVisitor<EvalResult> for ModuleExecutor<'global, 'module> {
    fn visit_any(&mut self, node: &dyn AstObject) -> EvalResult {
        unimplemented!("{:#?}", node.into_ast_node());
    }

    fn visit_pass(&mut self) -> EvalResult {
        self.advance_next_node();
        Ok(None)
    }

    fn visit_int(&mut self, int: &ast::Atom) -> EvalResult {
        let int = patma!(n; ast::Atom::Int(n) = int).unwrap().clone();

        self.integer(int).map(Some)
    }

    fn visit_assign(&mut self, asn: &ast::Assign) -> EvalResult {
        let value = asn.value.inner.visit_with(self)?.unwrap();
        let name = asn.name.inner.as_name().unwrap();

        self.define(name, value)?;

        self.advance_next_node();

        Ok(None)
    }

    fn visit_import(&mut self, import: &ast::Import) -> EvalResult {
        let from_import = matches!(import, ast::Import::From { .. });

        for decl in import.decls() {
            for (module, _) in self.host.import_module(decl.clone()) {
                let moduke_object = match self.runtime.modules.get(&module).cloned() {
                    Some(module) => module,
                    None => {
                        self.runtime.consteval(module, &*self.host)?;

                        self.runtime
                            .modules
                            .get(&module)
                            .expect("The module should've been created by now.")
                            .clone()
                    }
                };

                if from_import {
                    let attr_sref = match &decl.name.inner {
                        ast::Primary::Atomic(atom) => match atom.inner {
                            ast::Atom::Name(name) => name,
                            _ => unreachable!(),
                        },

                        _ => unreachable!(),
                    };

                    let attr = self.host.spanref_to_str(attr_sref);
                    let (attr, hash) = self.string(Some(attr))?;

                    let value = moduke_object
                        .get_attribute_direct(self.runtime, hash, attr)
                        .unwrap();

                    self.define(attr_sref, value)?;
                } else {
                    todo!("regular import");
                }
            }
        }

        self.advance_next_node();

        Ok(None)
    }

    fn visit_classdef(&mut self, classdef: &ast::ClassDef) -> EvalResult {
        let class_obj = self.new_class_object(classdef)?;

        self.define(classdef.name.inner.as_name().unwrap(), class_obj)?;

        self.advance_next_node();

        Ok(None)
    }

    fn visit_funcdef(&mut self, fndef: &ast::FunctionDef) -> EvalResult {
        let fndef_name = fndef.name.inner.as_name().unwrap();
        let name = self.host.spanref_to_str(fndef_name.clone());

        let idx = self.module.body
            .raw_nodes()
            .iter()
            .enumerate()
            .find_map(|(idx, node)| {
                if matches!(&node.weight, AstNode::FuncDef(func) if func.name.inner.as_name().map(|sref| sref.distinct() == fndef_name.distinct()).unwrap_or(false)) {
                    Some(NodeIndex::from(idx as u32))
                } else {
                    None
                }
            })
            .unwrap();

        let func_obj = self.function_from_funcdef_node(name, idx)?;

        self.define(fndef.name.inner.as_name().unwrap(), func_obj)?;

        self.advance_next_node();

        Ok(None)
    }

    fn visit_expr(&mut self, expr: &ast::Expr) -> EvalResult {
        todo!("{:?}", expr)
    }

    fn visit_float(&mut self, int: &ast::Atom) -> EvalResult {
        todo!("{:?}", int)
    }

    fn visit_str(&mut self, int: &ast::Atom) -> EvalResult {
        todo!("{:?}", int)
    }

    fn visit_none(&mut self, int: &ast::Atom) -> EvalResult {
        todo!("{:?}", int)
    }

    fn visit_name(&mut self, name: &ast::Atom) -> EvalResult {
        let name = patma!(n; ast::Atom::Name(n) = name).unwrap();
        return Ok(self.lookup(*name));
    }

    fn visit_tuple(&mut self, int: &ast::Atom) -> EvalResult {
        todo!("{:?}", int)
    }

    fn visit_ellipsis(&mut self, int: &ast::Atom) -> EvalResult {
        todo!("{:?}", int)
    }

    fn visit_bool(&mut self, int: &ast::Atom) -> EvalResult {
        todo!("{:?}", int)
    }

    fn visit_ifstmt(&mut self, ifch: &ast::IfChain) -> EvalResult {
        todo!("{:?}", ifch)
    }

    fn visit_return(&mut self, ret: &ast::Return) -> EvalResult {
        todo!("{:?}", ret)
    }

    fn visit_while(&mut self, while_: &ast::While) -> EvalResult {
        todo!("{:?}", while_)
    }

    fn visit_binop(&mut self, expr: &ast::Expr) -> EvalResult {
        todo!("{:?}", expr)
    }

    fn visit_unary(&mut self, unary: &ast::Expr) -> EvalResult {
        todo!("{:?}", unary)
    }

    fn visit_ternary(&mut self, ternary: &ast::Expr) -> EvalResult {
        todo!("{:?}", ternary)
    }

    fn visit_named_expr(&mut self, expr: &ast::Expr) -> EvalResult {
        todo!("{:?}", expr)
    }
}