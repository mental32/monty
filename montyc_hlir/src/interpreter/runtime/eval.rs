//! An executor that walks over a module and evaluates them/their side effects.

use std::{cell::RefCell, convert::TryInto, rc::Rc};

use montyc_core::{ModuleRef, SpanRef};
use montyc_parser::{
    ast::{self, ClassDef, FunctionDef},
    AstNode, AstObject, AstVisitor,
};

use petgraph::{graph::NodeIndex, visit::NodeIndexable};

use crate::{
    grapher::AstNodeGraph,
    interpreter::{
        object::{
            class::ClassObj, dict, func::Function, int::IntObj, string::StrObj, PyObject, RawObject,
        },
        HashKeyT, HostGlue, ObjAllocId, PyDictRaw, PyResult,
    },
};

use super::object::func;

#[derive(Debug, Clone, Copy)]
pub(in crate::interpreter) enum StackFrame {
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

pub(in crate::interpreter) trait TryIntoObject {
    fn try_into_object(&self, ex: &mut AstExecutor) -> PyResult<ObjAllocId>;
}

impl TryIntoObject for ObjAllocId {
    fn try_into_object(&self, _: &mut AstExecutor) -> PyResult<ObjAllocId> {
        Ok(*self)
    }
}

impl TryIntoObject for &str {
    fn try_into_object(&self, ex: &mut AstExecutor) -> PyResult<ObjAllocId> {
        ex.string(self).map(|st| st.0)
    }
}

impl TryIntoObject for SpanRef {
    fn try_into_object(&self, ex: &mut AstExecutor) -> PyResult<ObjAllocId> {
        ex.host.spanref_to_str(*self).try_into_object(ex)
    }
}

/// AST based executor used to execute arbitrary AST blocks.
pub(in crate::interpreter) struct AstExecutor<'global, 'module> {
    pub runtime: &'global mut super::Runtime,
    pub host: &'global dyn HostGlue,

    pub graph: &'module AstNodeGraph,
    pub ast_object: ObjAllocId,

    pub subgraphs: ahash::AHashMap<NodeIndex, Rc<AstNodeGraph>>,

    pub node_index: NodeIndex<u32>,
    pub eval_stack: Vec<(NodeIndex<u32>, StackFrame)>,
}

impl<'global, 'module> AstExecutor<'global, 'module> {
    /// Create a new `AstExecutor`
    #[inline]
    pub fn new_with_module(
        runtime: &'global mut super::Runtime,
        module: &'module crate::ModuleObject,
        host: &'global dyn HostGlue,
    ) -> Self {
        let graph = &module.body;

        let ast_object_id = runtime.objects.reserve();

        {
            let __class__ = runtime
                .builtins
                .get_attribute_direct(runtime, runtime.hash("_module_type_"), ast_object_id)
                .unwrap()
                .alloc_id();

            runtime
                .objects
                .try_set_value(
                    ast_object_id,
                    RawObject {
                        __class__,
                        __dict__: Default::default(),
                        alloc_id: ast_object_id,
                    },
                )
                .unwrap();

            runtime.modules.insert(module.mref, ast_object_id);
        }

        let namespace = runtime.scope_graph.insert(ast_object_id);
        let frame = StackFrame::Module(ast_object_id);

        Self {
            runtime,
            graph,
            host,
            subgraphs: Default::default(),
            ast_object: ast_object_id,
            node_index: module.body.from_index(0),
            eval_stack: vec![(namespace, frame)],
        }
    }

    #[inline]
    pub fn new_with_func(
        runtime: &'global mut super::Runtime,
        (ast_object, funcdef, graph): (
            &'module Function,
            &'module FunctionDef,
            &'module AstNodeGraph,
        ),
        host: &'global dyn HostGlue,
    ) -> Self {
        let ast_object_id = ast_object.alloc_id();

        let namespace = runtime.scope_graph.insert(ast_object_id);
        let frame = StackFrame::Function(ast_object_id);

        Self {
            runtime,
            graph,
            host,
            subgraphs: Default::default(),
            ast_object: ast_object.alloc_id(),
            node_index: graph.from_index(1),
            eval_stack: vec![(namespace, frame)],
        }
    }

    #[inline]
    fn advance_next_node(&mut self) {
        assert!(!self.runtime.tick(), "exceeded runtime budget.");

        if self.eval_stack.len() == 1 {
            self.node_index = self
                .graph
                .neighbors_directed(self.node_index, petgraph::EdgeDirection::Outgoing)
                .next()
                .unwrap_or(NodeIndex::end());
        }
    }

    #[inline]
    pub fn define(&mut self, name: SpanRef, value: ObjAllocId) -> PyResult<()> {
        let (_, frame) = self.eval_stack.last().as_ref().unwrap();
        let mut frame =
            patma!(*o; StackFrame::Function(o) | StackFrame::Module(o) = frame).unwrap();

        let name = self.host.spanref_to_str(name);
        let (name, hash) = self.string(name)?;

        frame.set_attribute_direct(self.runtime, hash, name, value);

        Ok(())
    }

    #[inline]
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

    #[inline]
    pub fn within_subgraph(
        &mut self,
        node: NodeIndex,
        graph_ctor: impl Fn() -> AstNodeGraph,
        mut f: impl FnMut(&mut Self, &AstNodeGraph) -> PyResult<()>,
    ) -> PyResult<()> {
        let _ = self.subgraphs.entry(node).or_insert_with(|| Rc::new(graph_ctor()));
        let graph = self.subgraphs.get(&node).unwrap().clone();

        f(self, &*graph)?;

        Ok(())
    }

    /// A helper method to set the stringy property `key` to `value` on some `object`.
    #[inline]
    fn set_attribute(
        &mut self,
        object: &mut impl PyObject,
        key: &str,
        value: impl TryIntoObject,
    ) -> PyResult<()> {
        let value = value.try_into_object(self)?;
        let (key, hash) = self.string(key)?;

        object.set_attribute_direct(self.runtime, hash, key, value);

        Ok(())
    }
}

impl<'global, 'module> AstExecutor<'global, 'module> {
    #[inline]
    fn insert_new_object<F, O>(&mut self, __class__: ObjAllocId, f: F) -> PyResult<ObjAllocId>
    where
        F: FnOnce(&mut Self, RawObject, ObjAllocId) -> PyResult<O>,
        O: PyObject,
    {
        let object_alloc_id = self.runtime.objects.reserve();

        let raw_object = RawObject {
            alloc_id: object_alloc_id,
            __dict__: Default::default(),
            __class__,
        };

        let object = f(self, raw_object, object_alloc_id)?;
        let object = Box::new(object) as Box<dyn PyObject>;

        let _ = self
            .runtime
            .objects
            .try_set_value(object_alloc_id, Rc::new(RefCell::new(object.into())));

        Ok(object_alloc_id)
    }

    /// Create a new string object.
    #[inline]
    pub fn string(&mut self, initial: impl AsRef<str>) -> PyResult<(ObjAllocId, HashKeyT)> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "str")
            .unwrap();

        let initial = initial.as_ref().to_owned();
        let st_hash = self.runtime.hash(initial.clone());

        if let Some(cached) = self.runtime.strings.get(&st_hash) {
            return Ok((*cached, st_hash));
        }

        let obj = self.insert_new_object(__class__, move |this, object, object_alloc_id| {
            this.runtime.strings.insert(st_hash, object_alloc_id);

            let string = StrObj {
                header: object,
                value: initial,
            };

            Ok(string)
        })?;

        Ok((obj, st_hash))
    }

    /// Create a new integer object.
    #[inline]
    pub fn integer(&mut self, value: i64) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "int")
            .unwrap();

        self.insert_new_object(__class__, |_, header, _| Ok(IntObj { header, value }))
    }

    /// Create a new, empty, dictionary object.
    #[inline]
    fn dict(&mut self) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "dict")
            .unwrap();

        self.insert_new_object(__class__, move |_, header, _| {
            let data = PyDictRaw::default();
            Ok(dict::PyDict { header, data })
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

        self.insert_new_object(__class__, move |ex, mut object, _| {
            ex.set_attribute(&mut object, "__name__", name)?;

            Ok(ClassObj { header: object })
        })
    }

    /// Create a new `function` object.
    #[inline]
    fn function(&mut self, name: &str, node: NodeIndex<u32>) -> PyResult<ObjAllocId> {
        let __class__ = self
            .runtime
            .builtins
            .getattr_static(self.runtime, "_function_type_")
            .unwrap();

        self.insert_new_object(__class__, move |ex, mut object, _| {
            ex.set_attribute(&mut object, "__name__", name)?;

            let funcdef = &ex.graph.raw_nodes().get(node.index()).unwrap().weight;
            let funcdef = match funcdef {
                AstNode::FuncDef(fndef) => fndef,
                _ => unreachable!(),
            };

            let annotations = {
                let mut annotations = ex.dict()?;

                if let Some(args) = funcdef.args.as_ref() {
                    for (arg, ann) in args.iter() {
                        if let Some(ann) = ann {
                            let arg = ex.host.spanref_to_str(*arg);
                            let (arg, _) = ex.string(arg)?;

                            match ann.visit_with(ex)? {
                                Some(obj) => annotations.set_item(&mut ex.runtime, arg, obj),
                                None => todo!("NameError: argument annotation not found."),
                            };
                        }
                    }

                    if let Some(ret) = &funcdef.returns {
                        let (returns, _) = ex.string("return")?;

                        match ret.inner.visit_with(ex)? {
                            Some(obj) => annotations.set_item(&mut ex.runtime, returns, obj),
                            None => todo!("NameError: argument annotation not found."),
                        };
                    }
                }

                annotations
            };

            ex.set_attribute(&mut object, "__annotations__", annotations)?;

            let object = func::Function {
                header: RefCell::new(object),
                inner: func::Callable::SourceDef(node),
                defsite: Some(node),
            };

            Ok(object)
        })
    }
}

impl<'global, 'module> AstExecutor<'global, 'module> {
    pub fn run_until_complete(mut self) -> PyResult<ObjAllocId> {
        let raw_nodes = self.graph.raw_nodes();

        loop {
            let node_index = self.node_index.index();
            let node = if node_index == NodeIndex::<u32>::end().index() {
                return Ok(self.ast_object);
            } else {
                &raw_nodes[node_index].weight
            };

            match node.visit_with(&mut self) {
                Ok(_) => continue,
                Err(_) => todo!(),
            }
        }
    }
}

// /// Responsible for executing a single module.
// pub(in crate::interpreter) struct ModuleExecutor<'global, 'module> {
//     pub runtime: &'global mut super::Runtime,
//     pub module: &'module crate::ModuleObject,
//     pub host: &'global dyn HostGlue,

//     pub node_index: NodeIndex<u32>,
//     pub eval_stack: Vec<(NodeIndex<u32>, StackFrame)>,
// }

// impl<'global, 'module> ModuleExecutor<'global, 'module> {
//     pub fn new(
//         runtime: &'global mut super::Runtime,
//         module: &'module crate::ModuleObject,
//         host: &'global dyn HostGlue,
//     ) -> Self {
//         Self {
//             runtime,
//             module,
//             host,
//             node_index: module.body.from_index(0),
//             eval_stack: vec![],
//         }
//     }

//     #[inline]
//     fn function_from_closure(&mut self, name: &str, f: func::NativeFn) -> PyResult<ObjAllocId> {
//         let __class__ = self
//             .runtime
//             .builtins
//             .getattr_static(self.runtime, "_function_type_")
//             .unwrap();

//         self.insert_new_object(move |ex, object_alloc_id| {
//             let mut object = RawObject {
//                 alloc_id: object_alloc_id,
//                 __dict__: Default::default(),
//                 __class__,
//             };

//             let value = ex.string(Some(name))?.0;
//             ex.set_attribute(&mut object, "__name__", value)?;

//             let value = ex.dict()?;
//             ex.set_attribute(&mut object, "__annotations__", value)?;

//             let object = func::Function {
//                 header: RefCell::new(object),
//                 inner: func::Callable::Native(f),
//                 defsite: None,
//             };

//             Ok(Box::new(object))
//         })
//     }

// }

type EvalResult = PyResult<Option<ObjAllocId>>;

impl<'global, 'module> AstVisitor<EvalResult> for AstExecutor<'global, 'module> {
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
                let module_object = match self.runtime.modules.get(&module).cloned() {
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
                    let (attr, hash) = self.string(attr)?;

                    let value = module_object
                        .get_attribute_direct(self.runtime, hash, attr)
                        .expect("failed to import value");

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

        let idx = self.graph
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

        let func_obj = self.function(name, idx)?;

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
        let (_, frame) = self.eval_stack.last().unwrap();

        assert!(matches!(frame, StackFrame::Function(_)), "{:?}", frame);

        let value = match &ret.value {
            Some(expr) => expr.visit_with(self)?.unwrap(),
            None => todo!(),
        };

        let _ = self.eval_stack.pop();

        self.node_index = NodeIndex::end();

        return Ok(Some(value));
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

    fn visit_call(&mut self, call: &ast::Primary) -> EvalResult {
        let (func, args) = patma!((func, args); ast::Primary::Call { func, args } = call).unwrap();

        let func = func.inner.visit_with(self)?.unwrap();
        let mut params = Vec::with_capacity(args.as_ref().map(|args| args.len()).unwrap_or(0));

        if let Some(args) = args {
            for arg in args.iter() {
                params.push(arg.inner.visit_with(self)?.unwrap());
            }
        }

        let result = func.call(self, params.as_slice())?;

        self.advance_next_node();

        Ok(Some(result))
    }
}
