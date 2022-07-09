use std::iter::FromIterator;

use ahash::AHashMap;
use ariadne::{Label, Report, ReportKind, Source};

use montyc_core::{
    patma, Function, ModuleRef, MontyError, MontyResult, TypeError, TypeId, TypingConstants,
    TypingContext, Value, ValueId,
};
use montyc_flatcode::{FlatCode, SequenceType};
use montyc_query::Queries;

use crate::{
    def_stack::{DefScope, ScopeChain},
    prelude::SessionContext,
};

#[derive(Debug)]
pub struct ValueContext<'this, 'gcx> {
    pub mref: ModuleRef,
    pub code: &'this FlatCode,
    pub gcx: &'gcx mut SessionContext,
    pub value: &'this Value,
    pub value_idx: ValueId,
}

struct SourceCache<'a, 'b> {
    cx: &'a ValueContext<'a, 'b>,
    inner: AHashMap<ModuleRef, ariadne::Source>,
}

impl<'t, 'c> From<&'t ValueContext<'t, 'c>> for SourceCache<'t, 'c> {
    fn from(cx: &'t ValueContext<'t, 'c>) -> Self {
        Self {
            cx,
            inner: Default::default(),
        }
    }
}

impl<'t, 'c> ariadne::Cache<ModuleRef> for SourceCache<'t, 'c> {
    fn fetch(&mut self, id: &ModuleRef) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        let Self { cx, inner } = self;

        let source = inner
            .entry(id.clone())
            .or_insert_with(|| ariadne::Source::from(cx.gcx.module_sources[id].clone()));

        Ok(source)
    }

    fn display<'a>(&self, _id: &'a ModuleRef) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new("wtf") as Box<_>)
    }
}

impl<'this, 'gcx> ValueContext<'this, 'gcx> {
    fn typeck_funcdef(mut self) -> MontyResult<TypeId> {
        let (ret_t, args_t, source, name) = patma!((ret_t, args_t, source, name), Value::Function { ret_t, args_t, source, name, .. } in self.value).unwrap();

        log::debug!("[ValueContext::typecheck] Typechecking function {:?}", name);

        let name = name
            .clone()
            .unwrap_or_else(|st| self.gcx.str_to_spanref(&st));

        self.gcx
            .value_store
            .metadata(self.value_idx)
            .type_id
            .replace(TypingConstants::UntypedFunc);

        let return_type = {
            match self.gcx.value_store.get(*ret_t).unwrap() {
                Value::Object { kind, .. } => self.gcx.typing_context.insert(kind.clone().into()),

                Value::Module { .. } => TypingConstants::Module,
                Value::String(_) => TypingConstants::Str,
                Value::Integer(_) => TypingConstants::Int,
                Value::Dict { .. } => todo!(),
                Value::Function { .. } => todo!(),
                Value::Class { .. } => self.gcx.value_store.metadata(*ret_t).type_id.unwrap(),
            }
        };

        let mut params = Vec::with_capacity(
            args_t
                .as_ref()
                .map(|(recv, args)| recv.is_some() as usize + args.len())
                .unwrap_or(0),
        );

        let arg_t: Option<Vec<TypeId>> = if let Some((recv, args)) = args_t {
            let mut seen = ahash::AHashSet::with_capacity(recv.is_some() as usize + args.len());

            if let Some(recv) = recv {
                seen.insert(recv.clone().group());
                params.push((recv.group(), TypingConstants::TSelf));
            }

            let mut is_dynamically_typed = false;

            // Verify that the parameter names are all unique.
            for (arg, kind) in args.iter() {
                // true iff the spanref group was already present in the set.
                if !seen.insert(arg.group()) {
                    return Err(MontyError::TypeError {
                        module: self.mref.clone(),
                        error: TypeError::DuplicateParameters,
                    });
                }

                if !is_dynamically_typed {
                    is_dynamically_typed = kind.is_none();
                }

                match kind.clone() {
                    Some(kind_value_ix) => {
                        let kind = self
                            .gcx
                            .value_store
                            .metadata(kind_value_ix)
                            .type_id
                            .unwrap();

                        params.push((arg.group(), kind))
                    }

                    None => continue,
                }
            }

            // Ignore any function definitions with non-annotated arguments.
            if is_dynamically_typed {
                log::debug!(
                    "[ValueContext::typecheck] Marking function as dynamically typed: {:?}",
                    name
                );

                return Ok(TypingConstants::UntypedFunc);
            } else {
                Some(params.iter().map(|(_, t)| *t).collect())
            }
        } else {
            None
        };

        let func_type = self.gcx.typing_context.callable(arg_t, return_type);

        self.gcx
            .value_store
            .metadata(self.value_idx)
            .type_id
            .replace(func_type);

        let seq = match source {
            Some((s_mref, seq)) => {
                let seq = *seq;

                if *s_mref != self.mref {
                    return Ok(func_type);
                }

                if self.code.is_sequence_ellipsis_stubbed(seq) {
                    let _seq = self.code.sequences()[seq].clone();
                    let meta = self.gcx.value_store.metadata(self.value_idx);

                    let mut def_stack = ScopeChain::new(Default::default());
                    def_stack.parameters.extend(params);

                    meta.type_id.replace(func_type);
                    meta.rib.replace(def_stack.parameters.into_iter().collect());

                    meta.function.replace(Function {
                        type_id: func_type,
                        refs: Default::default(),
                        mref: self.mref,
                        name,
                        value_id: self.value_idx.into(),
                    });

                    return Ok(func_type);
                }

                seq
            }

            None => todo!(),
        };

        let mut def_stack = {
            if self.mref != ModuleRef(1) {
                let builtin_rib = self
                    .gcx
                    .value_store
                    .metadata(ModuleRef(1))
                    .rib
                    .clone()
                    .unwrap();

                ScopeChain::new(builtin_rib)
            } else {
                ScopeChain::new(Default::default())
            }
        };

        {
            let module_rib = self
                .gcx
                .value_store
                .metadata(self.mref)
                .rib
                .clone()
                .unwrap();

            def_stack.globals.extend(module_rib);
            def_stack.parameters.extend(params);
        }

        let mut seq = self.code.sequences()[seq].clone();
        seq.kind = SequenceType::Function(Some(return_type));

        let (_code, refs) = match crate::typeck::typeck_seq(&mut self, seq, &mut def_stack) {
            Ok(a) => a,
            Err(errors) => {
                let tcx = &self.gcx.typing_context;

                for error in errors {
                    if let MontyError::TypeError { module: _, error } = error {
                        type R = Report<(ModuleRef, montyc_core::Span)>;

                        #[allow(warnings)]
                        let _ = match error {
                            TypeError::UnboundLocal {
                                name,
                                used,
                                defined: (module, offset),
                            } => {
                                todo!()
                            }

                            TypeError::DuplicateParameters => todo!(),
                            TypeError::ReturnOutsideFunction(_) => todo!(),

                            TypeError::IncompatibleReassignment {
                                name,
                                first_assignment,
                                bad_reassignment,
                                expected_type,
                                actual_type,
                            } => todo!(),

                            TypeError::BadReturnType {
                                expected,
                                actual,
                                ret_node,
                                def_node,
                            } => R::build(ReportKind::Error, ret_node.0, ret_node.1.start)
                                .with_message("Bad return type.")
                                .with_label(
                                    Label::new(ret_node)
                                        .with_message(format!(
                                            "This returns an object with a type of: {}",
                                            yansi::Paint::red(tcx.display_type(actual).unwrap())
                                        ))
                                        .with_color(ariadne::Color::Red),
                                )
                                .with_label(
                                    Label::new(def_node)
                                        .with_message(format!(
                                            "This function was annotated to return a type of: {}",
                                            yansi::Paint::yellow(
                                                tcx.display_type(expected).unwrap()
                                            )
                                        ))
                                        .with_color(ariadne::Color::Yellow),
                                )
                                .finish()
                                .print(SourceCache::from(&self)),

                            TypeError::BadConditionalType { actual, span } => todo!(),
                            TypeError::MissingReturn {
                                expected,
                                def_span,
                                ret_span,
                            } => todo!(),
                            TypeError::NotCallable { kind, callsite } => todo!(),
                            TypeError::UnknownType { sref } => todo!(),
                            TypeError::UndefinedVariable { sref } => todo!(),
                            TypeError::BadArgumentType {
                                expected,
                                actual,
                                arg_node,
                                def_node,
                            } => todo!(),
                            TypeError::SusArgumentLength {
                                callsite,
                                expected,
                                actual,
                            } => todo!(),
                            TypeError::IncompatibleTypes {
                                left_span,
                                left,
                                right_span,
                                right,
                            } => todo!(),
                            TypeError::BadBinaryOp { span, left, right } => todo!(),
                            TypeError::NotAFunction => todo!(),
                            TypeError::Unsupported { span, message } => todo!(),
                        };
                    }
                }

                todo!();
            }
        };

        let meta = self.gcx.value_store.metadata(self.value_idx);

        meta.type_id.replace(func_type);
        meta.rib
            .replace(<_ as FromIterator<(u32, TypeId)>>::from_iter(
                def_stack
                    .parameters
                    .into_iter()
                    .chain(def_stack.locals.into_iter()),
            ));

        meta.function.replace(Function {
            type_id: func_type,
            refs,
            mref: self.mref,
            name,
            value_id: self.value_idx.into(),
        });

        Ok(func_type)
    }

    fn typeck_classdef(self) -> MontyResult<TypeId> {
        let (name, properties) =
            patma!((name, properties), Value::Class { name, properties } in self.value).unwrap();

        log::trace!(
            "[Value::typecheck(ValueContext)] Typechecking Value::Class {{ name: {:?} }}",
            name
        );

        let class_value_id = self.value_idx;

        let type_id = {
            if self.mref == ModuleRef(1) {
                // builtins

                let type_id = match name.as_str() {
                    "int" => TypingConstants::Int,
                    "str" => TypingConstants::Str,
                    "bool" => TypingConstants::Bool,
                    "type" => TypingConstants::Type,
                    "float" => TypingConstants::Float,
                    "object" => TypingConstants::Object,
                    "tuple" => TypingConstants::UntypedTuple,
                    name => todo!("custom builtin class is not supported yet {:?}", name),
                };

                log::trace!(
                    "[Value::typecheck(ValueContext)] Setting value = {:?} as class of {:?}",
                    class_value_id,
                    type_id
                );

                self.gcx
                    .value_store
                    .type_data
                    .insert(type_id, class_value_id);

                type_id
            } else {
                // any other module.
                todo!();
            }
        };

        self.gcx
            .value_store
            .metadata(class_value_id)
            .type_id
            .replace(type_id);

        let mut rib = DefScope::default();

        let it = properties.iter_by_alloc_asc(|values| {
            let mut allocs: Vec<_> = self
                .gcx
                .value_store
                .alloc_data
                .iter()
                .filter_map(|(alloc, index)| values.get(index).map(|key| (*alloc, *index, *key)))
                .collect();

            allocs.sort_unstable_by(|(a, _, _), (b, _, _)| a.cmp(&b));

            allocs.into_iter().map(|(_, value, key)| (key, value))
        });

        for (key_idx, value_idx) in it.collect::<Vec<_>>() {
            let value = &self
                .gcx
                .value_store
                .value_graph
                .node_weight(value_idx.into())
                .unwrap()
                .clone();

            let value_type = ValueContext {
                mref: self.mref,
                gcx: self.gcx,
                code: self.code,
                value,
                value_idx,
            }
            .typecheck()?;

            let tid = &mut self.gcx.value_store.metadata(value_idx).type_id;

            if tid.is_none() {
                tid.replace(value_type);
            }

            if let Some(key) = self
                .gcx
                .value_store
                .value_graph
                .node_weight(key_idx.into())
                .map(|weight| match weight {
                    Value::String(st) => self.gcx.str_to_spanref(st),
                    _ => unreachable!(),
                })
            {
                rib.insert(key.group(), value_type);

                self.gcx
                    .value_store
                    .metadata(class_value_id)
                    .rib
                    .replace(rib.clone());
            }
        }

        Ok(type_id)
    }

    pub(crate) fn typecheck(self) -> MontyResult<TypeId> {
        match self.value {
            Value::Function { .. } => self.typeck_funcdef(),
            Value::Class { .. } => self.typeck_classdef(),

            Value::Dict { object: _, data: _ } => todo!(),

            Value::Module { .. } => Ok(TypingConstants::Module),
            Value::String(_) => Ok(TypingConstants::Str),
            Value::Integer(_) => Ok(TypingConstants::Int),
            Value::Object { .. } => Ok(TypingConstants::Object),
        }
    }
}
