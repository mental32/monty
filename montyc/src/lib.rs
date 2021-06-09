#![warn(warnings)]
#![feature(
//     variant_count,
//     bool_to_option,
//     drain_filter,
//     assert_matches,
//     box_syntax
)]

mod hliri;
mod opts;

pub mod prelude {
    use super::*;

    pub use global::GlobalContext;
    pub use opts::{CompilerOptions, VerifiedCompilerOptions};
}

mod global {
    use std::{io, path::Path};

    use montyc_core::{utils::SSAMap, ModuleRef, MontyError, SpanRef};
    use montyc_parser::SpanInterner;

    use crate::{hliri, prelude::*};

    const MAGICAL_NAMES: &[&'static str] = &[
        // names of builtin types or functions
        "int",
        "float",
        "tuple",
        "object",
        "dict",
        "set",
        "type",
        "id",
        "isinstance",
        "list",
        "str",
        "bool",
        // operator-like dunders
        "__lshift__",
        "__rlshift__",
        "__getattr__",
        "__rgetattr__",
        "__eq__",
        "__req__",
        "__pow__",
        "__rpow__",
        "__or__",
        "__ror__",
        "__and__",
        "__rand__",
        "__div__",
        "__rdiv__",
        "__ne__",
        "__rne__",
        "__getitem__",
        "__rgetitem__",
        "__call__",
        "__rcall__",
        "__mul__",
        "__rmul__",
        "__add__",
        "__radd__",
        "__sub__",
        "__rsub__",
        "__getattribute__",
        "__rgetattribute__",
        "__rshift__",
        "__rrshift__",
        // Special dunders
        "__new__",
        "__annotations__",
        "__delitem__",
        "__reversed__",
        "__module__",
        "__invert__",
        "__repr__",
        "__name__",
        "__eq__",
        "__pos__",
        "__len__",
        "__getitem__",
        "__call__",
        "__iter__",
        "__contains__",
        "__neg__",
        "__setitem__",
        "__next__",
        "__missing__",
        "__init__",
        // C types
        "c_ulong",
        "c_ulong_p",
        "c_double",
        "c_double_p",
        "c_uint",
        "c_uint_p",
        "c_bool",
        "c_bool_p",
        "c_uint32",
        "c_uint32_p",
        "c_long",
        "c_long_p",
        "c_ulonglong",
        "c_ulonglong_p",
        "c_void",
        "c_void_p",
        "c_longlong",
        "c_longlong_p",
        "c_uint64",
        "c_uint64_p",
        "c_wchar",
        "c_wchar_p",
        "c_ssize_t",
        "c_ssize_t_p",
        "c_size_t",
        "c_size_t_p",
        "c_ushort",
        "c_ushort_p",
        "c_int8",
        "c_int8_p",
        "c_uint8",
        "c_uint8_p",
        "c_uint16",
        "c_uint16_p",
        "c_int",
        "c_int_p",
        "c_longdouble",
        "c_longdouble_p",
        "c_char",
        "c_char_p",
        "c_float",
        "c_float_p",
        "c_byte",
        "c_byte_p",
        "c_int64",
        "c_int64_p",
        "c_int32",
        "c_int32_p",
        "c_ubyte",
        "c_ubyte_p",
        "__value",
    ] as &[_];

    type StaticNames = ahash::AHashMap<SpanRef, &'static str>;

    pub struct GlobalContext {
        /// The options this context was created with.
        opts: CompilerOptions,

        /// A Span interner shared between all parsing sessions.
        spanner: SpanInterner,

        /// Static names that get loaded at startup.
        static_names: StaticNames,

        /// A registry of the current modules that have been included.
        modules: SSAMap<ModuleRef, montyc_hlir::Module>,

        /// The global HLIR runtime.
        hliri: crate::hliri::Runtime,
    }

    impl GlobalContext {
        pub fn initialize(VerifiedCompilerOptions(opts): &VerifiedCompilerOptions) -> Self {
            let mut gcx = Self {
                opts: opts.clone(),
                spanner: SpanInterner::new(),
                static_names: Default::default(),
                modules: SSAMap::new(),
                hliri: hliri::Runtime,
            };

            gcx.modules.skip(1);

            log::debug!("[global_context:initialize] {:?}", opts);
            log::debug!("[global_context:initialize] stdlib := {:?}", opts.libstd());

            // There are identifiers we want to refer to statically e.g. __new__, __del__, or __rrshift__
            // but `SpanRef`s are generated at runtime through our `SpanInterner` so we just initialize a map
            // of `Dict[SpanRef, str]` at startup.
            //
            // This enables us to soundly refer to static identifiers as spans and correctly get the original
            // stringy name back from a `SpanRef` for codespan/reflection purposes.
            for raw_name in MAGICAL_NAMES.iter().cloned() {
                let name_ref = gcx
                    .spanner
                    .static_name_to_spanref::<0>(raw_name) // INVARIANT: ModuleRef(0) is reserved.
                    .expect("Span interner was already mutably borrowed!");

                gcx.static_names.insert(name_ref, raw_name);
            }

            log::debug!(
                "[global_context:initialize] initialized {:?} static names",
                gcx.static_names.len()
            );

            // TODO: typing context initialization

            let _ = gcx.include_module(opts.libstd().join("builtins.py"));

            gcx
        }

        #[inline]
        fn load_module_with<T>(
            &mut self,
            path: impl AsRef<Path>,
            f: impl Fn(&Self, ModuleRef) -> T,
        ) -> io::Result<T> {
            let path = path.as_ref();

            if let Some(_) = self
                .modules
                .iter()
                .filter(|(_, module)| module.path == path)
                .next()
            {
                log::error!("[global_context:load_module_with] Found a module with the same path as one we're trying to load! path={:?}", path);

                return Err(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    "Attempted to load a module with a path that is already loaded.",
                ));
            }

            let mref = self.modules.reserve();

            log::debug!(
                "[global_context:load_module_with] Loading module {:?}",
                path
            );

            let source = match std::fs::read_to_string(path) {
                Ok(st) => st.into_boxed_str(),
                Err(why) => {
                    log::error!(
                        "[global_context:load_module_with] Failed to read path contents! {:?}",
                        why
                    );
                    return Err(why);
                }
            };

            let module = montyc_parser::parse(
                source,
                montyc_parser::comb::module,
                Some(self.spanner.clone()),
                mref,
            );

            let module = montyc_hlir::ModuleObject {
                path: path.to_path_buf(),
                body: montyc_hlir::to_hlir_object(&module),
                ast: module,
            };

            let _ = self.modules.try_set_value(mref, module).unwrap();

            Ok(f(self, mref))
        }

        #[inline]
        pub fn include_module(
            &mut self,
            path: impl AsRef<Path>,
        ) -> montyc_core::MontyResult<ModuleRef> {
            self.load_module_with(path, |gcx, mref| {

                // let _ = gcx.hliri.eval(gcx, mref);

                todo!();

            })
            .map_err(|err| MontyError::IO(err))
        }
    }
}

// pub(crate) mod utils {
//     use std::rc::Rc;

//     use crate::{
//         ast::expr::{Expr, InfixOp},
//         context::LocalContext,
//         prelude::{AstObject, LocalTypeId, TypedObject},
//     };

//     fn lens_object(
//         o: &dyn AstObject,
//         predicate: &dyn Fn(&dyn AstObject) -> bool,
//     ) -> Option<Rc<dyn AstObject>> {
//         for subnode in o.walk()? {
//             if predicate(&*subnode) {
//                 return Some(Rc::clone(&subnode));
//             }

//             match lens_object(&*subnode, predicate) {
//                 s @ Some(_) => return s,
//                 None => continue,
//             }
//         }

//         None
//     }

//     pub fn try_parse_union_literal(
//         ctx: &LocalContext<'_>,
//         e: &Expr,
//         in_binop: bool,
//     ) -> crate::Result<Option<Vec<LocalTypeId>>> {
//         if let Expr::BinOp {
//             left,
//             right,
//             op: InfixOp::Or,
//         } = e
//         {
//             let l = ctx.with(left.clone(), |ctx, left| {
//                 try_parse_union_literal(&ctx, &left.inner, true)
//             })?;
//             let r = ctx.with(right.clone(), |ctx, right| {
//                 try_parse_union_literal(&ctx, &right.inner, true)
//             })?;

//             let mut v = l.unwrap_or_default();
//             v.extend(r.unwrap_or_default());

//             if v.is_empty() {
//                 Ok(None)
//             } else {
//                 Ok(Some(v))
//             }
//         } else if in_binop {
//             Ok(Some(vec![e
//                 .infer_type(ctx)?
//                 .canonicalize(&ctx.global_context.type_map)]))
//         } else {
//             Ok(None)
//         }
//     }

//     pub fn lens<T>(
//         object: &Rc<T>,
//         predicate: impl Fn(&dyn AstObject) -> bool,
//     ) -> Option<Rc<dyn AstObject>>
//     where
//         T: AstObject,
//     {
//         lens_object(&*(Rc::clone(object) as Rc<dyn AstObject>), &predicate)
//     }
// }

// pub mod lowering {
//     pub trait Lower<Output> {
//         fn lower(&self) -> Output;

//         fn lower_and_then<F, T>(&self, f: F) -> T
//         where
//             F: Fn(&Self, Output) -> T,
//         {
//             f(self, self.lower())
//         }
//     }

//     pub trait LowerWith<Input, Output> {
//         fn lower_with(&self, i: Input) -> Output;

//         fn lower_with_and_then<F, T>(&self, i: Input, f: F) -> T
//         where
//             F: Fn(&Self, Output) -> T,
//         {
//             f(self, self.lower_with(i))
//         }
//     }
// }

// #[macro_export]
// macro_rules! isinstance {
//     ($e:expr, $t:path) => {{
//         if let Some($crate::ast::Spanned { inner, .. }) =
//             $crate::ast::_downcast_ref::<$crate::ast::Spanned<$t>>($e)
//         {
//             Some(inner)
//         } else {
//             $crate::ast::_downcast_ref::<$t>($e)
//         }
//     }};

//     ($e:expr, $t:ident, $( $pattern:pat )|+ $( if $guard: expr )? $(,)? => $a:expr) => {{
//         let result = crate::isinstance!($e, $t); // result: Some(ref $t)

//         match &result {
//             Some($( $pattern )|+ $( if $guard )?) => Some($a),
//             _ => None,
//         }
//     }};
// }
// use codespan_reporting::diagnostic::{Diagnostic, Label};

// pub trait CompilerError {
//     type Success;

//     fn unwrap_or_compiler_error<'a>(self, ctx: &LocalContext<'a>) -> Self::Success;
// }

// impl CompilerError for Option<LocalTypeId> {
//     type Success = LocalTypeId;

//     fn unwrap_or_compiler_error<'a>(self, ctx: &LocalContext<'a>) -> Self::Success {
//         match self {
//             Some(t) => t,
//             None => {
//                 ctx.exit_with_error(MontyError::UnknownType {
//                     node: ctx.this.as_ref().unwrap().clone(),
//                 });
//             }
//         }
//     }
// }

// impl<T> CompilerError for std::result::Result<T, MontyError> {
//     type Success = T;

//     fn unwrap_or_compiler_error<'b>(self, ctx: &LocalContext<'b>) -> Self::Success {
//         match self {
//             Ok(t) => t,
//             Err(err) => ctx.exit_with_error(err),
//         }
//     }
// }

// impl MontyError {
//     pub fn into_diagnostic(
//         self,
//         ctx: &LocalContext<'_>,
//     ) -> codespan_reporting::diagnostic::Diagnostic<u64> {
//         macro_rules! fmt_type {
//             ($t:expr) => {{
//                 $crate::fmt::Formattable {
//                     gctx: ctx.global_context,
//                     inner: $t,
//                 }
//             }};
//         }

//         match self {
//             MontyError::FunctionlessReturn { ret_span: (span, mref ) } => Diagnostic::error()
//                 .with_message("return outside of function")
//                 .with_labels(vec![
//                     Label::primary(mref.hash(), span).with_message("return can only be used within a function.")
//                 ]),

//             MontyError::BadReturnType {
//                 expected,
//                 actual,
//                 ret_node,
//                 def_node,
//             } => {
//                 let mut labels = vec![];

//                 let ret_span = ret_node.span().unwrap();

//                 let primary = match &ret_node.inner.value {
//                     Some(value) => Label::primary(ctx.module_ref.hash(), value.span().unwrap_or(ret_span))
//                         .with_message("but value of this type is returned instead."),

//                     None => Label::primary(ctx.module_ref.hash(), ret_span).with_message("`None` is returned here."),
//                 };

//                 labels.push(primary);

//                 let def = Label::secondary(
//                     ctx.module_ref.hash(),
//                     def_node
//                         .inner
//                         .returns
//                         .clone()
//                         .and_then(|ret| ret.span())
//                         .unwrap_or(def_node.span.clone()),
//                 )
//                 .with_message(if def_node.inner.returns.is_some() {
//                     format!(
//                         "function defined here supposedly returning a value of {}.",
//                         fmt_type!(expected)
//                     )
//                 } else {
//                     "function defined here is expected to return `None`".to_string()
//                 });

//                 labels.push(def);

//                 Diagnostic::error()
//                     .with_message("incomaptible return type for function.")
//                     .with_labels(labels)
//                     .with_notes(vec![
//                         format!("expected: {}", fmt_type!(expected)),
//                         format!("actual: {}", fmt_type!(actual)),
//                     ])
//             }

//             MontyError::MissingReturn {
//                 expected,
//                 def_span,
//                 ret_span,
//             } => Diagnostic::error()
//                 .with_message("missing return for annotated function.")
//                 .with_labels(vec![
//                     Label::primary(ctx.module_ref.hash(), def_span)
//                         .with_message("But will implicitly return `None` here."),

//                     Label::secondary(ctx.module_ref.hash(), ret_span)
//                         .with_message(                    format!(
//                             "Expected to return a value of {}.",
//                             fmt_type!(expected)
//                         )),
//                 ]),

//             MontyError::UnknownType { node } => Diagnostic::error()
//                 .with_message("unable to infer type for value.")
//                 .with_labels(vec![Label::primary(ctx.module_ref.hash(), node.span().unwrap())
//                     .with_message("annotate this name with a type")]),

//             MontyError::UndefinedVariable { node } => Diagnostic::error()
//                 .with_message("use of undefined variable.")
//                 .with_labels(vec![Label::primary(ctx.module_ref.hash(), node.span().unwrap()).with_message(
//                     format!(
//                         "\"{}\" is not defined.",
//                         ctx.global_context
//                             .resolver
//                             .sources
//                             .get(&ctx.module_ref)
//                             .unwrap()
//                             .value()
//                             .get(node.span().unwrap())
//                             .unwrap()
//                             .to_string()
//                     ),
//                 )]),

//             MontyError::BadArgumentType {
//                 expected,
//                 actual,
//                 arg_node,
//                 def_node,
//             } => Diagnostic::error()
//                 .with_message("incompatible argument type in function call.")
//                 .with_labels(vec![
//                     Label::primary(ctx.module_ref.hash(), arg_node.span.clone()).with_message(format!(
//                         "The argument here evaluates to \"{}\"",
//                         fmt_type!(actual)
//                     )),
//                     Label::secondary(ctx.module_ref.hash(), def_node.inner.name.span.clone()).with_message(format!(
//                         "Function defined here expected \"{}\"",
//                         fmt_type!(expected),
//                     )),
//                 ]),

//             MontyError::BadBinaryOp {
//                 span,
//                 left,
//                 right,
//                 op,
//             } => {
//                 let ty = &ctx.global_context.type_map;

//                 let left = ty.get(left).unwrap();
//                 let right = ty.get(right).unwrap();

//                 Diagnostic::error()
//                     .with_message("Unsupported binary expression.")
//                     .with_labels(vec![Label::primary(ctx.module_ref.hash(), span.clone()).with_message(
//                         format!(
//                             "Operator {:?} is not supported for types \"{}\" and \"{}\"",
//                             op.sigil(),
//                             ctx.as_formattable(left.value()),
//                             ctx.as_formattable(right.value())
//                         ),
//                     )])
//             }

//             MontyError::BadConditionalType { actual, span } => Diagnostic::error()
//                 .with_message("Incompatible type for conditional.")
//                 .with_labels(vec![Label::primary(ctx.module_ref.hash(), span).with_message(format!(
//                     "expected {}, found {}",
//                     fmt_type!(TypeMap::BOOL),
//                     fmt_type!(actual),
//                 ))]),

//             MontyError::IncompatibleTypes {
//                 left_span,
//                 left,
//                 right_span,
//                 right,
//             } => Diagnostic::error()
//                 .with_message("Incompatible types")
//                 .with_labels(vec![
//                     Label::primary(ctx.module_ref.hash(), right_span).with_message(format!(
//                         "expected {}, found {}",
//                         fmt_type!(left),
//                         fmt_type!(right)
//                     )),
//                     Label::secondary(ctx.module_ref.hash(), left_span)
//                         .with_message(format!("{}", fmt_type!(left))),
//                 ]),

//             MontyError::UnboundLocal { name, assign, used } => Diagnostic::error()
//                 .with_message(format!(
//                     "Local variable {:?} referenced before assignment",
//                     ctx.global_context
//                         .resolver
//                         .resolve(ctx.module_ref.clone(), name)
//                         .unwrap()
//                 ))
//                 .with_labels(vec![
//                     Label::primary(ctx.module_ref.hash(), used).with_message("name used here."),
//                     Label::secondary(ctx.module_ref.hash(), assign).with_message("but initially assigned later here."),
//                 ]),

//             MontyError::IncompatibleReassignment {
//                 name,
//                 first_assigned,
//                 incorrectly_reassigned,
//                 expected,
//                 actual,
//             } => Diagnostic::error()
//                 .with_message("Incompatible type for reassignment.")
//                 .with_labels(vec![
//                     Label::primary(ctx.module_ref.hash(), first_assigned).with_message(format!(
//                         "\"{}\" was first assigned here with type {}",
//                         ctx.global_context
//                             .resolver
//                             .resolve(ctx.module_ref.clone(), name)
//                             .unwrap(),
//                         fmt_type!(expected),
//                     )),
//                     Label::secondary(ctx.module_ref.hash(), incorrectly_reassigned).with_message(format!(
//                         "but gets incorrectly reassigned here with type {}",
//                         fmt_type!(actual)
//                     )),
//                 ])
//                 .with_notes(vec![
//                     "Note: Names within the same scope can not be assigned with multiple different types...".to_string(),
//                     format!("Help: Consider using a union type e.g. Union[{}, {}] instead.",
//                         fmt_type!(actual),
//                         fmt_type!(expected)
//                     )
//                 ]),

//             MontyError::NotCallable { kind, callsite } => Diagnostic::error().with_message("Object is not callable.").with_labels(vec![
//                 Label::primary(ctx.module_ref.hash(), callsite).with_message(format!("Attempted to call a non-callable object of type: {}", fmt_type!(kind))),
//             ]),

//             MontyError::Unsupported { span, message } => Diagnostic::error().with_message("Unsupported feature").with_labels(vec![Label::primary(ctx.module_ref.hash(), span).with_message(message)])
//         }
//     }
// }
