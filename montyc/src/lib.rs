#![deny(warnings)]
#![feature(
    variant_count,
    bool_to_option,
    drain_filter,
    assert_matches,
    box_syntax
)]

pub mod ast;
pub mod class;
pub mod codegen;
pub mod context;
pub mod fmt;
pub mod func;
pub mod layout;
pub(crate) mod ssamap;
pub mod import;

pub(crate) mod utils {
    use std::rc::Rc;

    use crate::{
        ast::expr::{Expr, InfixOp},
        context::LocalContext,
        prelude::{AstObject, LocalTypeId, TypedObject},
    };

    fn lens_object(
        o: &dyn AstObject,
        predicate: &dyn Fn(&dyn AstObject) -> bool,
    ) -> Option<Rc<dyn AstObject>> {
        for subnode in o.walk()? {
            if predicate(&*subnode) {
                return Some(Rc::clone(&subnode));
            }

            match lens_object(&*subnode, predicate) {
                s @ Some(_) => return s,
                None => continue,
            }
        }

        None
    }

    pub fn try_parse_union_literal(
        ctx: &LocalContext<'_>,
        e: &Expr,
        in_binop: bool,
    ) -> crate::Result<Option<Vec<LocalTypeId>>> {
        if let Expr::BinOp {
            left,
            right,
            op: InfixOp::Or,
        } = e
        {
            let l = ctx.with(left.clone(), |ctx, left| {
                try_parse_union_literal(&ctx, &left.inner, true)
            })?;
            let r = ctx.with(right.clone(), |ctx, right| {
                try_parse_union_literal(&ctx, &right.inner, true)
            })?;

            let mut v = l.unwrap_or_default();
            v.extend(r.unwrap_or_default());

            if v.is_empty() {
                Ok(None)
            } else {
                Ok(Some(v))
            }
        } else if in_binop {
            Ok(Some(vec![e
                .infer_type(ctx)?
                .canonicalize(&ctx.global_context.type_map)]))
        } else {
            Ok(None)
        }
    }

    pub fn lens<T>(
        object: &Rc<T>,
        predicate: impl Fn(&dyn AstObject) -> bool,
    ) -> Option<Rc<dyn AstObject>>
    where
        T: AstObject,
    {
        lens_object(&*(Rc::clone(object) as Rc<dyn AstObject>), &predicate)
    }
}

pub mod lowering {
    pub trait Lower<Output> {
        fn lower(&self) -> Output;

        fn lower_and_then<F, T>(&self, f: F) -> T
        where
            F: Fn(&Self, Output) -> T,
        {
            f(self, self.lower())
        }
    }

    pub trait LowerWith<Input, Output> {
        fn lower_with(&self, i: Input) -> Output;

        fn lower_with_and_then<F, T>(&self, i: Input, f: F) -> T
        where
            F: Fn(&Self, Output) -> T,
        {
            f(self, self.lower_with(i))
        }
    }
}

pub mod database;
pub mod error;
pub mod interpreter;
pub mod parser;
pub mod phantom;
pub mod scope;
pub mod typing;

pub type Result<T> = std::result::Result<T, error::MontyError>;

#[macro_export]
macro_rules! isinstance {
    ($e:expr, $t:path) => {{
        if let Some($crate::ast::Spanned { inner, .. }) =
            $crate::ast::_downcast_ref::<$crate::ast::Spanned<$t>>($e)
        {
            Some(inner)
        } else {
            $crate::ast::_downcast_ref::<$t>($e)
        }
    }};

    ($e:expr, $t:ident, $( $pattern:pat )|+ $( if $guard: expr )? $(,)? => $a:expr) => {{
        let result = crate::isinstance!($e, $t); // result: Some(ref $t)

        match &result {
            Some($( $pattern )|+ $( if $guard )?) => Some($a),
            _ => None,
        }
    }};
}

use std::path::PathBuf;

use structopt::*;

#[derive(Debug)]
pub struct VerifiedCompilerOptions(CompilerOptions);

#[derive(Debug, StructOpt, Clone, Default)]
pub struct CompilerOptions {
    /// The path to a monty compatible stdlib.
    #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
    pub libstd: PathBuf,

    /// The input file to compile.
    #[structopt(parse(from_os_str))]
    pub input: PathBuf,

    /// Show the Cranelift IR for the specified function.
    #[structopt(long)]
    pub show_ir: Option<String>,

    // The name of the output binary, defaults to the input file's name.
    #[structopt(parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// The C compiler to use.
    #[structopt(parse(from_os_str))]
    pub cc: Option<PathBuf>,

    /// The linker to use.
    #[structopt(parse(from_os_str))]
    pub ld: Option<PathBuf>,

    /// Low level codegen settings to pass to Cranelift.
    #[structopt(multiple = true, short = "C", long = "codegen")]
    pub cranelift_settings: Vec<String>,
}

impl CompilerOptions {
    fn check_if_path_exists(
        &self,
        path: &std::path::Path,
        field_name: &str,
    ) -> std::result::Result<PathBuf, String> {
        match path.canonicalize() {
            Ok(path) if !path.exists() => Err(format!(
                "Provided path to {} does not exist. (path={:?})",
                field_name, path
            )),
            Err(err) => Err(format!(
                "Failed to get the absolute path of {}. (path={:?}, error={:?})",
                field_name,
                path,
                err.kind()
            )),
            Ok(path) => Ok(path), // provided path exists.
        }
    }

    pub fn codegen_settings(&self) -> Box<dyn cranelift_codegen::isa::TargetIsa> {
        use cranelift_codegen::{
            isa,
            settings::{self, Configurable},
        };

        let mut flags_builder = cranelift_codegen::settings::builder();

        let default_settings = vec!["opt_level=none", "is_pic=yes", "enable_verifier=yes"];

        let default_settings = default_settings.iter().map(ToString::to_string);

        for setting in default_settings.chain(self.cranelift_settings.iter().cloned()) {
            let mut it = setting.split("=").take(2);

            let (name, value) = (it.next().unwrap(), it.next().unwrap());

            flags_builder.set(name, value).unwrap();
        }

        let flags = settings::Flags::new(flags_builder);

        isa::lookup(target_lexicon::Triple::host())
            .unwrap()
            .finish(flags)
    }

    pub fn verify(self) -> VerifiedCompilerOptions {
        let mut errors = vec![];

        if let Err(st) = self.check_if_path_exists(&self.libstd, "the standard library") {
            errors.push(st);
        }

        if let Err(st) = self.check_if_path_exists(&self.input, "the provided input file") {
            errors.push(st);
        }

        if let Some(cc) = &self.cc {
            if let Err(st) = self.check_if_path_exists(&cc, "the specified C compiler") {
                errors.push(st);
            }
        }

        if let Some(ld) = &self.ld {
            if let Err(st) = self.check_if_path_exists(&ld, "the specified linker") {
                errors.push(st);
            }
        }

        if !errors.is_empty() {
            for err in errors.drain(..) {
                eprintln!("error: {}", err);
            }

            std::process::exit(1);
        } else {
            VerifiedCompilerOptions(self)
        }
    }
}

pub mod prelude {
    pub use crate::{
        ast::{AstObject, Spanned},
        codegen::CodegenModule,
        context::{GlobalContext, LocalContext, ModuleContext, ModuleRef},
        error::{CompilerError, MontyError},
        func::{DataRef, Function},
        layout::{BlockId, Layout},
        lowering::{Lower, LowerWith},
        parser::{token::PyToken, Parseable, ParserT, Span, SpanRef},
        scope::{ChainedScope, LocalScope, LookupTarget, OpaqueScope, Scope, ScopeRoot},
        typing::{
            ClassType, FunctionType, Generic, LocalTypeId, TaggedType, TypeDescriptor, TypeMap,
            TypedObject,
        },
    };
}
