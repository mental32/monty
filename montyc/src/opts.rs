use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::result::Result;

use cranelift_codegen::isa::TargetIsa;
use structopt::*;

#[derive(Debug)]
pub struct VerifiedCompilerOptions(pub(crate) CompilerOptions);

impl Deref for VerifiedCompilerOptions {
    type Target = CompilerOptions;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, StructOpt, Clone)]
pub enum CompilerOptions {
    // `libstd` and `input` are duplicated because I can't figure out how to
    // have them shared between subcommands.
    //
    /// Typecheck the provided input but do not build anything.
    Check {
        /// The path to a monty compatible stdlib.
        #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
        libstd: PathBuf,

        /// The input file to check.
        #[structopt(parse(from_os_str))]
        input: PathBuf,
    },

    /// Compile the provided input.
    Build {
        /// The path to the entry function, for instance `main` in a `__main__.py` would be `__main__:main` (and is the default.)
        #[structopt(short, long, default_value = "__main__:main")]
        entry: String,

        /// The path to a monty compatible stdlib.
        #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
        libstd: PathBuf,

        /// The input file to compile.
        #[structopt(parse(from_os_str))]
        input: PathBuf,

        // The name of the output binary, defaults to the input file's name.
        #[structopt(parse(from_os_str))]
        output: PathBuf,

        /// Show the Cranelift IR for the specified function.
        #[structopt(long)]
        show_ir: Option<String>,

        /// The C compiler to use.
        #[structopt(parse(from_os_str))]
        cc: Option<PathBuf>,

        /// The linker to use.
        #[structopt(parse(from_os_str))]
        ld: Option<PathBuf>,

        /// Low level codegen settings to pass to Cranelift.
        #[structopt(multiple = true, short = "C", long = "codegen")]
        cranelift_settings: Vec<String>,
    },
}

impl CompilerOptions {
    fn check_if_path_exists(path: &Path, field_name: &str) -> Result<PathBuf, String> {
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

    pub fn codegen_settings(&self) -> Box<dyn TargetIsa> {
        use cranelift_codegen::{
            isa,
            settings::{self, Configurable},
        };

        let settings = match self {
            Self::Build {
                cranelift_settings, ..
            } => cranelift_settings,
            _ => unreachable!("Refusing to generate codegen settings for a non-build option."),
        };

        let mut flags_builder = cranelift_codegen::settings::builder();

        let default_settings = vec!["opt_level=none", "is_pic=yes", "enable_verifier=yes"];

        let default_settings = default_settings.iter().map(ToString::to_string);

        for setting in default_settings.chain(settings.iter().cloned()) {
            let mut it = setting.split("=").take(2);

            let (name, value) = (it.next().unwrap(), it.next().unwrap());

            flags_builder.set(name, value).unwrap();
        }

        let flags = settings::Flags::new(flags_builder);

        isa::lookup(target_lexicon::Triple::host())
            .unwrap()
            .finish(flags)
    }

    pub fn libstd(&self) -> PathBuf {
        match self {
            CompilerOptions::Check { libstd, .. } | CompilerOptions::Build { libstd, .. } => {
                libstd.clone()
            }
        }
    }

    pub fn verify(mut self) -> VerifiedCompilerOptions {
        let mut errors = vec![];

        let (libstd, input) = match &mut self {
            CompilerOptions::Check { libstd, input }
            | CompilerOptions::Build { libstd, input, .. } => (libstd, input),
        };

        match Self::check_if_path_exists(&libstd, "the standard library") {
            Err(st) => {
                errors.push(st);
            }

            Ok(path) => (*libstd) = path,
        }

        if let Err(st) = Self::check_if_path_exists(&input, "the provided input file") {
            errors.push(st);
        }

        if let CompilerOptions::Build { cc, ld, .. } = &self {
            if let Some(cc) = cc {
                if let Err(st) = Self::check_if_path_exists(&cc, "the specified C compiler") {
                    errors.push(st);
                }
            }

            if let Some(ld) = ld {
                if let Err(st) = Self::check_if_path_exists(&ld, "the specified linker") {
                    errors.push(st);
                }
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
