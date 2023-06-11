use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::result::Result;

use clap::Parser;

#[derive(Debug)]
pub struct VerifiedCompilerOptions(pub CompilerOptions);

impl Deref for VerifiedCompilerOptions {
    type Target = CompilerOptions;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Parser, Clone)]
pub enum CompilerOptions {
    // `libstd` and `input` are duplicated because I can't figure out how to
    // have them shared between subcommands.
    //
    /// Typecheck the provided input but do not build anything.
    Check {
        /// The path to a monty compatible stdlib.
        #[clap(short, long, default_value = "libstd/")]
        libstd: PathBuf,

        /// The input file to check.
        input: PathBuf,
    },

    /// Compile the provided input.
    Build {
        /// The path to the entry function, for instance `main` in a `__main__.py` would be `__main__:main` (and is the default.)
        #[clap(short, long, default_value = "__main__:main")]
        entry: String,

        /// The path to a monty compatible stdlib.
        #[clap(short, long, default_value = "libstd/")]
        libstd: PathBuf,

        /// The input file to compile.
        input: PathBuf,

        // The name of the output binary, defaults to the input file's name.
        output: PathBuf,

        /// Show the Cranelift IR for the specified function.
        #[clap(long)]
        show_ir: Option<String>,

        /// The C compiler to use.
        #[clap(short, long)]
        cc: Option<PathBuf>,

        /// The linker to use.
        ld: Option<PathBuf>,

        /// Low level codegen settings to pass to Cranelift.
        #[clap(short = 'C', long = "codegen")]
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
    pub fn input(&self) -> PathBuf {
        match self {
            CompilerOptions::Check { input, .. } | CompilerOptions::Build { input, .. } => {
                input.clone()
            }
        }
    }

    pub fn libstd(&self) -> PathBuf {
        match self {
            CompilerOptions::Check { libstd, .. } | CompilerOptions::Build { libstd, .. } => {
                libstd.clone()
            }
        }
    }

    pub fn verify(mut self) -> Result<VerifiedCompilerOptions, Vec<String>> {
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

        if let Err(st) =
            Self::check_if_path_exists(&*libstd.join("builtins.py"), "the builtins module")
        {
            errors.push(st);
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
            Err(errors)
        } else {
            Ok(VerifiedCompilerOptions(self))
        }
    }
}
