//! A request is used to create a session, to parse, type-check, consteval and compile Python code.
//!

use std::path::PathBuf;

/// A builder for creating a `Request` instance with various options.
///
/// A `Request` is used to create a session, to parse, type-check, consteval and compile Python code.
///
/// # Examples
///
/// ```
/// use montyc_driver::session_request::SessionRequestBuilder;
///
/// let request = SessionRequestBuilder::default()
///     .entry("__main__:main")
///     .libstd("libstd/".into())
///     .input("input.mt".into())
///     .output("output".into())
///     .build();
/// ```
#[derive(Debug, Default)]
pub struct SessionRequestBuilder {
    entry: Option<String>,
    libstd: Option<PathBuf>,
    input: Option<PathBuf>,
    output: Option<PathBuf>,
    show_ir: Option<String>,
    cc: Option<PathBuf>,
    ld: Option<PathBuf>,
    cranelift_settings: Option<Vec<String>>,
}

impl SessionRequestBuilder {
    /// Sets the path to the entry function.
    ///
    /// The default value is `__main__:main`.
    pub fn entry(mut self, entry: &str) -> Self {
        self.entry.replace(entry.to_owned());
        self
    }

    /// Sets the path to a monty compatible stdlib.
    ///
    /// The default value is `libstd/`.
    pub fn libstd(mut self, libstd: PathBuf) -> Self {
        self.libstd.replace(libstd);
        self
    }

    /// Sets the input file to compile.
    pub fn input(mut self, input: PathBuf) -> Self {
        self.input.replace(input);
        self
    }

    /// Sets the name of the output binary.
    ///
    /// The default value is the input file's name.
    pub fn output(mut self, output: PathBuf) -> Self {
        self.output.replace(output);
        self
    }

    /// Shows the Cranelift IR for the specified function.
    pub fn show_ir(mut self, show_ir: Option<String>) -> Self {
        self.show_ir = show_ir;
        self
    }

    /// Sets the C compiler to use.
    pub fn cc(mut self, cc: Option<PathBuf>) -> Self {
        self.cc = cc;
        self
    }

    /// Sets the linker to use.
    pub fn ld(mut self, ld: Option<PathBuf>) -> Self {
        self.ld = ld;
        self
    }

    /// Sets low level codegen settings to pass to Cranelift.
    pub fn cranelift_settings(mut self, cranelift_settings: Vec<String>) -> Self {
        self.cranelift_settings = Some(cranelift_settings);
        self
    }

    /// Builds a `Request` instance with the configured options.
    pub fn build(self) -> SessionRequest {
        SessionRequest {
            entry: self.entry.unwrap_or("__main__:main".to_string()),
            libstd: self.libstd.unwrap_or(PathBuf::from("libstd/")),
            input: self.input.unwrap_or(PathBuf::from("main.py")),
            output: self.output.unwrap_or(PathBuf::from("main")),
            show_ir: self.show_ir,
            cc: self.cc,
            ld: self.ld,
            cranelift_settings: self.cranelift_settings.unwrap_or_default(),
        }
    }
}

#[derive(Debug)]
pub struct SessionRequest {
    /// The path to the entry function, for instance `main` in a `__main__.py` would be `__main__:main` (and is the default.)
    pub entry: String,
    /// The path to a monty compatible stdlib.
    pub libstd: PathBuf,
    /// The input file to compile.
    pub input: PathBuf,
    // The name of the output binary, defaults to the input file's name.
    pub output: PathBuf,
    /// Show the Cranelift IR for the specified function.
    pub show_ir: Option<String>,
    /// The C compiler to use.
    pub cc: Option<PathBuf>,
    /// The linker to use.
    pub ld: Option<PathBuf>,
    /// Low level codegen settings to pass to Cranelift.
    pub cranelift_settings: Vec<String>,
}

impl SessionRequest {
    pub fn new() -> SessionRequestBuilder {
        SessionRequestBuilder::default()
    }
}
