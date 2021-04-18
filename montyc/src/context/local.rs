use std::{path::PathBuf, rc::Rc};

use codespan_reporting::diagnostic::Diagnostic;

use crate::{MontyError, ast::AstObject, parser::SpanEntry, scope::Scope};

use super::{ModuleRef, global::GlobalContext};


#[derive(Clone, Debug)]
pub struct LocalContext<'a> {
    pub global_context: &'a GlobalContext,
    pub module_ref: ModuleRef,
    pub scope: Rc<dyn Scope>,
    pub this: Option<Rc<dyn AstObject>>,
    pub parent: Option<&'a LocalContext<'a>>,
}

impl<'a> LocalContext<'a> {
    pub fn exit_with_error(&self, err: MontyError) -> ! {
        let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Auto,
        );
        let config = codespan_reporting::term::Config::default();

        let module_path: PathBuf = self.module_ref.clone().into();
        let file_name = module_path.file_name().unwrap().to_string_lossy();
        let file_source = self
            .global_context
            .modules
            .get(&self.module_ref)
            .expect("missing source!")
            .source
            .as_ref();

        let file = codespan_reporting::files::SimpleFile::new(file_name, file_source);

        let diagnostic = err.into_diagnostic(self);

        codespan_reporting::term::emit(&mut writer, &config, &file, &diagnostic);

        std::process::exit(1);
    }
}
