use std::{path::PathBuf, rc::Rc};

use crate::{MontyError, ast::{AstObject, Spanned}, class::Class, fmt::Formattable, scope::Scope, typing::{LocalTypeId, TypeDescriptor}};

use super::{global::GlobalContext, ModuleRef};

#[derive(Clone, Debug)]
pub struct LocalContext<'a> {
    pub global_context: &'a GlobalContext,
    pub module_ref: ModuleRef,
    pub scope: Rc<dyn Scope>,
    pub this: Option<Rc<dyn AstObject>>,
}

impl<'a> LocalContext<'a> {
    pub fn try_get_class_of_type(&self, type_id: LocalTypeId) -> Option<Rc<Class>> {
        match self.global_context.type_map.get(type_id).unwrap().value() {
            TypeDescriptor::Simple(_) => Some(self.global_context.builtins.get(&type_id)?.0.clone()),

            TypeDescriptor::Class(klass) => self
                .global_context
                .get_class_from_module(klass.mref.clone(), klass.name),

            TypeDescriptor::Generic(_) | TypeDescriptor::Function(_) => todo!(),
        }
    }

    /// Associate a span of the current module with a type used for later codegen.
    pub fn cache_type<T>(&self, t: &Spanned<T>, type_id: LocalTypeId)
    where
        T: AstObject + Clone + ?Sized,
    {
        self.global_context
            .type_map
            .cache
            .insert((self.module_ref.clone(), t.span.clone()), type_id);
    }

    /// Invoke `f` with a context which sets `ctx.this` to `object`.
    pub fn with<T>(
        &self,
        object: Rc<impl AstObject>,
        f: impl Fn(Self, Rc<dyn AstObject>) -> T,
    ) -> T {
        let mut object_context = self.clone();

        object_context.this = Some(object.clone());

        f(object_context, object)
    }

    pub fn as_formattable<T>(&self, t: T) -> Formattable<'_, T> {
        Formattable { gctx: self.global_context, inner: t }
    }

    /// Format and emit the error to stderr then call [std::process::exit]
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

        let _ = codespan_reporting::term::emit(&mut writer, &config, &file, &diagnostic);

        std::process::exit(1);
    }
}
