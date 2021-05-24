use std::rc::Rc;

use crate::{
    ast::AstObject,
    class::Class,
    database::DefId,
    error::MontyError,
    fmt::Formattable,
    scope::Scope,
    typing::{LocalTypeId, TypeDescriptor},
};

use super::{global::GlobalContext, ModuleRef};

mod files {
    use std::{collections::HashMap, ops::Range, rc::Rc};

    use crate::context::ModuleRef;

    /// A file database that contains a single source file.
    ///
    /// Because there is only single file in this database we use `()` as a [`FileId`].
    ///
    /// This is useful for simple language tests, but it might be worth creating a
    /// custom implementation when a language scales beyond a certain size.
    ///
    /// [`FileId`]: Files::FileId
    #[derive(Debug, Clone)]
    pub struct SimpleFile<Name, Source> {
        /// The name of the file.
        name: Name,
        /// The source code of the file.
        source: Source,
        /// The starting byte indices in the source code.
        line_starts: Vec<usize>,
    }

    use codespan_reporting::files::{Error, Files};

    impl<Name, Source> SimpleFile<Name, Source>
    where
        Name: std::fmt::Display,
        Source: AsRef<str>,
    {
        /// Create a new source file.
        pub fn new(name: Name, source: Source) -> SimpleFile<Name, Source> {
            SimpleFile {
                name,
                line_starts: codespan_reporting::files::line_starts(source.as_ref()).collect(),
                source,
            }
        }

        /// Return the source of the file.
        pub fn source(&self) -> &Source {
            &self.source
        }

        /// Return the starting byte index of the line with the specified line index.
        /// Convenience method that already generates errors if necessary.
        fn line_start(&self, line_index: usize) -> Result<usize, Error> {
            use std::cmp::Ordering;

            match line_index.cmp(&self.line_starts.len()) {
                Ordering::Less => Ok(self
                    .line_starts
                    .get(line_index)
                    .cloned()
                    .expect("failed despite previous check")),
                Ordering::Equal => Ok(self.source.as_ref().len()),
                Ordering::Greater => Err(Error::LineTooLarge {
                    given: line_index,
                    max: self.line_starts.len() - 1,
                }),
            }
        }
    }

    impl<'a, Name, Source> Files<'a> for SimpleFile<Name, Source>
    where
        Name: 'a + std::fmt::Display + Clone,
        Source: 'a + AsRef<str>,
    {
        type FileId = u64;
        type Name = Name;
        type Source = &'a str;

        fn name(&self, _: Self::FileId) -> Result<Name, Error> {
            Ok(self.name.clone())
        }

        fn source(&self, _: Self::FileId) -> Result<&str, Error> {
            Ok(self.source.as_ref())
        }

        fn line_index(&self, _: Self::FileId, byte_index: usize) -> Result<usize, Error> {
            Ok(self
                .line_starts
                .binary_search(&byte_index)
                .unwrap_or_else(|next_line| next_line - 1))
        }

        fn line_range(&self, _: Self::FileId, line_index: usize) -> Result<Range<usize>, Error> {
            let line_start = self.line_start(line_index)?;
            let next_line_start = self.line_start(line_index + 1)?;

            Ok(line_start..next_line_start)
        }
    }

    /// A file database that can store multiple source files.
    ///
    /// This is useful for simple language tests, but it might be worth creating a
    /// custom implementation when a language scales beyond a certain size.
    /// It is a glorified `Vec<SimpleFile>` that implements the `Files` trait.
    #[derive(Debug, Clone)]
    pub struct SimpleFiles {
        files: HashMap<u64, SimpleFile<u64, Rc<str>>>,
        names: HashMap<u64, ModuleRef>,
    }

    impl SimpleFiles {
        pub fn new() -> SimpleFiles {
            SimpleFiles {
                files: HashMap::new(),
                names: HashMap::new(),
            }
        }

        pub fn add(&mut self, name: ModuleRef, source: Rc<str>) -> usize {
            let file_id = self.files.len();
            let hash = name.hash();
            self.files.insert(hash, SimpleFile::new(hash, source));
            self.names.insert(hash, name);
            file_id
        }

        pub fn get(&self, file_id: u64) -> Result<&SimpleFile<u64, Rc<str>>, Error> {
            self.files.get(&file_id).ok_or(Error::FileMissing)
        }

        pub fn get_name(&self, file_id: u64) -> Option<ModuleRef> {
            self.names.get(&file_id).cloned()
        }
    }

    impl<'a> Files<'a> for SimpleFiles {
        type FileId = u64;
        type Name = ModuleRef;
        type Source = &'a str;

        fn name(&self, file_id: Self::FileId) -> Result<Self::Name, Error> {
            self.get_name(file_id).ok_or(Error::FileMissing)
        }

        fn source(&self, file_id: Self::FileId) -> Result<&str, Error> {
            Ok(self.get(file_id)?.source().as_ref())
        }

        fn line_index(&self, file_id: Self::FileId, byte_index: usize) -> Result<usize, Error> {
            self.get(file_id)?.line_index(file_id, byte_index)
        }

        fn line_range(
            &self,
            file_id: Self::FileId,
            line_index: usize,
        ) -> Result<Range<usize>, Error> {
            self.get(file_id)?.line_range(file_id, line_index)
        }
    }
}

#[derive(Clone, Debug)]
pub struct LocalContext<'a> {
    pub global_context: &'a GlobalContext,
    pub module_ref: ModuleRef,
    pub scope: Rc<dyn Scope>,
    pub this: Option<Rc<dyn AstObject>>,
    pub current_branch: Option<DefId>,
}

impl<'a> LocalContext<'a> {
    pub fn try_get_class_of_type(&self, type_id: LocalTypeId) -> Option<Rc<Class>> {
        match self.global_context.type_map.get(type_id).unwrap().value() {
            TypeDescriptor::Simple(_) => {
                Some(self.global_context.builtins.get(&type_id)?.0.clone())
            }

            TypeDescriptor::Class(klass) => self
                .global_context
                .get_class_from_module(klass.mref.clone(), klass.name),

            TypeDescriptor::Instance(_)
            | TypeDescriptor::Generic(_)
            | TypeDescriptor::Function(_) => todo!(),
        }
    }

    /// Associate a span of the current module with a type used for later codegen.
    pub fn cache_type(&self, t: &Rc<dyn AstObject>, type_id: LocalTypeId) {
        log::trace!("context:cache_type caching object {:?}", t);

        let entry = self
            .global_context
            .database
            .entry(Rc::clone(&t), &self.module_ref);

        let def_id = self.global_context.database.id_of(&entry).unwrap();

        let _ = self.global_context.database.set_type_of(def_id, type_id);
    }

    /// Invoke `f` with a context which sets `ctx.this` to `object`.
    pub fn with<T, A>(&self, object: Rc<A>, f: impl Fn(Self, Rc<A>) -> T) -> T
    where
        A: AstObject,
    {
        let mut object_context = self.clone();

        object_context.this = Some(Rc::clone(&object) as Rc<_>);

        f(object_context, object)
    }

    pub fn as_formattable<T>(&self, t: T) -> Formattable<'_, T> {
        Formattable {
            gctx: self.global_context,
            inner: t,
        }
    }

    /// Format and emit the error to stderr then call [std::process::exit]
    pub fn exit_with_error(&self, err: MontyError) -> ! {
        let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Auto,
        );
        let config = codespan_reporting::term::Config::default();

        let file_source = self
            .global_context
            .resolver
            .sources
            .get(&self.module_ref)
            .expect("missing source!")
            .value()
            .clone();

        let mut files = files::SimpleFiles::new();

        files.add(self.module_ref.clone(), file_source);

        let diagnostic = err.into_diagnostic(self);

        let _ = codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic);

        std::process::exit(1);
    }
}
