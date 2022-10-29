use std::path::PathBuf;

use montyc_vfs::VfsPath;
use thiserror::Error;

/// universal error type for `montyc_driver`
#[derive(Debug, Error)]
pub enum Error {}

type ParserError = montyc_parser::Error;

pub trait CompilerQueries {
    fn file_id_of_path(&self, path: &std::path::Path) -> Option<montyc_vfs::FileId>;

    fn raw_tokens_of_file(
        &self,
        file_id: montyc_vfs::FileId,
    ) -> Result<Vec<(montyc_lexer::PyToken, montyc_lexer::Span)>, crate::Error>;

    fn module_ast_of_file(
        &self,
        file_id: montyc_vfs::FileId,
    ) -> Result<
        (
            Option<montyc_ast::spanned::Spanned<montyc_ast::module::Module>>,
            Vec<ParserError>,
        ),
        crate::Error,
    >;
}

#[derive(Debug, Clone)]
pub struct CompilerOpts {}

impl CompilerOpts {
    pub fn new() -> Self {
        Self {}
    }

    pub fn session_builder<'a>(&'a self) -> CompilerSessionBuilder<'a> {
        CompilerSessionBuilder {
            opts: self,
            vfs: montyc_vfs::Vfs::new(),
        }
    }
}

pub struct CompilerSessionBuilder<'opts> {
    opts: &'opts CompilerOpts,
    vfs: montyc_vfs::Vfs,
}

impl<'opts> CompilerSessionBuilder<'opts> {
    #[inline]
    pub fn add_file_to_vfs(&mut self, path: PathBuf, file_contents: String) -> montyc_vfs::FileId {
        let path = VfsPath::new_filesystem_path(path);
        self.vfs.insert(path, file_contents)
    }

    pub fn build(self) -> CompilerSession {
        CompilerSession {
            opts: self.opts.clone(),
            vfs: self.vfs,
        }
    }
}

pub struct CompilerSession {
    opts: CompilerOpts,
    vfs: montyc_vfs::Vfs,
}

impl CompilerQueries for CompilerSession {
    fn file_id_of_path(&self, path: &std::path::Path) -> Option<montyc_vfs::FileId> {
        let path = VfsPath::new_filesystem_path(path.to_path_buf());
        self.vfs.file_id(&path)
    }

    fn raw_tokens_of_file(
        &self,
        file_id: montyc_vfs::FileId,
    ) -> Result<Vec<(montyc_lexer::PyToken, montyc_lexer::Span)>, crate::Error> {
        let file = self.vfs.get(file_id).unwrap();
        let tokens = montyc_lexer::tokens(file.contents());
        Ok(tokens)
    }

    fn module_ast_of_file(
        &self,
        file_id: montyc_vfs::FileId,
    ) -> Result<
        (
            Option<montyc_ast::spanned::Spanned<montyc_ast::module::Module>>,
            Vec<ParserError>,
        ),
        crate::Error,
    > {
        let span_interner = montyc_parser::span_interner::SpanInterner::new();
        let file = self.vfs.get(file_id).unwrap();

        let (out, errs) = montyc_parser::parse(&span_interner, file_id, file.contents());

        Ok((out, errs))
    }
}
