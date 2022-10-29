//! # Virtual File System ([`Vfs`])
//!
//! The virtual file system type (VFS) is a container for files that
//! have been read into memory and are made indexable with [`FileId`]s
//!
//! VFS does not do IO or file watching it simply acts as the container
//! to be used after reading files to memory. Additionally VFS also acts
//! as a path interner for [`std::path::Path`] / [`std::path::PathBuf`] types.
//!
//! All interned files will get a unique associating identifier [`FileId`] that
//! can be used to retrieve the file contents.
//!
//! Astute readers will notice the design of [`Vfs`] and [`VfsPath`] closely
//! resembles the implementation from rust-analyzer; this is not by accident
//! but actually it is where I got the idea from :)
//!

use std::path::PathBuf;
use std::sync::Arc;

/// A, unique, file identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
enum VfsPathRepr {
    VirtualPath(String),
    FilesystemPath(PathBuf),
}

/// An abstraction over paths in [`Vfs`]
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VfsPath(VfsPathRepr);

impl VfsPath {
    #[inline]
    pub fn new_virtual_path(st: String) -> Self {
        Self(VfsPathRepr::VirtualPath(st))
    }

    #[inline]
    pub fn new_filesystem_path(path: PathBuf) -> Self {
        Self(VfsPathRepr::FilesystemPath(path))
    }
}

/// A file-contents and file-path interner.
pub struct Vfs {
    file_id_ticker: u32,
    file_map: ahash::AHashMap<FileId, VfsFile>,
    path_map: ahash::AHashMap<VfsPath, FileId>,
}

#[derive(Clone)]
pub struct VfsFile {
    path: VfsPath,
    file_contents: Arc<str>,
}

impl VfsFile {
    #[inline]
    pub fn path(&self) -> &VfsPath {
        &self.path
    }

    #[inline]
    pub fn contents(&self) -> &str {
        &self.file_contents
    }
}

impl Vfs {
    #[inline]
    fn tick(&mut self) -> u32 {
        let cur = self.file_id_ticker;
        self.file_id_ticker += 1;
        cur
    }
}

impl Vfs {
    #[inline]
    pub fn new() -> Self {
        Self {
            file_id_ticker: 0,
            file_map: ahash::AHashMap::new(),
            path_map: ahash::AHashMap::new(),
        }
    }

    #[inline]
    pub fn insert(&mut self, path: VfsPath, file_contents: String) -> FileId {
        if let Some(file_id) = self.path_map.get(&path) {
            return *file_id;
        }

        let file_id = FileId(self.tick());

        self.path_map.insert(path.clone(), file_id);

        let vfs_file = VfsFile {
            path,
            file_contents: Arc::from(file_contents),
        };

        self.file_map.insert(file_id, vfs_file);

        file_id
    }

    #[inline]
    pub fn file_id(&self, path: &VfsPath) -> Option<FileId> {
        self.path_map.get(path).cloned()
    }

    #[inline]
    pub fn get(&self, file_id: FileId) -> Option<&VfsFile> {
        self.file_map.get(&file_id)
    }
}

impl Default for Vfs {
    fn default() -> Self {
        Self::new()
    }
}
