use std::path::PathBuf;

#[derive(Debug)]
pub struct ImportPath {
    base: PathBuf,
    curdir: PathBuf,
}

impl ImportPath {
    pub fn new(base: PathBuf) -> Self {
        Self {
            base: base.clone(),
            curdir: base,
        }
    }

    pub fn advance(&mut self, expected: impl AsRef<str>) -> Option<PathBuf> {
        let expected = expected.as_ref();

        log::trace!("import: {:?} ++ {:?}", self.curdir, expected);

        let next = self.curdir.read_dir().ok()?.find_map(|maybe_entry| {
            let entry = maybe_entry.ok()?;
            let file_type = entry.file_type().ok()?;
            let path = entry.path();

            let stem = path.file_stem()?.to_string_lossy();
            let wellformed_stem = !stem.contains(".");

            let ok = if file_type.is_file() {
                let has_py_ext = path.extension()?.to_string_lossy() == "py";

                has_py_ext && wellformed_stem && (stem == expected)
            } else if file_type.is_dir() {
                stem == expected
            } else {
                unreachable!();
            };

            ok.then_some(path)
        })?;

        if next.is_dir() {
            self.curdir = next.clone();
        }

        Some(next)
    }
}
