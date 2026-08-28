//! Locating table files by name.

use std::env;
use std::ffi::OsStr;
use std::fmt;
use std::path::{Path, PathBuf};

/// Locates the file for a table name.
///
/// `table` is the name given to the translator or written on an `include`
/// line, usually a bare file name. `base` is the path of the including table
/// when resolving an `include`, `None` for top-level tables.
pub trait TableResolver: Send + Sync + fmt::Debug {
    fn resolve(&self, table: &Path, base: Option<&Path>) -> Option<PathBuf>;
}

/// Resolves table names against an explicit list of directories.
///
/// Tries, in order: the name as-is when it is an absolute path, the directory
/// of the including table (`base`), then each listed directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SearchDirs {
    dirs: Vec<PathBuf>,
}

impl SearchDirs {
    pub fn new<I, P>(dirs: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        Self {
            dirs: dirs.into_iter().map(Into::into).collect(),
        }
    }

    /// The directories listed in `LOUIS_TABLE_PATH` (split like `PATH`, empty
    /// entries dropped), or just the current directory when the variable is
    /// unset.
    pub fn from_env() -> Self {
        Self {
            dirs: dirs_from_env_value(env::var_os("LOUIS_TABLE_PATH").as_deref()),
        }
    }

    pub fn dirs(&self) -> &[PathBuf] {
        &self.dirs
    }
}

fn dirs_from_env_value(value: Option<&OsStr>) -> Vec<PathBuf> {
    match value {
        None => vec![PathBuf::from(".")],
        Some(value) => env::split_paths(value)
            .filter(|dir| !dir.as_os_str().is_empty())
            .collect(),
    }
}

impl TableResolver for SearchDirs {
    fn resolve(&self, table: &Path, base: Option<&Path>) -> Option<PathBuf> {
        if table.is_absolute() {
            return table.is_file().then(|| table.to_path_buf());
        }
        base.and_then(Path::parent)
            .map(|dir| dir.join(table))
            .filter(|path| path.is_file())
            .or_else(|| {
                self.dirs
                    .iter()
                    .map(|dir| dir.join(table))
                    .find(|path| path.is_file())
            })
    }
}

impl<T: TableResolver + ?Sized> TableResolver for std::sync::Arc<T> {
    fn resolve(&self, table: &Path, base: Option<&Path>) -> Option<PathBuf> {
        (**self).resolve(table, base)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsStr;
    use std::fs;
    use std::path::{Path, PathBuf};

    /// Fresh scratch directory per test, so tests never share state or touch
    /// `LOUIS_TABLE_PATH`.
    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("louis-rs-resolver-test-{name}"));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn touch(dir: &Path, name: &str) -> PathBuf {
        let path = dir.join(name);
        fs::write(&path, "").unwrap();
        path
    }

    #[test]
    fn absolute_path_resolves_with_empty_dirs() {
        let dir = scratch("absolute");
        let table = touch(&dir, "a.utb");
        let resolver = SearchDirs::new(Vec::<PathBuf>::new());
        assert_eq!(resolver.resolve(&table, None), Some(table.clone()));
    }

    #[test]
    fn absolute_missing_path_is_none() {
        let dir = scratch("absolute-missing");
        let resolver = SearchDirs::new([dir.clone()]);
        assert_eq!(resolver.resolve(&dir.join("missing.utb"), None), None);
    }

    #[test]
    fn name_next_to_base_wins_over_listed_dir() {
        let base_dir = scratch("base-wins-base");
        let listed = scratch("base-wins-listed");
        let next_to_base = touch(&base_dir, "sub.utb");
        touch(&listed, "sub.utb");
        let base = touch(&base_dir, "top.utb");
        let resolver = SearchDirs::new([listed]);
        assert_eq!(
            resolver.resolve(Path::new("sub.utb"), Some(base.as_path())),
            Some(next_to_base)
        );
    }

    #[test]
    fn listed_dirs_tried_in_order() {
        let first = scratch("order-first");
        let second = scratch("order-second");
        let in_first = touch(&first, "both.utb");
        touch(&second, "both.utb");
        let only_in_second = touch(&second, "second.utb");
        let resolver = SearchDirs::new([first, second]);
        assert_eq!(
            resolver.resolve(Path::new("both.utb"), None),
            Some(in_first)
        );
        assert_eq!(
            resolver.resolve(Path::new("second.utb"), None),
            Some(only_in_second)
        );
    }

    #[test]
    fn missing_name_is_none() {
        let dir = scratch("missing");
        let resolver = SearchDirs::new([dir]);
        assert_eq!(resolver.resolve(Path::new("missing.utb"), None), None);
    }

    #[test]
    fn cwd_not_consulted_without_explicit_dot() {
        // `cargo test` runs with the crate root as cwd, where Cargo.toml exists.
        let dir = scratch("no-cwd");
        let resolver = SearchDirs::new([dir]);
        assert_eq!(resolver.resolve(Path::new("Cargo.toml"), None), None);
        let with_dot = SearchDirs::new(["."]);
        assert_eq!(
            with_dot.resolve(Path::new("Cargo.toml"), None),
            Some(Path::new(".").join("Cargo.toml"))
        );
    }

    #[test]
    fn unset_env_falls_back_to_cwd() {
        assert_eq!(dirs_from_env_value(None), vec![PathBuf::from(".")]);
    }

    #[test]
    fn empty_env_yields_no_dirs() {
        assert_eq!(
            dirs_from_env_value(Some(OsStr::new(""))),
            Vec::<PathBuf>::new()
        );
    }

    #[test]
    fn env_entries_split_on_platform_separator_dropping_empty_ones() {
        let value = std::env::join_paths(["a", "", "b"]).unwrap();
        assert_eq!(
            dirs_from_env_value(Some(&value)),
            vec![PathBuf::from("a"), PathBuf::from("b")]
        );
    }

    #[test]
    fn dirs_accessor_returns_list_in_order() {
        assert_eq!(
            SearchDirs::new(["a", "b"]).dirs(),
            [PathBuf::from("a"), PathBuf::from("b")]
        );
    }
}
