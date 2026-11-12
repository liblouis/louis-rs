//! Find tables by the `#+key: value` metadata they carry, instead of by file name.

use std::{
    collections::{HashMap, HashSet},
    fmt, fs,
    path::{Path, PathBuf},
};

/// Why an [`Index`] could not be built or a [`Query`] parsed.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum MetaDataError {
    /// A directory on the search path could not be read
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    /// A part of the query is not a `key:value` pair
    #[error("{0:?} is not a key:value pair")]
    InvalidQuery(String),
}

use search_path::SearchPath;

/// A query into the metadata, i.e. a list of `key:value` pairs
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query(Vec<(String, String)>);

impl Query {
    /// Parse a query in liblouis's syntax, `key:value` pairs separated by whitespace, such
    /// as `language:de grade:2`.
    pub fn parse(query: &str) -> Result<Self, MetaDataError> {
        query
            .split_whitespace()
            .map(|pair| match pair.split_once(':') {
                Some((k, v)) if !k.is_empty() && !v.is_empty() => {
                    Ok((k.to_string(), v.to_string()))
                }
                _ => Err(MetaDataError::InvalidQuery(pair.to_string())),
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Self)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<&HashMap<String, String>> for Query {
    fn from(query: &HashMap<String, String>) -> Self {
        Self(query.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
    }
}

impl fmt::Display for Query {
    /// Sorted, so that an error message naming a query does not depend on the order
    /// the pairs happened to be collected in.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pairs: Vec<_> = self.0.iter().map(|(k, v)| format!("{k}:{v}")).collect();
        pairs.sort();
        write!(f, "{}", pairs.join(" "))
    }
}

/// Maps each `#+key: value` metadata pair found on the search path to the tables
/// that carry it.
#[derive(Debug)]
pub struct Index(HashMap<(String, String), HashSet<PathBuf>>);

impl Index {
    /// Index the tables in the `LOUIS_TABLE_PATH` search path. Use [`Self::with_search_path`]
    /// to supply the directories instead.
    ///
    /// # Errors
    ///
    /// If a directory on the search path cannot be read.
    pub fn new() -> Result<Self, MetaDataError> {
        Self::build(&SearchPath::new_or("LOUIS_TABLE_PATH", "."))
    }

    /// Like [`Self::new`], but indexing the tables in `search_path` instead.
    ///
    /// # Errors
    ///
    /// If a directory on the search path cannot be read.
    pub fn with_search_path(search_path: Vec<PathBuf>) -> Result<Self, MetaDataError> {
        Self::build(&SearchPath::from(search_path))
    }

    /// The tables matching the query, best match first, like liblouis's `lou_findTables`.
    ///
    /// Today only a table carrying every one of the query's pairs matches, and the matches
    /// come sorted by path. liblouis also ranks partial matches; that is still to come.
    pub fn find(&self, query: Query) -> Vec<&Path> {
        if query.is_empty() {
            return Vec::new();
        }
        let candidates: Vec<_> = query
            .0
            .into_iter()
            .map(|(k, v)| self.0.get(&(k, v)))
            .collect();
        // if any of the queries was not found in the index then the whole query is empty
        if candidates.iter().any(|c| c.is_none()) {
            Vec::new()
        } else {
            let candidates: Vec<_> = candidates.iter().flatten().collect();
            // get the intersection of all sets
            let mut found: Vec<&Path> = candidates[0]
                .iter()
                .filter(|c| candidates[1..].iter().all(|s| s.contains(*c)))
                .map(PathBuf::as_path)
                .collect();
            found.sort();
            found
        }
    }

    /// Derive the index by walking the whole of `search_path` and reading the
    /// metadata out of every table on it.
    fn build(search_path: &SearchPath) -> Result<Self, MetaDataError> {
        let mut index: HashMap<(String, String), HashSet<PathBuf>> = HashMap::new();

        for dir in search_path.iter() {
            for entry in fs::read_dir(dir)? {
                let path = entry?.path();

                if let Ok(content) = fs::read_to_string(&path) {
                    for line in content.lines() {
                        if let Some(line) = line.strip_prefix("#+")
                            && let Some((key, value)) = line.split_once(':')
                        {
                            let k = key.trim();
                            let v = value.trim();
                            if !k.is_empty()
                                && !v.is_empty()
                                && k.chars().all(|c| c.is_ascii_alphanumeric())
                                && v.chars().all(|c| c.is_ascii_graphic())
                            {
                                index
                                    .entry((k.into(), v.into()))
                                    .or_default()
                                    .insert(path.clone());
                            }
                        }
                    }
                }
            }
        }
        Ok(Self(index))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_returns_the_tables_carrying_every_pair_sorted() {
        let dir = std::env::temp_dir().join(format!("louis-metadata-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("b.utb"), "#+language: de\n#+grade: 2\n").unwrap();
        fs::write(dir.join("a.utb"), "#+language: de\n#+grade: 2\n").unwrap();
        fs::write(dir.join("c.utb"), "#+language: de\n#+grade: 1\n").unwrap();
        let index = Index::with_search_path(vec![dir.clone()]).unwrap();
        let found = index.find(Query::parse("language:de grade:2").unwrap());
        assert_eq!(found, [dir.join("a.utb"), dir.join("b.utb")]);
        assert!(index.find(Query::parse("language:fr").unwrap()).is_empty());
        assert!(Query::parse("language=de").is_err());
        assert!(Query::parse("language:").is_err());
        fs::remove_dir_all(&dir).unwrap();
    }
}
