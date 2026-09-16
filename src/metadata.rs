use std::{
    collections::{HashMap, HashSet},
    fmt, fs,
    path::PathBuf,
    sync::LazyLock,
};

#[derive(thiserror::Error, Debug)]
pub enum MetaDataError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("{0:?} is not a key=value pair")]
    InvalidQuery(String),
}

use search_path::SearchPath;

/// A query into the metadata, i.e. a set of `key=value` pairs
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query(Vec<(String, String)>);

impl Query {
    /// Parse the `key=value,key=value` form the `query` subcommand takes.
    pub fn parse(query: &str) -> Result<Self, MetaDataError> {
        query
            .split(',')
            .map(|pair| {
                pair.split_once('=')
                    .map(|(k, v)| (k.to_string(), v.to_string()))
                    .ok_or_else(|| MetaDataError::InvalidQuery(pair.to_string()))
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
        let mut pairs: Vec<_> = self.0.iter().map(|(k, v)| format!("{k}={v}")).collect();
        pairs.sort();
        write!(f, "{}", pairs.join(","))
    }
}

/// Maps each `#+key: value` metadata pair found on the search path to the tables
/// that carry it.
pub struct Index(HashMap<(String, String), HashSet<PathBuf>>);

static INDEX: LazyLock<Result<Index, MetaDataError>> =
    LazyLock::new(|| Index::build(&SearchPath::new_or("LOUIS_TABLE_PATH", ".")));

/// The index of the tables, built once on first use.
pub fn index() -> Result<&'static Index, &'static MetaDataError> {
    INDEX.as_ref()
}

impl Index {
    /// The tables carrying every one of the query's metadata pairs.
    pub fn find(&self, query: Query) -> HashSet<&PathBuf> {
        if query.is_empty() {
            return HashSet::new();
        }
        let candidates: Vec<_> = query
            .0
            .into_iter()
            .map(|(k, v)| self.0.get(&(k, v)))
            .collect();
        // if any of the queries was not found in the index then the whole query is empty
        if candidates.iter().any(|c| c.is_none()) {
            HashSet::new()
        } else {
            let candidates: Vec<_> = candidates.iter().flatten().collect();
            // get the intersection of all sets
            candidates[0]
                .iter()
                .filter(|c| candidates[1..].iter().all(|s| s.contains(*c)))
                .collect()
        }
    }

    /// Derive the index by walking the whole of `search_path` and reading the
    /// metadata out of every table on it.
    pub fn build(search_path: &SearchPath) -> Result<Self, MetaDataError> {
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
