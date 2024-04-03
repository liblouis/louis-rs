use std::{
    collections::{HashMap, HashSet},
    fs,
    path::PathBuf,
};

#[derive(thiserror::Error, Debug)]
pub enum MetaDataError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

use search_path::SearchPath;

type Query = Vec<(String, String)>;

type Index = HashMap<(String, String), HashSet<PathBuf>>;

pub fn find(index: &Index, query: Query) -> HashSet<&PathBuf> {
    let candidates: Vec<_> = query.into_iter().map(|(k, v)| index.get(&(k, v))).collect();
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

pub fn index() -> Result<Index, MetaDataError> {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");

    let mut index: Index = HashMap::new();

    for dir in search_path.iter() {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();

            if let Ok(content) = fs::read_to_string(&path) {
                for line in content.lines() {
                    if let Some(line) = line.strip_prefix("#+") {
                        if let Some((key, value)) = line.split_once(':') {
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
    }
    Ok(index)
}
