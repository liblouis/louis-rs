use std::{
    collections::{HashMap, HashSet},
    fs,
    path::PathBuf,
};

#[derive(thiserror::Error, Debug)]
pub enum MetaDataError {
    #[error(transparent)]
    RegexError(#[from] regex::Error),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

use regex::Regex;
use search_path::SearchPath;

type Query = Vec<(String, String)>;

type Index = HashMap<(String, String), HashSet<PathBuf>>;

fn intersect_many(candidates: Vec<&HashSet<PathBuf>>) -> HashSet<PathBuf> {
    match candidates.len() {
        0 => HashSet::new(),
        1 => candidates[0].clone(),
        _ => {
            let (first, rest) = candidates.split_first().unwrap();
            *first & &intersect_many(rest.to_vec())
        }
    }
}

pub fn find(index: &Index, query: Query) -> HashSet<PathBuf> {
    let candidates: Vec<&HashSet<PathBuf>> = query
        .into_iter()
        .filter_map(|(k, v)| index.get(&(k, v)))
        .collect();
    intersect_many(candidates)
}

pub fn index() -> Result<Index, MetaDataError> {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");

    let mut index: Index = HashMap::new();

    for dir in search_path.iter() {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();

            if let Ok(content) = fs::read_to_string(&path) {
                let re = Regex::new(r"(?m)^#\+([^:]+):(.+)$")?;
                for (_, [key, value]) in re.captures_iter(&content).map(|c| c.extract()) {
                    let k = key.trim().into();
                    let v = value.trim().into();
                    index.entry((k, v)).or_default().insert(path.clone());
                }
            }
        }
    }
    Ok(index)
}
