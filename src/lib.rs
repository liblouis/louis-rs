//! An experimental  re-write of [liblouis] in Rust
//!
//! [liblouis]: http://liblouis.io

use search_path::SearchPath;
use std::{fs, io, path::PathBuf};
use thiserror::Error;

use parser::{expand_includes, Rule, table};
use translator::{Direction, TranslationTable, DisplayTable};

pub mod check;
pub mod parser;
pub mod translator;

/// An error encountered during braille translation.
#[derive(Error, Debug)]
pub enum TranslationError {
    /// The table could not be read
    #[error("Cannot open table")]
    ReadTable(#[from] io::Error),
    #[error("unknown translation error")]
    Unknown,
}

/// Translate the `input` using the given translation `table`. Return
/// a `Result` containing the translation.
pub fn translate(table: PathBuf, input: &str) -> Result<String, TranslationError> {
    let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let path = search_path.find_file(&PathBuf::from(table)).unwrap();
    let table = fs::read_to_string(path).unwrap();
    let (_, lines) = parser::table(&table).unwrap();
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let rules = expand_includes(&search_path, rules);
    let table = TranslationTable::compile(rules, Direction::Forward);
    Ok(table.translate(input))
}

pub fn display(table: PathBuf, input: &str) -> Result<String, TranslationError> {
    let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let path = search_path.find_file(&PathBuf::from(table)).unwrap();
    let table = fs::read_to_string(path).unwrap();
    let (_, lines) = parser::table(&table).unwrap();
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let rules = expand_includes(&search_path, rules);
    let table = DisplayTable::compile(rules);
    Ok(table.translate(input))
}

/// Return a `Vec` of all rules in the given translation `table` for
/// debugging purposes.
pub fn debug(table: PathBuf) -> Result<Vec<Rule>, TranslationError> {
    let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let table = fs::read_to_string(table)?;
    let (_, lines) = parser::table(&table).expect("Cannot parse table");
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let rules = expand_includes(&search_path, rules);
    Ok(rules)
}

#[cfg(test)]
mod tests {
    use crate::translate;
    use std::path::PathBuf;

    #[test]
    fn it_works() {
        let input = "testing123".to_string();
        let output = "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀".to_string();
        assert_eq!(
            translate(PathBuf::from("tests/test_table.txt"), &input).unwrap(),
            output
        );
    }
}
