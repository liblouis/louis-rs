//! An experimental  re-write of [liblouis] in Rust
//!
//! [liblouis]: http://liblouis.io

use search_path::SearchPath;
use std::{fs, io, path::PathBuf};
use thiserror::Error;

use parser::{expand_includes, LouisParseError, Rule};
use translator::{Direction, DisplayTable, TranslationTable};

pub mod check;
pub mod parser;
pub mod translator;

/// An error encountered during braille translation.
#[derive(Error, Debug)]
pub enum TranslationError {
    /// The table could not be read
    #[error("Cannot open table")]
    ReadTable(#[from] io::Error),
    #[error("Cannot parse table")]
    ParseTable(#[from] LouisParseError),
    #[error("unknown translation error")]
    Unknown,
}

pub fn compile(table: &PathBuf) -> Result<TranslationTable, TranslationError> {
    let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let path = search_path.find_file(&PathBuf::from(table)).unwrap();
    let table = fs::read_to_string(path)?;
    let lines = parser::table(&table)?;
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let rules = expand_includes(&search_path, rules);
    Ok(TranslationTable::compile(rules, Direction::Forward))
}

pub fn compile_display(table: &PathBuf) -> Result<DisplayTable, TranslationError> {
    let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let path = search_path.find_file(&PathBuf::from(table)).unwrap();
    let table = fs::read_to_string(path)?;
    let lines = parser::table(&table)?;
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let rules = expand_includes(&search_path, rules);
    Ok(DisplayTable::compile(rules))
}

/// Translate the `input` using the given translation `table`. Return
/// a `Result` containing the translation.
pub fn translate(table: &TranslationTable, input: &str) -> String {
    table.translate(input)
}

pub fn display(table: &DisplayTable, input: &str) -> String {
    table.translate(input)
}

/// Return a `Vec` of all rules in the given translation `table` for
/// debugging purposes.
pub fn debug(table: PathBuf) -> Result<Vec<Rule>, TranslationError> {
    let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let table = fs::read_to_string(table)?;
    let lines = parser::table(&table)?;
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let rules = expand_includes(&search_path, rules);
    Ok(rules)
}

#[cfg(test)]
mod tests {
    use crate::{compile, translate};
    use std::path::PathBuf;

    #[test]
    fn it_works() {
        let input = "testing123".to_string();
        let output = "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀".to_string();
        let table = compile(&PathBuf::from("tests/test_table.txt")).unwrap();
        assert_eq!(translate(&table, &input), output);
    }
}
