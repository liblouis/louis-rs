use std::{fs, io, path::PathBuf};
use thiserror::Error;

use parser::Line;
use translator::TranslationTable;

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

pub fn translate(table: PathBuf, input: &str) -> Result<String, TranslationError> {
    let rules = fs::read_to_string(table)?;

    let (_, lines) = parser::table(&rules).unwrap();
    let rules = lines
        .into_iter()
        .filter_map(|line| line.as_rule())
        .collect();
    let table = TranslationTable::compile(rules);
    Ok(table.translate(input))
}

pub fn debug(table: PathBuf) -> Result<Vec<Line>, TranslationError> {
    let rules = fs::read_to_string(table)?;
    let (_, all_rules) = parser::table(&rules).expect("Cannot parse table");
    Ok(all_rules)
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
