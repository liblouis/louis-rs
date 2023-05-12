use std::{path::PathBuf, fs};

use parser::{Line, Rule};

pub mod parser;

#[derive(Debug, PartialEq)]
pub struct TranslationError;

pub fn translate(table: PathBuf, input: &str) -> Result<String, TranslationError> {
    let rules = match fs::read_to_string(table) {
	Ok(rules) => rules,
	Err(_) => return Err(TranslationError{}),
    };
    let (_, lines) = parser::table(&rules).unwrap();
    let rules : Vec<Rule> = lines.into_iter().filter_map(|line| line.as_rule()).collect();
    Ok(input.to_string())
}

pub fn debug(table: PathBuf) -> Result<Vec<Line>, TranslationError> {
    let rules = match fs::read_to_string(table) {
	Ok(rules) => rules,
	Err(_) => return Err(TranslationError{}),
    };
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
        assert_eq!(translate(PathBuf::from("tests/test_table.txt"), &input), Ok(input));
    }
}
