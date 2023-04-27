use std::{path::PathBuf, fs};

use parser::Line;

pub mod parser;

#[derive(Debug, PartialEq)]
pub struct TranslationError;

fn is_rule(line: &Line) -> bool {
    match line {
     	Line::Rule {rule: _, comment: _} => true,
     	_ => false,
    }
//    *line == Line::Rule {rule: _, comment: _}
}

pub fn translate(table: PathBuf, input: &str) -> Result<String, TranslationError> {
    let rules = match fs::read_to_string(table) {
	Ok(rules) => rules,
	Err(_) => return Err(TranslationError{}),
    };
    let (_, all_rules) = parser::table(&rules).unwrap();
    let rules : Vec<Line> = all_rules.into_iter().filter(|line| is_rule(line)).collect();
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
