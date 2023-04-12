use std::path::PathBuf;

pub mod parser;

#[derive(Debug, PartialEq)]
pub struct TranslationError;

pub fn translate(_table: PathBuf, input: &str) -> Result<String, TranslationError> {
    Ok(input.to_string())
}

#[cfg(test)]
mod tests {
    use crate::translate;
    use std::path::PathBuf;

    #[test]
    fn it_works() {
	let input = "testing123".to_string();
        assert_eq!(translate(PathBuf::new(), &input), Ok(input));
    }
}
