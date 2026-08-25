/*! A library to translate text to braille and back

This library is a rust-only implementation of [liblouis](https://liblouis.io).

## Status

The API of this library is not stable yet. Breaking changes may occur
in any version.

## Usage

```no_run
use std::path::Path;

use louis::Translator;
use louis::Direction;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
let translator = Translator::new(&["en-us-g1.ctb"], Direction::Forward)?;
let braille = translator.translate("hello world")?;
assert_eq!(braille, "⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙");
# Ok(())
# }
```

*/

mod emphasis;
mod hyphenation;
mod parser;
mod translator;

use std::path::Path;

pub use emphasis::EmphasisSpan;
pub use parser::Direction;
use translator::TranslationPipeline;
pub use translator::{TranslationMode, TranslationModes, TranslationOptions};

#[derive(thiserror::Error, Debug)]
pub enum TranslationError {
    #[error(transparent)]
    TranslationFailed(#[from] translator::TranslationError),
    #[error("Errors when reading given braille table(s)")]
    ParseFailed(Vec<parser::TableError>),
}
#[derive(Debug, Clone)]
pub struct SpacingInfo {
    // TODO:
}

#[derive(Debug, Default)]
pub struct TranslationResult {
    pub output: String,
    pub emphasis: Option<Vec<EmphasisSpan>>, // Only if input had emphasis
    pub spacing: Option<Vec<SpacingInfo>>,
    pub output_positions: Option<Vec<usize>>, // Maps input pos -> output pos
    pub input_positions: Option<Vec<usize>>,  // Maps output pos -> input pos
    pub cursor_pos: Option<usize>,
}

#[derive(Debug)]
pub struct Translator(TranslationPipeline);

impl Translator {
    pub fn new<P: AsRef<Path>>(
        tables: &[P],
        direction: Direction,
    ) -> Result<Self, TranslationError> {
        let mut all_rules = Vec::new();

        for table_path in tables {
            let path = table_path.as_ref();
            let rules = parser::table_expanded(path).map_err(TranslationError::ParseFailed)?;
            all_rules.extend(rules);
        }

        Ok(Self(TranslationPipeline::compile(&all_rules, direction)?))
    }

    /// Build a translator from raw liblouis table source held in memory.
    ///
    /// This parses only the inline table text. Unlike [`Self::new`], it does
    /// not resolve `include` directives from the filesystem.
    pub fn from_table_source(table: &str, direction: Direction) -> Result<Self, TranslationError> {
        let rules = parser::table(table, None).map_err(TranslationError::ParseFailed)?;
        if rules
            .iter()
            .any(|rule| matches!(rule.rule, parser::Rule::Include { .. }))
        {
            return Err(TranslationError::ParseFailed(vec![
                parser::TableError::IncludesNotSupportedInMemory,
            ]));
        }

        Ok(Self(TranslationPipeline::compile(&rules, direction)?))
    }

    /// Simple translation - just input text to braille
    pub fn translate(&self, input: &str) -> Result<String, TranslationError> {
        Ok(self.0.translate(input))
    }

    /// Full-featured translation with all options
    pub fn translate_with_options(
        &self,
        input: &str,
        options: TranslationOptions,
    ) -> Result<TranslationResult, TranslationError> {
        let (output, positions) = self.0.translate_with_positions(input, &options);
        let cursor_pos = options.cursor_pos().map(|cursor| positions.cursor(cursor));
        let (output_positions, input_positions) = positions.into_parts();
        Ok(TranslationResult {
            output,
            output_positions: Some(output_positions),
            input_positions: Some(input_positions),
            cursor_pos,
            ..Default::default()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translator_from_table_source() {
        let table = "space \\s 0\nletter a 1\nletter b 12\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        assert_eq!(translator.translate("ab").unwrap(), "⠁⠃");
    }

    #[test]
    fn translator_from_table_source_rejects_include() {
        let error =
            Translator::from_table_source("include en-us-g1.ctb", Direction::Forward).unwrap_err();
        match error {
            TranslationError::ParseFailed(errors) => {
                assert!(matches!(
                    errors.as_slice(),
                    [parser::TableError::IncludesNotSupportedInMemory]
                ));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    const NUMBER_TABLE: &str =
        "space \\s 0\nletter a 1\nlitdigit 4 145\nlitdigit 2 12\nnumsign 3456\n";

    #[test]
    fn positions_with_number_sign() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("a 42", TranslationOptions::default().with_cursor_pos(2))
            .unwrap();
        assert_eq!(result.output, "⠁⠀⠼⠙⠃");
        assert_eq!(result.output_positions, Some(vec![0, 1, 3, 4]));
        assert_eq!(result.input_positions, Some(vec![0, 1, 2, 2, 3]));
        assert_eq!(result.cursor_pos, Some(3));
    }

    #[test]
    fn cursor_past_the_end_maps_to_the_output_length() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("a 42", TranslationOptions::default().with_cursor_pos(4))
            .unwrap();
        assert_eq!(result.cursor_pos, Some(5));
    }

    #[test]
    fn positions_without_cursor() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("a 42", TranslationOptions::default())
            .unwrap();
        assert_eq!(result.output, translator.translate("a 42").unwrap());
        assert_eq!(result.cursor_pos, None);
        assert!(result.output_positions.is_some());
        assert!(result.input_positions.is_some());
    }

    #[test]
    fn positions_with_capital_sign() {
        let table = "lowercase h 125\nlowercase i 24\nbase uppercase H h\ncapsletter 6\n";
        let forward = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = forward
            .translate_with_options("Hi", TranslationOptions::default().with_cursor_pos(0))
            .unwrap();
        assert_eq!(result.output, "⠠⠓⠊");
        assert_eq!(result.output_positions, Some(vec![1, 2]));
        assert_eq!(result.input_positions, Some(vec![0, 0, 1]));
        assert_eq!(result.cursor_pos, Some(1));

        let backward = Translator::from_table_source(table, Direction::Backward).unwrap();
        let result = backward
            .translate_with_options("⠠⠓⠊", TranslationOptions::default())
            .unwrap();
        assert_eq!(result.output, "Hi");
        assert_eq!(result.output_positions, Some(vec![0, 0, 1]));
        assert_eq!(result.input_positions, Some(vec![1, 2]));
    }

    #[test]
    fn positions_with_correction_insertion() {
        let table = "letter f 124\nletter o 135\nletter b 12\nletter a 1\nletter r 1235\npunctuation , 6\npunctuation - 36\nnoback correct \"f,\"[]\"o\" \"-\"\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("f,oobar", TranslationOptions::default())
            .unwrap();
        assert_eq!(result.output, "⠋⠠⠤⠕⠕⠃⠁⠗");
        assert_eq!(result.output_positions, Some(vec![0, 1, 3, 4, 5, 6, 7]));
        assert_eq!(result.input_positions, Some(vec![0, 1, 2, 2, 3, 4, 5, 6]));
    }

    #[test]
    fn positions_with_pass2_deletion() {
        let table = "letter f 124\nletter o 135\nletter x 1346\nnoback pass2 @1346 ?\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("xfoo", TranslationOptions::default())
            .unwrap();
        assert_eq!(result.output, "⠋⠕⠕");
        assert_eq!(result.output_positions, Some(vec![0, 0, 1, 2]));
        assert_eq!(result.input_positions, Some(vec![1, 2, 3]));
    }
}
