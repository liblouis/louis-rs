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
        self.translate_with_options(input, TranslationOptions::default())
            .map(|result| result.output)
    }

    /// Full-featured translation with all options
    pub fn translate_with_options(
        &self,
        input: &str,
        options: TranslationOptions,
    ) -> Result<TranslationResult, TranslationError> {
        Ok(TranslationResult {
            output: self.0.translate_with_options(input, &options),
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
}
