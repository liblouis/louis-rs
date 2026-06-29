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

pub mod emphasis;
mod parser;
mod test;
mod translator;

use std::path::Path;

pub use emphasis::EmphasisSpan;
pub use parser::Direction;
use translator::TranslationPipeline;
pub use translator::{TranslationMode, TranslationModes, TranslationOptions};

use crate::translator::ResolvedTranslation;

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
            let rules = parser::table_expanded(path)
                .map_err(|errors| TranslationError::ParseFailed(errors))?;
            all_rules.extend(rules);
        }

        Ok(Self(TranslationPipeline::compile(&all_rules, direction)?))
    }

    fn compute_output_positions(translations: &[ResolvedTranslation]) -> Vec<usize> {
        let mut output_positions: Vec<usize> = Vec::new();
        let mut output_offset: usize = 0;

        for translation in translations {
            let input_len = translation.input().chars().count();
            let output_len = translation.output().chars().count();

            for i in 0..input_len {
                output_positions.push(output_offset + i.min(output_len.saturating_sub(1)));
            }
            output_offset += output_len;
        }
        output_positions
    }

    fn compute_input_positions(translations: &[ResolvedTranslation]) -> Vec<usize> {
        let mut input_positions: Vec<usize> = Vec::new();
        let mut input_offset: usize = 0;

        for translation in translations {
            let input_len = translation.input().chars().count();
            let output_len = translation.output().chars().count();

            for i in 0..output_len {
                input_positions.push(input_offset + i.min(input_len.saturating_sub(1)));
            }
            input_offset += input_len;
        }
        input_positions
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
    use crate::translator::TranslationStage;

    use super::*;

    fn translation(input: &str, output: &str) -> ResolvedTranslation {
        ResolvedTranslation::new(input, output, 0, TranslationStage::Main, None)
    }

    #[test]
    fn output_positions() {
        assert_eq!(
            Translator::compute_output_positions(&[translation("abc", "⠁⠃⠇")]),
            [0, 1, 2]
        );
        assert_eq!(
            Translator::compute_output_positions(&[translation("foo", "⠁")]),
            [0, 0, 0]
        );
        assert_eq!(
            Translator::compute_output_positions(&[translation("foo", "⠁⠃")]),
            [0, 1, 1]
        );
        assert_eq!(
            Translator::compute_output_positions(&[translation("a", "⠁⠃⠇")]),
            [0]
        );
        assert_eq!(
            Translator::compute_output_positions(&[
                translation("abc", "⠁⠃⠇"),
                translation("abc", "⠁⠃⠇")
            ]),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            Translator::compute_output_positions(&[
                translation("foo", "⠁"),
                translation("abc", "⠁⠃⠇")
            ]),
            [0, 0, 0, 1, 2, 3]
        );
        assert_eq!(
            Translator::compute_output_positions(&[
                translation("a", "⠁⠃⠇"),
                translation("abc", "⠁⠃⠇")
            ]),
            [0, 3, 4, 5]
        );
    }

    #[test]
    fn input_positions() {
        assert_eq!(
            Translator::compute_input_positions(&[translation("abc", "⠁⠃⠇")]),
            [0, 1, 2]
        );
        assert_eq!(
            Translator::compute_input_positions(&[translation("foo", "⠁")]),
            [0]
        );
        assert_eq!(
            Translator::compute_input_positions(&[translation("foo", "⠁⠃")]),
            [0, 1]
        );
        assert_eq!(
            Translator::compute_input_positions(&[translation("a", "⠁⠃⠇")]),
            [0, 0, 0]
        );
        assert_eq!(
            Translator::compute_input_positions(&[
                translation("abc", "⠁⠃⠇"),
                translation("abc", "⠁⠃⠇")
            ]),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            Translator::compute_input_positions(&[
                translation("foo", "⠁"),
                translation("abc", "⠁⠃⠇")
            ]),
            [0, 3, 4, 5]
        );
        assert_eq!(
            Translator::compute_input_positions(&[
                translation("a", "⠁⠃⠇"),
                translation("abc", "⠁⠃⠇")
            ]),
            [0, 0, 0, 1, 2, 3]
        );
    }
}
