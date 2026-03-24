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

mod parser;
mod test;
mod translator;

use std::path::Path;

pub use parser::Direction;
pub use test::Typeform;
pub use test::{TranslationMode, TranslationModes};
use translator::TranslationPipeline;

#[derive(thiserror::Error, Debug)]
pub enum TranslationError {
    #[error(transparent)]
    TranslationFailed(#[from] translator::TranslationError),
    #[error("Errors when reading given braille table(s)")]
    ParseFailed(Vec<parser::TableError>),
}

#[derive(Debug, Clone)]
pub struct TranslationOptions {
    pub mode: TranslationModes,
    pub typeforms: Option<Vec<Typeform>>,
    pub cursor_pos: Option<usize>,
}

impl Default for TranslationOptions {
    fn default() -> Self {
        Self {
            mode: TranslationModes::empty(),
            typeforms: None,
            cursor_pos: None,
        }
    }
}
#[derive(Debug, Clone)]
pub struct SpacingInfo {
    // TODO:
}

#[derive(Debug, Default)]
pub struct TranslationResult {
    pub output: String,
    pub typeforms: Option<Vec<Typeform>>, // Only if input had typeforms
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
            output: self.0.translate(input),
            ..Default::default()
        })
    }
}
