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

use std::path::{Path, PathBuf};

pub use emphasis::EmphasisSpan;
pub use parser::Direction;
use search_path::SearchPath;
use translator::TranslationPipeline;
pub use translator::{TranslationMode, TranslationModes, TranslationOptions};

/// What the `louis` command line tool needs beyond the public API: rule tracing for `trace`,
/// and the pipeline and display tables for `check-table` and the YAML test harness.
/// Not part of the public API and not covered by semver; it changes whenever the CLI does.
#[doc(hidden)]
pub mod internals {
    pub mod parser {
        pub use crate::parser::{
            AnchoredRule, Direction, EscapingContext, ParseError, TableError, expand_includes,
            table, table_expanded_in, unescape,
        };
    }
    pub mod translator {
        pub use crate::translator::{
            DisplayTable, PositionMap, ResolvedTranslation, TranslationError, TranslationMode,
            TranslationModes, TranslationOptions, TranslationPipeline, TranslationStage,
        };
    }
}

#[derive(thiserror::Error, Debug)]
pub enum TranslationError {
    #[error(transparent)]
    TranslationFailed(#[from] translator::TranslationError),
    #[error("Errors when reading given braille table(s)")]
    ParseFailed(Vec<parser::TableError>),
}

#[derive(Debug, Default)]
pub struct TranslationResult {
    pub output: String,
    pub emphasis: Option<Vec<EmphasisSpan>>, // Only if input had emphasis
    pub output_positions: Option<Vec<usize>>, // Maps input pos -> output pos
    pub input_positions: Option<Vec<usize>>, // Maps output pos -> input pos
    pub cursor_pos: Option<usize>,
}

#[derive(Debug)]
pub struct Translator(TranslationPipeline);

impl Translator {
    /// Tables are looked up in the `LOUIS_TABLE_PATH` search path. Use
    /// [`Self::with_search_path`] to supply the directories instead, which a host
    /// application managing its own table locations will want.
    pub fn new<P: AsRef<Path>>(
        tables: &[P],
        direction: Direction,
    ) -> Result<Self, TranslationError> {
        Self::compile_tables(
            tables,
            direction,
            &SearchPath::new_or("LOUIS_TABLE_PATH", "."),
        )
    }

    /// Like [`Self::new`], but looking tables up in `search_path` instead of reading the
    /// `LOUIS_TABLE_PATH` environment variable.
    pub fn with_search_path<P: AsRef<Path>>(
        tables: &[P],
        direction: Direction,
        search_path: Vec<PathBuf>,
    ) -> Result<Self, TranslationError> {
        Self::compile_tables(tables, direction, &SearchPath::from(search_path))
    }

    fn compile_tables<P: AsRef<Path>>(
        tables: &[P],
        direction: Direction,
        search_path: &SearchPath,
    ) -> Result<Self, TranslationError> {
        let mut all_rules = Vec::new();

        for table_path in tables {
            let path = table_path.as_ref();
            let rules = parser::table_expanded_in(path, search_path)
                .map_err(TranslationError::ParseFailed)?;
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

    const CAPS_WORD_TABLE: &str = "space \\s 0\nlowercase a 1\nlowercase b 12\nlowercase c 14\nlowercase i 24\nlowercase m 134\nlowercase n 1345\nlowercase r 1235\nlowercase s 234\nlowercase t 2345\nbase uppercase A a\nbase uppercase B b\nbase uppercase C c\nbase uppercase I i\nbase uppercase M m\nbase uppercase N n\nbase uppercase R r\nbase uppercase S s\nbase uppercase T t\npunctuation ; 23\npunctuation - 36\npunctuation ' 3\nletsign 56\nbegcapsword 6-6\nendcapsword 56\nmidword bb 23\nalways 's 3-234\n";

    #[test]
    fn a_contraction_gets_its_letter_sign_only_where_it_applies() {
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\nlowercase s 234\nbase uppercase A a\nletsign 56\ncapsletter 6\nword as 1356\ncontraction as\ncontraction ab\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        // the earlier `word as` applies, so no letter sign
        assert_eq!(translator.translate("as").unwrap(), "⠵");
        // the contraction applies, its letter sign before the capital
        assert_eq!(translator.translate("ab").unwrap(), "⠰⠁⠃");
        assert_eq!(translator.translate("Ab").unwrap(), "⠰⠠⠁⠃");
    }

    #[test]
    fn a_capitalized_word_goes_on_while_letters_are_written() {
        let translator =
            Translator::from_table_source(CAPS_WORD_TABLE, Direction::Backward).unwrap();
        // whatever rule writes them: ⠆ is the semicolon and the contraction "bb"
        assert_eq!(translator.translate("⠠⠠⠗⠁⠆⠊⠞").unwrap(), "RABBIT");
        // a hyphen after a letter ends the word, but not one before the first letter
        assert_eq!(translator.translate("⠠⠠⠁⠃⠤⠍⠁⠝").unwrap(), "AB-man");
        assert_eq!(translator.translate("⠠⠠⠤⠍⠁⠝").unwrap(), "-MAN");
        // it can end partway through what a rule writes
        assert_eq!(translator.translate("⠠⠠⠍⠁⠝⠄⠎").unwrap(), "MAN's");
        // after a letter, ⠰ is `endcapsword`, not `letsign`
        assert_eq!(translator.translate("⠠⠠⠁⠃⠰⠉").unwrap(), "ABc");
    }

    #[test]
    fn a_rule_after_one_of_two_classes_applies_after_either() {
        // French `after conabr after bvoy endword ent 126`
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\nlowercase c 14\nlowercase x 1346\nattribute one a\nattribute two b\nafter one after two always yy 1346\n";
        let translator = Translator::from_table_source(table, Direction::Backward).unwrap();
        assert_eq!(translator.translate("⠁⠭⠀⠃⠭⠀⠉⠭").unwrap(), "ayy byy cx");
    }

    #[test]
    fn digits_after_a_number_sign_are_not_read_as_a_contraction() {
        let table = "space \\s 0\nletter a 1\nletter b 12\nlitdigit 1 1\nlitdigit 2 12\nnumsign 3456\nword about 1-12\n";
        let translator = Translator::from_table_source(table, Direction::Backward).unwrap();
        assert_eq!(translator.translate("⠼⠁⠃⠀⠁⠃").unwrap(), "12 about");
    }

    #[test]
    fn positions_with_number_sign() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("a 42", TranslationOptions::default().with_cursor_pos(2))
            .unwrap();
        assert_eq!(result.output, "⠁⠀⠼⠙⠃");
        assert_eq!(result.output_positions, Some(vec![0, 1, 2, 4]));
        assert_eq!(result.input_positions, Some(vec![0, 1, 2, 2, 3]));
        assert_eq!(result.cursor_pos, Some(2));
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
        assert_eq!(result.output_positions, Some(vec![0, 2]));
        assert_eq!(result.input_positions, Some(vec![0, 0, 1]));
        assert_eq!(result.cursor_pos, Some(0));

        let backward = Translator::from_table_source(table, Direction::Backward).unwrap();
        let result = backward
            .translate_with_options("⠠⠓⠊", TranslationOptions::default())
            .unwrap();
        assert_eq!(result.output, "Hi");
        assert_eq!(result.output_positions, Some(vec![0, 0, 1]));
        // backward, a consumed cell is claimed by the character that follows it, so the "H"
        // covers the capital sign as well ("prepend to next", as liblouis calls it)
        assert_eq!(result.input_positions, Some(vec![0, 2]));
    }

    #[test]
    fn positions_with_correction_insertion() {
        let table = "letter f 124\nletter o 135\nletter b 12\nletter a 1\nletter r 1235\npunctuation , 6\npunctuation - 36\nnoback correct \"f,\"[]\"o\" \"-\"\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = translator
            .translate_with_options("f,oobar", TranslationOptions::default())
            .unwrap();
        assert_eq!(result.output, "⠋⠠⠤⠕⠕⠃⠁⠗");
        assert_eq!(result.output_positions, Some(vec![0, 1, 2, 4, 5, 6, 7]));
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
