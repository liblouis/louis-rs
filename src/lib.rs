/*! Translate text to braille and back.

louis-rs is a re-implementation of [liblouis](https://liblouis.io) in Rust. It reads
liblouis's tables and passes its test suite, but shares no code with it.

## Status

The API follows semantic versioning: breaking changes come only with a new minor
version (0.4 → 0.5), never with a patch release. Items hidden from this documentation
are not covered.

## Usage

A [`Translator`] compiles a list of tables for one [`Direction`]; translating the other
way takes a second translator. [`Translator::new`] looks the tables up in the
directories listed in the `LOUIS_TABLE_PATH` environment variable, and
[`Translator::with_search_path`] takes the directories as an argument.

```no_run
use louis::{Direction, Translator};

let translator = Translator::new(&["en-us-g1.ctb"], Direction::Forward)?;
assert_eq!(translator.translate("hello world"), "⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙");
# Ok::<(), louis::Error>(())
```

[`Translator::from_table_source`] compiles a table held in memory, which cannot
include other tables:

```
use louis::{Direction, Translator};

let table = "space \\s 0\nlowercase a 1\nlowercase b 12\n";
let forward = Translator::from_table_source(table, Direction::Forward)?;
assert_eq!(forward.translate("ab ba"), "⠁⠃⠀⠃⠁");
let backward = Translator::from_table_source(table, Direction::Backward)?;
assert_eq!(backward.translate("⠁⠃⠀⠃⠁"), "ab ba");
# Ok::<(), louis::Error>(())
```

Braille is read and written as Unicode braille. A [`BrailleEncoding`] writes the cells
as other characters instead, such as ASCII braille. It is loaded from a liblouis display
table (`.dis`):

```no_run
use louis::{BrailleEncoding, Direction, Translator};

let brf = BrailleEncoding::new("en-us-brf.dis")?;
let translator =
    Translator::new(&["en-us-g2.ctb"], Direction::Forward)?.with_braille_encoding(&brf);
assert_eq!(translator.translate("the"), "!");
# Ok::<(), louis::Error>(())
```

[`Translator::translate_with_options`] also maps positions between input and output,
and [`metadata`] finds tables by their metadata instead of their file name.

*/

mod braille_encoding;
mod emphasis;
mod hyphenation;
pub mod metadata;
mod parser;
mod translator;

use std::path::{Path, PathBuf};

pub use braille_encoding::BrailleEncoding;
pub use emphasis::{Emphasis, EmphasisSpan};
pub use parser::{Direction, SyntaxError, TableError};
use search_path::SearchPath;
use translator::TranslationPipeline;
pub use translator::{
    CompileError, CompileErrorKind, TranslationMode, TranslationModes, TranslationOptions,
};

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
            CompileError, DisplayTable, PositionMap, ResolvedTranslation, TranslationMode,
            TranslationModes, TranslationOptions, TranslationPipeline, TranslationStage,
        };
    }
}

/// Why a [`Translator`] could not be built.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    /// The tables could not be read: each error names its table and, for a syntax error,
    /// its line.
    #[error("{}", table_errors(.0))]
    Table(Vec<TableError>),
    /// The tables were read, but a rule refers to something that no rule defines.
    #[error(transparent)]
    Compile(#[from] CompileError),
}

/// The first of `errors`, and how many more there are
fn table_errors(errors: &[TableError]) -> String {
    match errors {
        [] => "cannot read the tables".to_string(),
        [error] => error.to_string(),
        [error, rest @ ..] => format!("{error} (and {} more)", rest.len()),
    }
}

/// The result of [`Translator::translate_with_options`].
///
/// All positions are indices of characters (Unicode scalar values), not of bytes.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TranslationResult {
    output: String,
    emphasis: Option<Emphasis>,
    output_positions: Vec<usize>,
    input_positions: Vec<usize>,
    cursor_position: Option<usize>,
}

impl TranslationResult {
    /// The translated text.
    pub fn output(&self) -> &str {
        &self.output
    }

    /// Backward, the emphasis of the output, as the braille's emphasis indicators give it:
    /// the typeform of a back-translation. Forward, where the emphasis is given with the
    /// input, always `None`.
    pub fn emphasis(&self) -> Option<&Emphasis> {
        self.emphasis.as_ref()
    }

    /// For each input character, the position of its translation in the output.
    pub fn output_positions(&self) -> &[usize] {
        &self.output_positions
    }

    /// For each output character, the position of the input it was translated from.
    pub fn input_positions(&self) -> &[usize] {
        &self.input_positions
    }

    /// The cursor position given in the options, mapped to the output.
    pub fn cursor_position(&self) -> Option<usize> {
        self.cursor_position
    }
}

/// Translates text to braille ([`Direction::Forward`]) or braille to text
/// ([`Direction::Backward`]) with a set of compiled tables.
#[derive(Debug)]
pub struct Translator(TranslationPipeline);

impl Translator {
    /// Compile `tables` for translating in `direction`.
    ///
    /// Tables are looked up in the `LOUIS_TABLE_PATH` search path. Use
    /// [`Self::with_search_path`] to supply the directories instead, which a host
    /// application managing its own table locations will want.
    ///
    /// # Errors
    ///
    /// [`Error::Table`] if a table or one it includes cannot be found, read or parsed, and
    /// [`Error::Compile`] if the rules cannot be compiled.
    pub fn new<P: AsRef<Path>>(tables: &[P], direction: Direction) -> Result<Self, Error> {
        Self::compile_tables(
            tables,
            direction,
            &SearchPath::new_or("LOUIS_TABLE_PATH", "."),
        )
    }

    /// Like [`Self::new`], but looking tables up in `search_path` instead of reading the
    /// `LOUIS_TABLE_PATH` environment variable.
    ///
    /// # Errors
    ///
    /// As for [`Self::new`].
    pub fn with_search_path<P: AsRef<Path>>(
        tables: &[P],
        direction: Direction,
        search_path: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> Result<Self, Error> {
        let search_path: Vec<PathBuf> = search_path.into_iter().map(Into::into).collect();
        Self::compile_tables(tables, direction, &SearchPath::from(search_path))
    }

    fn compile_tables<P: AsRef<Path>>(
        tables: &[P],
        direction: Direction,
        search_path: &SearchPath,
    ) -> Result<Self, Error> {
        let mut all_rules = Vec::new();

        for table_path in tables {
            let path = table_path.as_ref();
            let rules = parser::table_expanded_in(path, search_path).map_err(Error::Table)?;
            all_rules.extend(rules);
        }

        Ok(Self(TranslationPipeline::compile(&all_rules, direction)?))
    }

    /// Build a translator from raw liblouis table source held in memory.
    ///
    /// This parses only the inline table text. Unlike [`Self::new`], it does
    /// not resolve `include` directives from the filesystem.
    ///
    /// # Errors
    ///
    /// [`Error::Table`] if the source cannot be parsed or contains an `include`, and
    /// [`Error::Compile`] if the rules cannot be compiled.
    pub fn from_table_source(table: &str, direction: Direction) -> Result<Self, Error> {
        let rules = parser::table(table, None).map_err(Error::Table)?;
        if rules
            .iter()
            .any(|rule| matches!(rule.rule, parser::Rule::Include { .. }))
        {
            return Err(Error::Table(vec![TableError::IncludesNotSupportedInMemory]));
        }

        Ok(Self(TranslationPipeline::compile(&rules, direction)?))
    }

    /// Read and write braille in `encoding` rather than as Unicode braille. An encoding
    /// given before is replaced.
    ///
    /// This is what liblouis does with the display table at the start of a table list;
    /// louis-rs takes it explicitly instead.
    pub fn with_braille_encoding(self, encoding: &BrailleEncoding) -> Self {
        let direction = self.0.direction();
        Self(self.0.with_display(encoding.compile(direction)))
    }

    /// The emphasis classes the tables declare with `emphclass`, such as `italic` or
    /// `bold`, in the order they declare them: the classes an [`EmphasisSpan`] can name.
    pub fn emphasis_classes(&self) -> impl Iterator<Item = &str> {
        self.0.emphasis_classes().iter().map(String::as_str)
    }

    /// Translate `input`.
    pub fn translate(&self, input: &str) -> String {
        self.0.translate(input)
    }

    /// Translate `input` with `options`, and say which input became which output.
    pub fn translate_with_options(
        &self,
        input: &str,
        options: &TranslationOptions,
    ) -> TranslationResult {
        let (output, positions, emphasis) = self.0.translate_with_details(input, options);
        let cursor_position = options
            .cursor_position()
            .map(|cursor| positions.cursor(cursor));
        let (output_positions, input_positions) = positions.into_parts();
        TranslationResult {
            output,
            output_positions,
            input_positions,
            cursor_position,
            emphasis,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // a translator is meant to be shared between threads
    #[test]
    fn translator_is_send_and_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Translator>();
    }

    #[test]
    fn translator_from_table_source() {
        let table = "space \\s 0\nletter a 1\nletter b 12\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        assert_eq!(translator.translate("ab"), "⠁⠃");
    }

    #[test]
    fn back_translation_returns_the_emphasis() {
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\nemphclass italic\n\
                     emphletter italic 46-23\nbegemphword italic 46-2\nendemphword italic 46-3\n";
        let translator = Translator::from_table_source(table, Direction::Backward).unwrap();
        let emphasis = |braille| {
            translator
                .translate_with_options(braille, &TranslationOptions::default())
                .emphasis()
                .unwrap()
                .clone()
        };
        // a letter, then a word up to its end indicator
        assert_eq!(
            emphasis("⠨⠆⠁⠃").spans(),
            [EmphasisSpan::new("italic", 0..1)]
        );
        assert_eq!(
            emphasis("⠃⠀⠨⠂⠁⠃⠨⠄⠁").spans(),
            [EmphasisSpan::new("italic", 2..4)]
        );
        // a word without an end indicator ends at the space
        assert_eq!(
            emphasis("⠨⠂⠁⠃⠀⠁").spans(),
            [EmphasisSpan::new("italic", 0..2)]
        );
        // forward, the emphasis comes with the input
        let forward = Translator::from_table_source(table, Direction::Forward).unwrap();
        assert!(
            forward
                .translate_with_options("ab", &TranslationOptions::default())
                .emphasis()
                .is_none()
        );
    }

    #[test]
    fn a_base_character_must_be_defined_in_some_direction() {
        let error =
            Translator::from_table_source("base uppercase A a", Direction::Forward).unwrap_err();
        let Error::Compile(error) = error else {
            panic!("expected a compile error, got {error:?}");
        };
        assert_eq!(
            *error.kind(),
            CompileErrorKind::BaseCharacterNotDefined {
                base: 'a',
                derived: 'A'
            }
        );
        // table source in memory has no path, and the rule is on the first line
        assert_eq!((error.path(), error.line()), (None, 1));
        assert_eq!(
            error.to_string(),
            "line 1: base character 'a' of 'A' not defined"
        );
        // defined forward only: backward there is nothing to derive, which is fine
        let table = "noback lowercase a 1\nbase uppercase A a\n";
        for direction in [Direction::Forward, Direction::Backward] {
            assert!(Translator::from_table_source(table, direction).is_ok());
        }
    }

    #[test]
    fn a_braille_encoding_writes_the_cells_both_ways() {
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\n";
        let encoding = BrailleEncoding::from_table_source("display a 1\ndisplay b 12\n").unwrap();
        let forward = Translator::from_table_source(table, Direction::Forward)
            .unwrap()
            .with_braille_encoding(&encoding);
        assert_eq!(forward.translate("ab ba"), "ab⠀ba");
        let backward = Translator::from_table_source(table, Direction::Backward)
            .unwrap()
            .with_braille_encoding(&encoding);
        assert_eq!(backward.translate("ab⠀ba"), "ab ba");
        // a second encoding replaces the first, in either direction
        let other = BrailleEncoding::from_table_source("display x 1\ndisplay y 12\n").unwrap();
        assert_eq!(forward.with_braille_encoding(&other).translate("ab"), "xy");
        assert_eq!(backward.with_braille_encoding(&other).translate("xy"), "ab");
        // and without one, Unicode braille
        let plain = Translator::from_table_source(table, Direction::Forward).unwrap();
        assert_eq!(plain.translate("ab"), "⠁⠃");
    }

    #[test]
    fn a_translator_lists_its_emphasis_classes() {
        let table = "emphclass italic\nemphclass bold\nemphclass italic\nletter a 1\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        assert_eq!(
            translator.emphasis_classes().collect::<Vec<_>>(),
            ["italic", "bold"]
        );
    }

    #[test]
    fn partial_input_has_no_word_end() {
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\nlowercase y 13456\n\
                     word about 1-12\njoinword by 356\n";
        let translator = Translator::from_table_source(table, Direction::Backward).unwrap();
        let partial = TranslationOptions::default()
            .with_modes([TranslationMode::PartialTrans].into_iter().collect());
        let translate = |braille, options: &TranslationOptions| {
            translator
                .translate_with_options(braille, options)
                .output()
                .to_string()
        };
        // the input could still become a longer word, so `word about` doesn't apply
        assert_eq!(translate("⠁⠃", &TranslationOptions::default()), "about");
        assert_eq!(translate("⠁⠃", &partial), "ab");
        // a `joinword` applies, and puts its space back only once a word follows
        assert_eq!(translate("⠴", &partial), "by");
        assert_eq!(translate("⠴⠁", &partial), "by a");
    }

    #[test]
    fn the_word_at_the_cursor_is_computer_braille() {
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\nlowercase c 14\n\
                     always abc 123456\ncomp6 a 2\ncomp6 b 23\ncomp6 c 25\n\
                     begcomp 456-346\nendcomp 456-156\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        let translate = |mode, cursor| {
            let options = TranslationOptions::default()
                .with_modes([mode].into_iter().collect())
                .with_cursor_position(cursor);
            translator
                .translate_with_options("abc abc abc", &options)
                .output()
                .to_string()
        };
        // the whole word at the cursor, without begcomp and endcomp
        assert_eq!(translate(TranslationMode::CompbrlAtCursor, 5), "⠿⠀⠂⠆⠒⠀⠿");
        // only up to the cursor
        assert_eq!(translate(TranslationMode::CompbrlLeftCursor, 6), "⠿⠀⠂⠆⠉⠀⠿");
        // without the mode, the cursor changes nothing
        assert_eq!(translate(TranslationMode::NoUndefined, 5), "⠿⠀⠿⠀⠿");
    }

    #[test]
    fn a_table_error_says_what_and_where() {
        let error =
            Translator::from_table_source("letter a 1\nfoo b 2\nbar c 3\n", Direction::Forward)
                .unwrap_err();
        assert_eq!(
            error.to_string(),
            "line 2: opcode expected, got Some(\"foo\") (and 1 more)"
        );
    }

    #[test]
    fn translator_from_table_source_rejects_include() {
        let error =
            Translator::from_table_source("include en-us-g1.ctb", Direction::Forward).unwrap_err();
        match error {
            Error::Table(errors) => {
                assert!(matches!(
                    errors.as_slice(),
                    [TableError::IncludesNotSupportedInMemory]
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
        assert_eq!(translator.translate("as"), "⠵");
        // the contraction applies, its letter sign before the capital
        assert_eq!(translator.translate("ab"), "⠰⠁⠃");
        assert_eq!(translator.translate("Ab"), "⠰⠠⠁⠃");
    }

    #[test]
    fn a_capitalized_word_goes_on_while_letters_are_written() {
        let translator =
            Translator::from_table_source(CAPS_WORD_TABLE, Direction::Backward).unwrap();
        // whatever rule writes them: ⠆ is the semicolon and the contraction "bb"
        assert_eq!(translator.translate("⠠⠠⠗⠁⠆⠊⠞"), "RABBIT");
        // a hyphen after a letter ends the word, but not one before the first letter
        assert_eq!(translator.translate("⠠⠠⠁⠃⠤⠍⠁⠝"), "AB-man");
        assert_eq!(translator.translate("⠠⠠⠤⠍⠁⠝"), "-MAN");
        // it can end partway through what a rule writes
        assert_eq!(translator.translate("⠠⠠⠍⠁⠝⠄⠎"), "MAN's");
        // after a letter, ⠰ is `endcapsword`, not `letsign`
        assert_eq!(translator.translate("⠠⠠⠁⠃⠰⠉"), "ABc");
    }

    #[test]
    fn a_rule_after_one_of_two_classes_applies_after_either() {
        // French `after conabr after bvoy endword ent 126`
        let table = "space \\s 0\nlowercase a 1\nlowercase b 12\nlowercase c 14\nlowercase x 1346\nattribute one a\nattribute two b\nafter one after two always yy 1346\n";
        let translator = Translator::from_table_source(table, Direction::Backward).unwrap();
        assert_eq!(translator.translate("⠁⠭⠀⠃⠭⠀⠉⠭"), "ayy byy cx");
    }

    #[test]
    fn digits_after_a_number_sign_are_not_read_as_a_contraction() {
        let table = "space \\s 0\nletter a 1\nletter b 12\nlitdigit 1 1\nlitdigit 2 12\nnumsign 3456\nword about 1-12\n";
        let translator = Translator::from_table_source(table, Direction::Backward).unwrap();
        assert_eq!(translator.translate("⠼⠁⠃⠀⠁⠃"), "12 about");
    }

    #[test]
    fn positions_with_number_sign() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator.translate_with_options(
            "a 42",
            &TranslationOptions::default().with_cursor_position(2),
        );
        assert_eq!(result.output(), "⠁⠀⠼⠙⠃");
        assert_eq!(result.output_positions(), [0, 1, 2, 4]);
        assert_eq!(result.input_positions(), [0, 1, 2, 2, 3]);
        assert_eq!(result.cursor_position(), Some(2));
    }

    #[test]
    fn cursor_past_the_end_maps_to_the_output_length() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator.translate_with_options(
            "a 42",
            &TranslationOptions::default().with_cursor_position(4),
        );
        assert_eq!(result.cursor_position(), Some(5));
    }

    #[test]
    fn positions_without_cursor() {
        let translator = Translator::from_table_source(NUMBER_TABLE, Direction::Forward).unwrap();
        let result = translator.translate_with_options("a 42", &TranslationOptions::default());
        assert_eq!(result.output(), translator.translate("a 42"));
        assert_eq!(result.cursor_position(), None);
    }

    #[test]
    fn positions_with_capital_sign() {
        let table = "lowercase h 125\nlowercase i 24\nbase uppercase H h\ncapsletter 6\n";
        let forward = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = forward
            .translate_with_options("Hi", &TranslationOptions::default().with_cursor_position(0));
        assert_eq!(result.output(), "⠠⠓⠊");
        assert_eq!(result.output_positions(), [0, 2]);
        assert_eq!(result.input_positions(), [0, 0, 1]);
        assert_eq!(result.cursor_position(), Some(0));

        let backward = Translator::from_table_source(table, Direction::Backward).unwrap();
        let result = backward.translate_with_options("⠠⠓⠊", &TranslationOptions::default());
        assert_eq!(result.output(), "Hi");
        assert_eq!(result.output_positions(), [0, 0, 1]);
        // backward, a consumed cell is claimed by the character that follows it, so the "H"
        // covers the capital sign as well ("prepend to next", as liblouis calls it)
        assert_eq!(result.input_positions(), [0, 2]);
    }

    #[test]
    fn positions_with_correction_insertion() {
        let table = "letter f 124\nletter o 135\nletter b 12\nletter a 1\nletter r 1235\npunctuation , 6\npunctuation - 36\nnoback correct \"f,\"[]\"o\" \"-\"\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = translator.translate_with_options("f,oobar", &TranslationOptions::default());
        assert_eq!(result.output(), "⠋⠠⠤⠕⠕⠃⠁⠗");
        assert_eq!(result.output_positions(), [0, 1, 2, 4, 5, 6, 7]);
        assert_eq!(result.input_positions(), [0, 1, 2, 2, 3, 4, 5, 6]);
    }

    #[test]
    fn positions_with_pass2_deletion() {
        let table = "letter f 124\nletter o 135\nletter x 1346\nnoback pass2 @1346 ?\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        let result = translator.translate_with_options("xfoo", &TranslationOptions::default());
        assert_eq!(result.output(), "⠋⠕⠕");
        assert_eq!(result.output_positions(), [0, 0, 1, 2]);
        assert_eq!(result.input_positions(), [1, 2, 3]);
    }
}
