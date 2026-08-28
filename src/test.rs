//! A test runner for tests defined in [liblouis](https://liblouis.io) YAML test files

use std::{collections::HashMap, path::PathBuf};

use enumset::EnumSet;
use rayon::prelude::*;

use crate::resolver::SearchDirs;

use crate::{
    emphasis::EmphasisSpan,
    parser::{self, Direction, TableError},
    translator::{
        self, DisplayTable, PositionMap, TranslationModes, TranslationOptions, TranslationPipeline,
    },
};

#[derive(thiserror::Error, Debug)]
pub enum TestError {
    #[error("{0} have not been implemented (yet)")]
    NotImplemented(String),
    #[error("Errors in table {0:?}")]
    TableErrors(Vec<TableError>),
    #[error("Error when compiling table {0:?}")]
    CompilationError(#[from] translator::TranslationError),
}

impl From<Vec<TableError>> for TestError {
    fn from(errors: Vec<TableError>) -> Self {
        Self::TableErrors(errors)
    }
}

#[derive(Debug)]
pub enum TestMode {
    Forward,
    Backward,
    BothDirections,
    Display,
    Hyphenate,
    HyphenateBraille,
}

/// A translation that failed either because the translated text itself didn't match (`Translation`)
/// or, when the text matched, because the position mapping didn't (`Position`). Position is only
/// ever checked once the translation itself succeeds, so the two reasons are mutually exclusive.
#[derive(PartialEq, Debug)]
pub enum FailureReason {
    Translation {
        input: String,
        expected: String,
        actual: String,
        direction: Direction,
    },
    Position(PositionMismatch),
}

#[derive(PartialEq, Debug)]
pub enum TestResult {
    Success,
    Failure(FailureReason),
    ExpectedFailure {
        input: String,
        expected: String,
        actual: String,
        direction: Direction,
    },
    UnexpectedSuccess {
        input: String,
        direction: Direction,
    },
}

impl TestResult {
    pub fn is_success(&self) -> bool {
        matches!(self, TestResult::Success)
    }
    pub fn is_translation_failure(&self) -> bool {
        matches!(self, TestResult::Failure(FailureReason::Translation { .. }))
    }
    pub fn is_position_failure(&self) -> bool {
        matches!(self, TestResult::Failure(FailureReason::Position(_)))
    }
    pub fn is_expected_failure(&self) -> bool {
        matches!(self, TestResult::ExpectedFailure { .. })
    }
    pub fn is_unexpected_success(&self) -> bool {
        matches!(self, TestResult::UnexpectedSuccess { .. })
    }
}

#[derive(PartialEq, Debug)]
pub struct PositionMismatch {
    input: String,
    direction: Direction,
    diffs: Vec<PositionDiff>,
}

impl PositionMismatch {
    pub fn input(&self) -> &str {
        &self.input
    }
    pub fn direction(&self) -> Direction {
        self.direction
    }
    pub fn diffs(&self) -> &[PositionDiff] {
        &self.diffs
    }
}

#[derive(PartialEq, Debug)]
pub enum PositionDiff {
    InputPos {
        expected: Vec<usize>,
        actual: Vec<usize>,
    },
    OutputPos {
        expected: Vec<usize>,
        actual: Vec<usize>,
    },
    Cursor {
        expected: usize,
        actual: usize,
    },
}

/// A group of [`Tests`](Test) that share the same braille table(s), display table and test mode.
#[derive(Debug)]
pub struct TestMatrix<'a> {
    /// The display table used for the translation tests
    display: &'a Option<Display>,
    /// The braille table(s) used for the translation tests
    tables: &'a Vec<Table>,
    /// The test mode used for the translation tests
    mode: &'a TestMode,
    /// The tests used for the translation tests
    tests: &'a Vec<Test>,
}

impl<'a> TestMatrix<'a> {
    pub fn new(
        display: &'a Option<Display>,
        tables: &'a Vec<Table>,
        mode: &'a TestMode,
        tests: &'a Vec<Test>,
    ) -> Self {
        TestMatrix {
            display,
            tables,
            mode,
            tests,
        }
    }

    fn display_table(&self, direction: Direction) -> Result<DisplayTable, TestError> {
        let resolver = SearchDirs::from_env();
        let display_rules = match self.display {
            Some(Display::Simple(path)) => parser::table_expanded(path.as_path())?,
            Some(Display::Inline(text)) => {
                let rules = parser::table(text, None)?;
                parser::expand_includes(rules, &resolver, &[])?
            }
            Some(Display::List(paths)) => {
                let mut rules = Vec::new();
                for path in paths {
                    rules.extend(parser::table_expanded(path)?);
                }
                rules
            }
            None => vec![],
        };
        Ok(DisplayTable::compile(&display_rules, direction))
    }

    fn translation_table(
        &self,
        table: &Table,
        direction: Direction,
    ) -> Result<TranslationPipeline, TestError> {
        let resolver = SearchDirs::from_env();
        let rules = match table {
            Table::Simple(path) => parser::table_expanded(path.as_path())?,
            Table::List(paths) => {
                let mut rules = Vec::new();
                for path in paths {
                    rules.extend(parser::table_expanded(path)?);
                }
                rules
            }
            Table::Inline(text) => {
                let rules = parser::table(text, None)?;
                parser::expand_includes(rules, &resolver, &[])?
            }
            Table::Query(..) => return Err(TestError::NotImplemented("Table queries".to_string())),
        };
        Ok(TranslationPipeline::compile(&rules, direction)?)
    }

    pub fn check(&self) -> Result<Vec<TestResult>, TestError> {
        let mut results = Vec::new();
        match self.mode {
            TestMode::Forward => {
                let display_table = self.display_table(Direction::Forward)?;
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Forward)?;
                    results.extend(
                        self.tests
                            .par_iter()
                            .map(|test| test.check(&table, &display_table, Direction::Forward))
                            .collect::<Vec<_>>(),
                    );
                }
            }
            TestMode::Backward => {
                // ignore the backward test if LOUIS_TEST_FOWARD_ONLY is defined
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    let display_table = self.display_table(Direction::Backward)?;
                    for table in self.tables {
                        let table = self.translation_table(table, Direction::Backward)?;
                        results.extend(
                            self.tests
                                .par_iter()
                                .map(|test| test.check(&table, &display_table, Direction::Backward))
                                .collect::<Vec<_>>(),
                        );
                    }
                }
            }
            TestMode::BothDirections => {
                let display_table = self.display_table(Direction::Forward)?;
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Forward)?;
                    results.extend(
                        self.tests
                            .par_iter()
                            .map(|test| test.check(&table, &display_table, Direction::Forward))
                            .collect::<Vec<_>>(),
                    );
                }
                // ignore the backward test if LOUIS_TEST_FOWARD_ONLY is defined
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    let display_table = self.display_table(Direction::Backward)?;
                    // reverse the tests, i.e. swap `input` and `expected`
                    let reversed: Vec<Test> =
                        self.tests.iter().cloned().map(|t| t.reverse()).collect();
                    for table in self.tables {
                        let table = self.translation_table(table, Direction::Backward)?;
                        results.extend(
                            reversed
                                .par_iter()
                                .map(|test| test.check(&table, &display_table, Direction::Backward))
                                .collect::<Vec<_>>(),
                        );
                    }
                }
            }
            _ => (), // FIXME: not yet implemented
        }
        Ok(results)
    }
}

/// A braille table to be used in a [`Test`].
#[derive(Debug)]
pub enum Table {
    /// A (file based) braille table
    Simple(PathBuf),
    Query(TableQuery),
    /// A list of (file based) braille tables
    List(Vec<PathBuf>),
    /// A braille table that is defined inline in the YAML file
    Inline(String),
}

/// A display table to be used in a [`Test`].
#[derive(Debug)]
pub enum Display {
    /// A single (file based) display table
    Simple(PathBuf),
    /// A list of (file based) display tables
    List(Vec<PathBuf>),
    /// A display table that is defined inline in the YAML file
    Inline(String),
}

pub type TableQuery = HashMap<String, String>;

#[derive(Debug, Clone)]
pub enum CursorPosition {
    Single(usize),
    Tuple(usize, usize),
}

pub type Directions = EnumSet<Direction>;

#[derive(Debug, Clone)]
pub enum ExpectedFailure {
    Simple(bool),
    Reason(String),
    Direction(Directions),
}

impl ExpectedFailure {
    fn is_failure(&self, direction: Direction) -> bool {
        match &self {
            Self::Simple(v) => *v,
            Self::Reason(_) => true,
            Self::Direction(d) => d.contains(direction),
        }
    }
}

/// A test to verify a braille translation.
#[derive(Debug, Clone)]
pub struct Test {
    /// Input for the test
    input: String,
    /// Expected output of the test
    expected: String,
    /// Is the test expected to fail?
    xfail: ExpectedFailure,
    emphasis: Vec<EmphasisSpan>,
    expected_emphasis: Vec<EmphasisSpan>,
    input_pos: Vec<usize>,
    output_pos: Vec<usize>,
    cursor_pos: Option<CursorPosition>,
    modes: TranslationModes,
    max_output_length: Option<usize>,
    real_input_length: Option<usize>,
}

impl Test {
    fn check(
        &self,
        table: &TranslationPipeline,
        display_table: &DisplayTable,
        direction: Direction,
    ) -> TestResult {
        let mut options = TranslationOptions::default()
            .with_mode(self.modes.clone())
            .with_emphasis(self.emphasis.clone());
        if let Some(CursorPosition::Single(cursor) | CursorPosition::Tuple(cursor, _)) =
            self.cursor_pos
        {
            options = options.with_cursor_pos(cursor);
        }
        let (translated, positions) = match direction {
            // For forward translation we first translate the input and then apply the display table
            // on the result
            Direction::Forward => {
                let (translated, positions) = table.translate_with_positions(&self.input, &options);
                (display_table.translate(&translated), positions)
            }
            // For backward translation we first apply the display table on the input and then
            // translate the result
            Direction::Backward => {
                let displayed = display_table.translate(&self.input);
                table.translate_with_positions(&displayed, &options)
            }
        };
        let matched = translated == self.expected;
        if matched {
            if self.xfail.is_failure(direction) {
                TestResult::UnexpectedSuccess {
                    input: self.input.to_string(),
                    direction,
                }
            } else {
                match self.position_mismatch(&positions, direction) {
                    None => TestResult::Success,
                    Some(mismatch) => TestResult::Failure(FailureReason::Position(mismatch)),
                }
            }
        } else if self.xfail.is_failure(direction) {
            TestResult::ExpectedFailure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: translated,
                direction,
            }
        } else {
            TestResult::Failure(FailureReason::Translation {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: translated,
                direction,
            })
        }
    }

    fn position_mismatch(
        &self,
        positions: &PositionMap,
        direction: Direction,
    ) -> Option<PositionMismatch> {
        let mut diffs = Vec::new();
        if !self.input_pos.is_empty() && self.input_pos != positions.input_positions() {
            diffs.push(PositionDiff::InputPos {
                expected: self.input_pos.clone(),
                actual: positions.input_positions().to_vec(),
            });
        }
        if !self.output_pos.is_empty() && self.output_pos != positions.output_positions() {
            diffs.push(PositionDiff::OutputPos {
                expected: self.output_pos.clone(),
                actual: positions.output_positions().to_vec(),
            });
        }
        if let Some(CursorPosition::Tuple(cursor, expected)) = self.cursor_pos {
            let actual = positions.cursor(cursor);
            if actual != expected {
                diffs.push(PositionDiff::Cursor { expected, actual });
            }
        }
        if diffs.is_empty() {
            None
        } else {
            Some(PositionMismatch {
                input: self.input.clone(),
                direction,
                diffs,
            })
        }
    }

    pub fn new(
        input: String,
        expected: String,
        xfail: ExpectedFailure,
        emphasis: Vec<EmphasisSpan>,
        expected_emphasis: Vec<EmphasisSpan>,
        input_pos: Vec<usize>,
        output_pos: Vec<usize>,
        cursor_pos: Option<CursorPosition>,
        modes: TranslationModes,
        max_output_length: Option<usize>,
        real_input_length: Option<usize>,
    ) -> Self {
        Test {
            input,
            expected,
            xfail,
            emphasis,
            expected_emphasis,
            input_pos,
            output_pos,
            cursor_pos,
            modes,
            max_output_length,
            real_input_length,
        }
    }

    /// Create a reversed copy of the test by swapping input and expected values.
    ///
    /// This is useful for creating bidirectional test cases where you want to test
    /// both the forward translation (input → expected) and the reverse translation
    /// (expected → input).
    ///
    /// # Returns
    ///
    /// A new `Test` instance with `input` and `expected` swapped, while preserving
    /// all other fields.
    ///
    /// # Examples
    ///
    /// ```compile_fail
    /// let original = Test {
    ///     input: "hello".to_string(),
    ///     expected: "⠓⠑⠇⠇⠕".to_string(),
    ///     // ... other fields
    /// };
    ///
    /// let reversed = original.reverse();
    /// // reversed.input == "⠓⠑⠇⠇⠕"
    /// // reversed.expected == "hello"
    /// ```
    pub fn reverse(self) -> Self {
        Test {
            input: self.expected,
            expected: self.input,
            input_pos: self.output_pos,
            output_pos: self.input_pos,
            cursor_pos: None,
            ..self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_with_positions(
        input_pos: Vec<usize>,
        output_pos: Vec<usize>,
        cursor_pos: Option<CursorPosition>,
    ) -> Test {
        Test::new(
            "ab".to_string(),
            "⠁⠃".to_string(),
            ExpectedFailure::Simple(false),
            vec![],
            vec![],
            input_pos,
            output_pos,
            cursor_pos,
            TranslationModes::empty(),
            None,
            None,
        )
    }

    #[test]
    fn reverse_swaps_the_positions_and_drops_the_cursor() {
        let reversed =
            test_with_positions(vec![1], vec![2, 3], Some(CursorPosition::Single(0))).reverse();
        assert_eq!(reversed.input, "⠁⠃");
        assert_eq!(reversed.expected, "ab");
        assert_eq!(reversed.input_pos, [2, 3]);
        assert_eq!(reversed.output_pos, [1]);
        assert!(reversed.cursor_pos.is_none());
    }
}
