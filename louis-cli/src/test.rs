//! A test runner for tests defined in [liblouis](https://liblouis.io) YAML test files

use std::{collections::HashMap, path::PathBuf};

use rayon::prelude::*;

use search_path::SearchPath;

use louis::Emphasis;
use louis::internals::{
    parser::{self, Direction, TableError},
    translator::{
        self, DisplayTable, PositionMap, TranslationModes, TranslationOptions, TranslationPipeline,
    },
};

use louis::metadata;

#[derive(thiserror::Error, Debug)]
pub enum TestError {
    #[error("Errors in table {0:?}")]
    TableErrors(Vec<TableError>),
    #[error("Error when compiling table {0:?}")]
    CompilationError(#[from] translator::CompileError),
    #[error("Could not index the tables: {0}")]
    NotIndexable(String),
    #[error("No table matches the query {0}")]
    NoTableForQuery(String),
    #[error("The query {0} matches more than one table: {1:?}")]
    AmbiguousTableQuery(String, Vec<PathBuf>),
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
    /// The text matched, but the emphasis of the output (its typeform) didn't
    Emphasis {
        input: String,
        expected: Emphasis,
        actual: Emphasis,
        direction: Direction,
    },
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
        reason: Option<String>,
    },
    UnexpectedSuccess {
        input: String,
        direction: Direction,
    },
    /// A `tests:` block that could not be run at all, because its table failed
    /// to load or compile. Recorded per block so that one unusable table does
    /// not discard every other block in the same file.
    Error(String),
}

impl TestResult {
    pub fn is_success(&self) -> bool {
        matches!(self, TestResult::Success)
    }
    pub fn is_translation_failure(&self) -> bool {
        matches!(
            self,
            TestResult::Failure(FailureReason::Translation { .. } | FailureReason::Emphasis { .. })
        )
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
    pub fn is_error(&self) -> bool {
        matches!(self, TestResult::Error(_))
    }
}

#[derive(PartialEq, Debug)]
pub struct PositionMismatch {
    input: String,
    direction: Direction,
    diffs: Vec<PositionDiff>,
}

// only the tests read a mismatch field by field
#[cfg(test)]
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
        let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
        let display_rules = match self.display {
            Some(Display::Simple(path)) => parser::table_expanded_in(path.as_path(), &search_path)?,
            Some(Display::Inline(text)) => {
                let rules = parser::table(text, None)?;
                parser::expand_includes(rules, &search_path, &[])?
            }
            Some(Display::List(paths)) => {
                let mut rules = Vec::new();
                for path in paths {
                    rules.extend(parser::table_expanded_in(path, &search_path)?);
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
        let search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
        let rules = match table {
            Table::Simple(path) => parser::table_expanded_in(path.as_path(), &search_path)?,
            Table::List(paths) => {
                let mut rules = Vec::new();
                for path in paths {
                    rules.extend(parser::table_expanded_in(path, &search_path)?);
                }
                rules
            }
            Table::Inline(text) => {
                let rules = parser::table(text, None)?;
                parser::expand_includes(rules, &search_path, &[])?
            }
            Table::Query(query) => {
                let index = crate::index().map_err(|e| TestError::NotIndexable(e.to_string()))?;
                let query: metadata::Query =
                    query.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                let matches: Vec<PathBuf> =
                    index.find(&query).into_iter().map(PathBuf::from).collect();
                match matches.as_slice() {
                    [] => return Err(TestError::NoTableForQuery(query.to_string())),
                    [path] => parser::table_expanded_in(path, &search_path)?,
                    several => {
                        return Err(TestError::AmbiguousTableQuery(
                            query.to_string(),
                            several.to_vec(),
                        ));
                    }
                }
            }
        };
        // the display table is a pipeline stage on the braille side, so the pipeline
        // applies it in the right place for `direction` and it composes into the positions
        Ok(TranslationPipeline::compile(&rules, direction)?
            .with_display(self.display_table(direction)?))
    }

    pub fn check(&self) -> Result<Vec<TestResult>, TestError> {
        let mut results = Vec::new();
        match self.mode {
            TestMode::Forward => {
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Forward)?;
                    results.extend(
                        self.tests
                            .par_iter()
                            .map(|test| test.check(&table, Direction::Forward))
                            .collect::<Vec<_>>(),
                    );
                }
            }
            TestMode::Backward => {
                // ignore the backward test if LOUIS_TEST_FOWARD_ONLY is defined
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    for table in self.tables {
                        let table = self.translation_table(table, Direction::Backward)?;
                        results.extend(
                            self.tests
                                .par_iter()
                                .map(|test| test.check(&table, Direction::Backward))
                                .collect::<Vec<_>>(),
                        );
                    }
                }
            }
            TestMode::BothDirections => {
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Forward)?;
                    results.extend(
                        self.tests
                            .par_iter()
                            .map(|test| test.check(&table, Direction::Forward))
                            .collect::<Vec<_>>(),
                    );
                }
                // ignore the backward test if LOUIS_TEST_FOWARD_ONLY is defined
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    // reverse the tests, i.e. swap `input` and `expected`
                    let reversed: Vec<Test> =
                        self.tests.iter().cloned().map(|t| t.reverse()).collect();
                    for table in self.tables {
                        let table = self.translation_table(table, Direction::Backward)?;
                        results.extend(
                            reversed
                                .par_iter()
                                .map(|test| test.check(&table, Direction::Backward))
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

/// Whether a test is expected to pass or fail in one direction
#[derive(Debug, Clone, Default, PartialEq)]
pub enum Expectation {
    #[default]
    Pass,
    /// Expected to fail, for the reason given, if any
    Fail(Option<String>),
}

/// What a test is expected to do in each direction (its `xfail`)
#[derive(Debug, Clone, Default)]
pub struct ExpectedFailure {
    forward: Expectation,
    backward: Expectation,
}

impl ExpectedFailure {
    pub fn new(forward: Expectation, backward: Expectation) -> Self {
        Self { forward, backward }
    }

    /// The same expectation in both directions
    pub fn both(expectation: Expectation) -> Self {
        Self::new(expectation.clone(), expectation)
    }

    fn get(&self, direction: Direction) -> &Expectation {
        match direction {
            Direction::Forward => &self.forward,
            Direction::Backward => &self.backward,
        }
    }

    fn is_failure(&self, direction: Direction) -> bool {
        matches!(self.get(direction), Expectation::Fail(_))
    }

    fn reason(&self, direction: Direction) -> Option<String> {
        match self.get(direction) {
            Expectation::Fail(reason) => reason.clone(),
            Expectation::Pass => None,
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
    emphasis: Emphasis,
    expected_emphasis: Option<Emphasis>,
    input_pos: Vec<usize>,
    output_pos: Vec<usize>,
    cursor_pos: Option<CursorPosition>,
    modes: TranslationModes,
    max_output_length: Option<usize>,
    real_input_length: Option<usize>,
}

impl Test {
    fn check(&self, table: &TranslationPipeline, direction: Direction) -> TestResult {
        let mut options = TranslationOptions::default()
            .with_modes(self.modes)
            .with_emphasis(self.emphasis.clone());
        if let Some(CursorPosition::Single(cursor) | CursorPosition::Tuple(cursor, _)) =
            self.cursor_pos
        {
            options = options.with_cursor_position(cursor);
        }
        let (translated, positions, emphasis) = table.translate_with_details(&self.input, &options);
        let matched = translated == self.expected;
        if matched {
            if self.xfail.is_failure(direction) {
                TestResult::UnexpectedSuccess {
                    input: self.input.to_string(),
                    direction,
                }
            } else {
                match self.position_mismatch(&positions, direction) {
                    Some(mismatch) => TestResult::Failure(FailureReason::Position(mismatch)),
                    None => match self.emphasis_mismatch(emphasis.unwrap_or_default(), direction) {
                        Some(failure) => TestResult::Failure(failure),
                        None => TestResult::Success,
                    },
                }
            }
        } else if self.xfail.is_failure(direction) {
            TestResult::ExpectedFailure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: translated,
                direction,
                reason: self.xfail.reason(direction),
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

    /// Compare the emphasis of the output, where the test gives one (`expected_typeform`)
    fn emphasis_mismatch(&self, actual: Emphasis, direction: Direction) -> Option<FailureReason> {
        let expected = self.expected_emphasis.as_ref()?;
        (*expected != actual).then(|| FailureReason::Emphasis {
            input: self.input.to_string(),
            expected: expected.clone(),
            actual,
            direction,
        })
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
        emphasis: Emphasis,
        expected_emphasis: Option<Emphasis>,
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
            ExpectedFailure::default(),
            Emphasis::default(),
            None,
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
