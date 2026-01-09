//! A test runner for tests defined in [liblouis](https://liblouis.io) YAML test files

use std::{collections::HashMap, path::PathBuf};

use enumset::{EnumSet, EnumSetType};

use crate::{
    parser::{self, Direction, TableError},
    translator::{self, DisplayTable, TranslationPipeline},
};

#[derive(thiserror::Error, Debug)]
pub enum TestError {
    #[error("Not implemented (yet)")]
    NotImplemented,
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

#[derive(PartialEq, Debug)]
pub enum TestResult {
    Success,
    Failure {
        input: String,
        expected: String,
        actual: String,
        direction: Direction,
    },
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
    pub fn is_failure(&self) -> bool {
        matches!(self, TestResult::Failure { .. })
    }
    pub fn is_expected_failure(&self) -> bool {
        matches!(self, TestResult::ExpectedFailure { .. })
    }
    pub fn is_unexpected_success(&self) -> bool {
        matches!(self, TestResult::UnexpectedSuccess { .. })
    }
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
        let display_rules = match self.display {
            Some(Display::Simple(path)) => parser::table_expanded(path.as_path())?,
            Some(Display::Inline(text)) => {
                let rules = parser::table(text, None)?;
                parser::expand_includes(rules)?
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
                parser::expand_includes(rules)?
            }
            Table::Query(..) => return Err(TestError::NotImplemented),
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
                    for test in self.tests {
                        results.push(test.check(&table, &display_table, Direction::Forward));
                    }
                }
            }
            TestMode::Backward => {
                // ignore the backward test if LOUIS_TEST_FOWARD_ONLY is defined
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    let display_table = self.display_table(Direction::Backward)?;
                    for table in self.tables {
                        let table = self.translation_table(table, Direction::Backward)?;
                        for test in self.tests {
                            results.push(test.check(&table, &display_table, Direction::Backward));
                        }
                    }
                }
            }
            TestMode::BothDirections => {
                let display_table = self.display_table(Direction::Forward)?;
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Forward)?;
                    for test in self.tests {
                        results.push(test.check(&table, &display_table, Direction::Forward));
                    }
                }
                // ignore the backward test if LOUIS_TEST_FOWARD_ONLY is defined
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    let display_table = self.display_table(Direction::Backward)?;
                    for table in self.tables {
                        let table = self.translation_table(table, Direction::Backward)?;
                        // reverse the tests, i.e. swap `input` and `expected`
                        for test in self.tests.iter().cloned().map(|t| t.reverse()) {
                            results.push(test.check(&table, &display_table, Direction::Backward));
                        }
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

#[derive(EnumSetType, Debug)]
pub enum TranslationMode {
    NoContractions,
    CompbrlAtCursor,
    DotsIo,
    CompbrlLeftCursor,
    UcBrl,
    NoUndefined,
    PartialTrans,
}

pub type Typeform = HashMap<String, String>;

#[derive(Debug, Clone)]
pub enum CursorPosition {
    Single(u16),
    Tuple(u16, u16),
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

/// A test to verify a braille translation.$
#[derive(Debug, Clone)]
pub struct Test {
    /// Input for the test
    input: String,
    /// Expected output of the test
    expected: String,
    /// Is the test expected to fail?
    xfail: ExpectedFailure,
    typeform: Typeform,
    input_pos: Vec<u16>,
    output_pos: Vec<u16>,
    cursor_pos: Option<CursorPosition>,
    modes: EnumSet<TranslationMode>,
    max_output_length: Option<u16>,
    real_input_length: Option<u16>,
}

impl Test {
    fn check(
        &self,
        table: &TranslationPipeline,
        display_table: &DisplayTable,
        direction: Direction,
    ) -> TestResult {
        let translated = table.translate(&self.input);
        let displayed = display_table.translate(&translated);
        if displayed == self.expected {
            if !self.xfail.is_failure(direction) {
                TestResult::Success
            } else {
                TestResult::UnexpectedSuccess {
                    input: self.input.to_string(),
                    direction,
                }
            }
        } else if self.xfail.is_failure(direction) {
            TestResult::ExpectedFailure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: displayed,
                direction,
            }
        } else {
            TestResult::Failure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: displayed,
                direction,
            }
        }
    }

    pub fn new(
        input: String,
        expected: String,
        xfail: ExpectedFailure,
        typeform: HashMap<String, String>,
        input_pos: Vec<u16>,
        output_pos: Vec<u16>,
        cursor_pos: Option<CursorPosition>,
        modes: EnumSet<TranslationMode>,
        max_output_length: Option<u16>,
        real_input_length: Option<u16>,
    ) -> Self {
        Test {
            input,
            expected,
            xfail,
            typeform,
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
    /// ```
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
            ..self
        }
    }
}
