use std::{collections::HashMap, path::PathBuf};

use enumset::{EnumSet, EnumSetType};

use crate::{
    parser::{self, Direction, TableError},
    translator::{self, DisplayTable, TranslationTable},
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
    },
    ExpectedFailure {
        input: String,
        expected: String,
        actual: String,
    },
    UnexpectedSuccess {
        input: String,
    },
}

impl TestResult {
    pub fn is_success(&self) -> bool {
        matches!(self, TestResult::Success { .. })
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

#[derive(Debug)]
pub struct TestMatrix<'a> {
    display: &'a Option<Display>,
    tables: &'a Vec<Table>,
    mode: &'a TestMode,
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
        Ok(DisplayTable::compile(display_rules, direction))
    }

    fn translation_table(
        &self,
        table: &Table,
        direction: Direction,
    ) -> Result<TranslationTable, TestError> {
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
        Ok(TranslationTable::compile(rules, direction)?)
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
                let display_table = self.display_table(Direction::Backward)?;
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Backward)?;
                    for test in self.tests {
                        results.push(test.check(&table, &display_table, Direction::Backward));
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
                let display_table = self.display_table(Direction::Backward)?;
                for table in self.tables {
                    let table = self.translation_table(table, Direction::Backward)?;
                    for test in self.tests.iter().cloned().map(|t| t.reverse()) {
                        results.push(test.check(&table, &display_table, Direction::Backward));
                    }
                }
            }
            _ => (), // FIXME: not yet implemented
        }
        Ok(results)
    }
}

#[derive(Debug)]
pub enum Table {
    Simple(PathBuf),
    Query(TableQuery),
    List(Vec<PathBuf>),
    Inline(String),
}

#[derive(Debug)]
pub enum Display {
    Simple(PathBuf),
    List(Vec<PathBuf>),
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

#[derive(Debug, Clone)]
pub struct Test {
    input: String,
    expected: String,
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
        table: &TranslationTable,
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
                }
            }
        } else if self.xfail.is_failure(direction) {
            TestResult::ExpectedFailure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: displayed,
            }
        } else {
            TestResult::Failure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: displayed,
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

    pub fn reverse(self) -> Self {
        Test {
            input: self.expected,
            expected: self.input,
            ..self
        }
    }
}
