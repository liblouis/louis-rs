use std::{collections::HashMap, path::PathBuf};

use enumset::{EnumSet, EnumSetType};
use search_path::SearchPath;

use crate::{
    parser::{self, Direction, TableError},
    translator::TranslationTable,
};

#[derive(thiserror::Error, Debug)]
pub enum TestError {
    #[error("Not implemented (yet)")]
    NotImplemented,
    #[error("Errors in table {0:?}")]
    TableErrors(Vec<TableError>),
    #[error("Table {0:?} not found")]
    TableNotFound(PathBuf),
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

struct TestMatrix<'a> {
    paths: Vec<&'a PathBuf>,
    directions: &'a Vec<Direction>,
    tests: &'a Vec<Test>,
}

impl<'a> TestMatrix<'a> {
    fn check(&self) -> Result<Vec<TestResult>, TestError> {
        let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
        let mut results = Vec::new();
        for path in &self.paths {
            let path = search_path
                .find_file(path)
                .ok_or(TestError::TableNotFound(path.into()))?;
            for direction in self.directions {
                let rules = parser::table(path.as_path())?;
                let rules = parser::expand_includes(search_path, rules)?;
                let table = TranslationTable::compile(rules, *direction);
                for test in self.tests {
                    results.push(test.check(&table, *direction));
                }
            }
        }
        Ok(results)
    }
}

#[derive(Debug)]
pub struct TestSuite<'a> {
    pub display_table: &'a Option<PathBuf>,
    pub table: &'a Table,
    pub mode: &'a TestMode,
    pub tests: &'a Vec<Test>,
}

impl<'a> TestSuite<'a> {
    pub fn check(&self) -> Result<Vec<TestResult>, TestError> {
        let directions = match self.mode {
            TestMode::Forward => vec![Direction::Forward],
            TestMode::Backward => vec![Direction::Backward],
            TestMode::BothDirections => vec![Direction::Forward, Direction::Backward],
            _ => vec![],
        };
        let paths = match &self.table {
            Table::Simple(path) => vec![path],
            _ => return Err(TestError::NotImplemented),
        };
        let matrix = TestMatrix {
            paths,
            directions: &directions,
            tests: self.tests,
        };
        Ok(matrix.check()?)
    }
}

#[derive(Debug)]
pub enum Table {
    Simple(PathBuf),
    Query(TableQuery),
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

#[derive(Debug)]
pub enum CursorPosition {
    Single(u16),
    Tuple(u16, u16),
}

pub type Directions = EnumSet<Direction>;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Test {
    pub input: String,
    pub expected: String,
    pub xfail: ExpectedFailure,
    pub typeform: Typeform,
    pub input_pos: Vec<u16>,
    pub output_pos: Vec<u16>,
    pub cursor_pos: Option<CursorPosition>,
    pub modes: EnumSet<TranslationMode>,
    pub max_output_length: Option<u16>,
    pub real_input_length: Option<u16>,
}

impl Test {
    fn check(&self, table: &TranslationTable, direction: Direction) -> TestResult {
        let translated = table.translate(&self.input);
        if translated == self.expected {
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
                actual: translated,
            }
        } else {
            TestResult::Failure {
                input: self.input.to_string(),
                expected: self.expected.to_string(),
                actual: translated,
            }
        }
    }
}
