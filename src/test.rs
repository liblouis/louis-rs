use std::{collections::HashMap, path::PathBuf};

use enumset::{EnumSet, EnumSetType};
use search_path::SearchPath;

use crate::{
    parser::{self, Direction},
    translator::TranslationTable,
};

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
    Error,
    NotImplemented,
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
    fn check(&self) -> Vec<TestResult> {
        let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
        let mut results = Vec::new();
        for path in &self.paths {
            if let Some(path) = search_path.find_file(path) {
                for direction in self.directions {
                    if let Ok(rules) = parser::table(path.as_path()) {
                        if let Ok(rules) = parser::expand_includes(search_path, rules) {
                            let table = TranslationTable::compile(rules, *direction);
                            for test in self.tests {
                                results.push(test.check(&table, *direction));
                            }
                        } else {
                            results.push(TestResult::Error);
                        }
                    } else {
                        results.push(TestResult::Error);
                    }
                }
            } else {
                results.push(TestResult::Error);
            }
        }
        results
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
    pub fn check(&self) -> Vec<TestResult> {
        let directions = match self.mode {
            TestMode::Forward => vec![Direction::Forward],
            TestMode::Backward => vec![Direction::Backward],
            TestMode::BothDirections => vec![Direction::Forward, Direction::Backward],
            _ => vec![],
        };
        let paths = match &self.table {
            Table::Simple(path) => vec![path],
            _ => return vec![TestResult::NotImplemented],
        };
        let matrix = TestMatrix {
            paths,
            directions: &directions,
            tests: self.tests,
        };
        matrix.check()
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
