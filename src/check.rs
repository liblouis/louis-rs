//! Test braille translation using tests defined in YAML files

use crate::{
    compile, compile_display, display, translate,
    translator::{Direction, DisplayTable, TranslationTable},
};

use serde::Deserialize;

use std::{collections::HashSet, path::PathBuf};

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
}

impl TestResult {
    pub fn is_failure(&self) -> bool {
        matches!(self, TestResult::Failure { .. })
    }
}

#[derive(Default, Debug, PartialEq, Deserialize)]
pub struct TestSuite {
    display: PathBuf,
    // FIXME: instead of a reference to a file a test should rather
    // contain something that can be constructed in a test such as a
    // TranslationTable
    table: PathBuf,
    directions: HashSet<Direction>,
    tests: Vec<Test>,
}

impl TestSuite {
    pub fn check(&self) -> Vec<TestResult> {
        let translation_table = compile(&self.table).unwrap();
        let display_table = compile_display(&self.display).unwrap();
        self.tests
            .iter()
            .map(|t| t.check(&translation_table, &display_table))
            .collect()
        //        self.tests.iter().map(|t| t.check(self.table.clone(), self.display.clone())).collect()
        //        match self.direction {
        //            Direction::Forward => {
        //                return self.tests.iter().map(|t| t.check(self.table.clone())).collect();
        //            }
        //            Direction::Backward => return vec![TestResult::Error],
        //        }
    }
}

#[derive(Default, Debug, PartialEq, Deserialize)]
pub struct Test {
    input: String,
    expected: String,
    xfail: bool,
}

impl Test {
    fn check(&self, table: &TranslationTable, display_table: &DisplayTable) -> TestResult {
        let translated = translate(table, &self.input);
        let displayed = display(display_table, &translated);
        if displayed == self.expected {
            if !self.xfail {
                TestResult::Success
            } else {
                TestResult::UnexpectedSuccess {
                    input: self.input.to_string(),
                }
            }
        } else if self.xfail {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_test() {
        let test = Test {
            input: "some text".to_string(),
            expected: "some braille".to_string(),
            xfail: false,
        };
        let test_suite = TestSuite {
            table: PathBuf::from("tests/test_table.txt"),
            display: PathBuf::from("tests/test_display.txt"),
            tests: vec![test],
            directions: HashSet::from([Direction::Forward]),
        };
        let result = TestResult::Failure {
            input: "some text".to_string(),
            expected: "some braille".to_string(),
            actual: "         ".to_string(),
        };
        assert_eq!(test_suite.check(), vec![result]);
    }
}
