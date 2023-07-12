//! Test braille translation using tests defined in YAML files

use crate::{translate, translator::Direction};

use std::path::PathBuf;

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
        if let TestResult::Failure { .. } = self {
            return true;
        } else {
            return false;
        }
    }
}

#[derive(Default)]
pub struct TestSuite {
    tests: Vec<Test>,
    direction: Direction,
}

impl TestSuite {
    pub fn check(&self) -> Vec<TestResult> {
        match self.direction {
            Direction::Forward => {
                return self.tests.iter().map(|t| t.check()).collect();
            }
            Direction::Backward => return vec![TestResult::Error],
        }
    }
}

pub struct Test {
    // FIXME: instead of a reference to a file a test should rather
    // contain something that can be constructed in a test such as a
    // TranslationTable
    table: PathBuf,
    input: String,
    expected: String,
    xfail: bool,
}

impl Test {
    fn check(&self) -> TestResult {
        if let Ok(actual) = translate(self.table.clone(), &self.input) {
            if actual == self.expected {
                if !self.xfail {
                    return TestResult::Success;
                } else {
                    return TestResult::UnexpectedSuccess {
                        input: self.input.to_string(),
                    };
                }
            } else {
                if self.xfail {
                    return TestResult::ExpectedFailure {
                        input: self.input.to_string(),
                        expected: self.expected.to_string(),
                        actual,
                    };
                } else {
                    return TestResult::Failure {
                        input: self.input.to_string(),
                        expected: self.expected.to_string(),
                        actual,
                    };
                }
            }
        } else {
            return TestResult::Error;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_test() {
        let test = Test {
            table: PathBuf::from("tests/test_table.txt"),
            input: "some text".to_string(),
            expected: "some braille".to_string(),
            xfail: false,
        };
        let test_suite = TestSuite {
            direction: Direction::Forward,
            tests: vec![test],
        };
        let result = TestResult::Failure {
            input: "some text".to_string(),
            expected: "some braille".to_string(),
            actual: "⠀⠀⠀⠀⠀⠀⠀⠀⠀".to_string(),
        };
        assert_eq!(test_suite.check(), vec![result]);
    }
}
