use std::{fs, path::PathBuf};

#[derive(PartialEq)]
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

pub struct Test {
    input: String,
    expected: String,
}

pub fn check(tests: Vec<Test>) -> Vec<TestResult> {
    Vec::new()
}

pub fn check_yaml(yaml: PathBuf) -> Vec<TestResult> {
    let tests = fs::read_to_string(yaml).expect("Cannot read yaml file");
    let tests = Vec::new();
    check(tests)
}
