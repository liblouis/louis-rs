//! A parser for [liblouis](https://liblouis.io) YAML test files

use std::{
    collections::HashMap, fs::File, io::Read, iter::Peekable, num::ParseIntError, path::PathBuf,
    vec,
};

use louis::internals::{parser, parser::EscapingContext, parser::unescape};

use saphyr_parser::{BufferedInput, Event, Parser, ScalarStyle, ScanError, Span};

use crate::test::{
    CursorPosition, Display, Expectation, ExpectedFailure, Table, TableQuery, Test, TestError,
    TestMatrix, TestMode, TestResult,
};
use louis::{Emphasis, EmphasisSpan};
use louis::{TranslationMode, TranslationModes};

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("line {line}: {expected} expected, found {found}")]
    Unexpected {
        expected: &'static str,
        found: String,
        line: usize,
    },
    #[error("unexpected end of YAML input")]
    UnexpectedEnd,
    #[error(transparent)]
    YAMLError(#[from] ScanError),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("Invalid test mode")]
    InvalidTestMode,
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("Invalid translation mode {0:?}")]
    InvalidMode(String),
    #[error("Invalid xfail value")]
    InvalidXFail,
    #[error("Invalid table value")]
    InvalidTableValue,
    #[error("Invalid token {0:?}")]
    InvalidToken(String),
    #[error(transparent)]
    TestError(#[from] TestError),
    #[error("invalid escape sequence {0:?}")]
    InvalidEscape(#[from] parser::ParseError),
}

pub struct YAMLParser {
    events: Peekable<Parser<'static, BufferedInput<vec::IntoIter<char>>>>,
}

impl YAMLParser {
    pub fn new(mut reader: File) -> Result<Self, ParseError> {
        // read_to_string rejects input that is not UTF-8
        let mut content = String::new();
        reader.read_to_string(&mut content)?;
        let chars: Vec<char> = content.chars().collect();
        Ok(Self {
            events: Parser::new_from_iter(chars.into_iter()).peekable(),
        })
    }

    /// The next event. A YAML syntax error is reported as such rather than as an
    /// unexpected event.
    fn next_event(&mut self) -> Result<(Event<'static>, Span), ParseError> {
        Ok(self.events.next().ok_or(ParseError::UnexpectedEnd)??)
    }

    fn unexpected(expected: &'static str, (event, span): (Event, Span)) -> ParseError {
        ParseError::Unexpected {
            expected,
            found: format!("{event:?}"),
            line: span.start.line(),
        }
    }

    fn scalar(&mut self) -> Result<String, ParseError> {
        match self.next_event()? {
            (Event::Scalar(value, ..), _) => Ok(value.into_owned()),
            e => Err(Self::unexpected("scalar", e)),
        }
    }

    fn mapping_start(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::MappingStart(..), _) => Ok(()),
            e => Err(Self::unexpected("mapping start", e)),
        }
    }

    fn mapping_end(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::MappingEnd, _) => Ok(()),
            e => Err(Self::unexpected("mapping end", e)),
        }
    }

    fn sequence_start(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::SequenceStart(..), _) => Ok(()),
            e => Err(Self::unexpected("sequence start", e)),
        }
    }

    fn sequence_end(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::SequenceEnd, _) => Ok(()),
            e => Err(Self::unexpected("sequence end", e)),
        }
    }

    /// A plain scalar can name several tables at once in the form of a comma separated
    /// list. Returns `None` when the scalar names a single table. Sequences and block
    /// literals are handled by the callers.
    fn comma_separated(value: &str) -> Option<Vec<PathBuf>> {
        value
            .contains(',')
            .then(|| value.split(',').map(|s| s.into()).collect())
    }

    fn display_table(&mut self) -> Result<Display, ParseError> {
        let table = match self.events.peek() {
            Some(Ok((Event::SequenceStart(..), _))) => Display::List(self.table_list()?),
            // a block literal (`|`) or folded (`>`) scalar is an inline table
            Some(Ok((Event::Scalar(_, ScalarStyle::Literal | ScalarStyle::Folded, ..), _))) => {
                Display::Inline(self.scalar()?)
            }
            Some(Ok((Event::Scalar(..), _))) => {
                let value = self.scalar()?;
                match Self::comma_separated(&value) {
                    Some(tables) => Display::List(tables),
                    None => Display::Simple(value.into()),
                }
            }
            _ => {
                return Err(ParseError::InvalidTableValue);
            }
        };
        Ok(table)
    }

    fn table_query(&mut self) -> Result<TableQuery, ParseError> {
        self.mapping_start()?;
        let mut query: HashMap<String, String> = HashMap::new();
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let key = self.scalar()?;
            let value = self.scalar()?;
            query.insert(key, value);
        }
        self.mapping_end()?;
        Ok(query)
    }

    fn table_list(&mut self) -> Result<Vec<PathBuf>, ParseError> {
        self.sequence_start()?;
        let mut tables = Vec::new();
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let table = self.scalar()?;
            tables.push(table.into());
        }
        self.sequence_end()?;
        Ok(tables)
    }

    fn table(&mut self) -> Result<Table, ParseError> {
        let table = match self.events.peek() {
            Some(Ok((Event::MappingStart(..), _))) => {
                let query = self.table_query()?;
                if query.contains_key("__assert-match") {
                    Table::Simple(query.get("__assert-match").unwrap().into())
                } else {
                    Table::Query(query)
                }
            }
            Some(Ok((Event::SequenceStart(..), _))) => Table::List(self.table_list()?),
            // a block literal (`|`) or folded (`>`) scalar is an inline table
            Some(Ok((Event::Scalar(_, ScalarStyle::Literal | ScalarStyle::Folded, ..), _))) => {
                Table::Inline(self.scalar()?)
            }
            Some(Ok((Event::Scalar(..), _))) => {
                let value = self.scalar()?;
                match Self::comma_separated(&value) {
                    Some(tables) => Table::List(tables),
                    None => Table::Simple(value.into()),
                }
            }
            _ => {
                return Err(ParseError::InvalidTableValue);
            }
        };
        Ok(table)
    }

    fn flags(&mut self) -> Result<TestMode, ParseError> {
        self.mapping_start()?;
        let value = self.scalar()?;
        if value != "testmode" {
            return Err(ParseError::InvalidTestMode);
        }
        let mode = self.scalar()?;
        let mode = match &*mode {
            "forward" => TestMode::Forward,
            "backward" => TestMode::Backward,
            "bothDirections" => TestMode::BothDirections,
            "display" => TestMode::Display,
            "hyphenate" => TestMode::Hyphenate,
            "hyphenateBraille" => TestMode::HyphenateBraille,
            _ => {
                return Err(ParseError::InvalidTestMode);
            }
        };
        self.mapping_end()?;
        Ok(mode)
    }

    fn xfail_value(&mut self) -> Result<Expectation, ParseError> {
        let value = self.scalar()?;
        let value = match &*value {
            "off" | "false" => Expectation::Pass,
            "on" | "true" => Expectation::Fail(None),
            reason => Expectation::Fail(Some(reason.to_string())),
        };
        Ok(value)
    }

    fn xfail_mapping_value(&mut self) -> Result<ExpectedFailure, ParseError> {
        let mut forward = Expectation::Pass;
        let mut backward = Expectation::Pass;
        self.mapping_start()?;
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let direction = self.scalar()?;
            let value = self.xfail_value()?;
            match &*direction {
                "forward" => forward = value,
                "backward" => backward = value,
                _ => {
                    return Err(ParseError::InvalidXFail);
                }
            }
        }
        self.mapping_end()?;
        Ok(ExpectedFailure::new(forward, backward))
    }

    fn xfail(&mut self) -> Result<ExpectedFailure, ParseError> {
        match self.events.peek() {
            Some(Ok((Event::MappingStart(..), _))) => self.xfail_mapping_value(),
            Some(Ok((Event::Scalar(..), _))) => Ok(ExpectedFailure::both(self.xfail_value()?)),
            _ => Err(ParseError::InvalidXFail),
        }
    }

    fn emphasis_value(&mut self) -> Result<Emphasis, ParseError> {
        let mut spans: Vec<EmphasisSpan> = Vec::new();
        self.mapping_start()?;
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let class = self.scalar()?;
            let mask = self.scalar()?;
            // a span per `+` in the mask ("++--++"), which `Emphasis` merges into runs
            for (i, ch) in mask.chars().enumerate() {
                if ch == '+' {
                    spans.push(EmphasisSpan::new(class.clone(), i..i + 1));
                }
            }
        }
        self.mapping_end()?;
        Ok(spans.into_iter().collect())
    }

    fn usize_value(&mut self) -> Result<usize, ParseError> {
        let value = self.scalar()?;
        let value = value.parse::<usize>()?;
        Ok(value)
    }

    /// A single `inputPos`/`outputPos` value.
    ///
    /// liblouis leaves these at the `-1` it pre-fills its position arrays with when the
    /// translation output is empty, i.e. when an input char has no output position to point at
    /// (see `outputPos[k] = -1` in liblouis' `lou_translateString.c`). That only ever happens
    /// for the whole array at once, and we express the same thing as position 0, so normalize
    /// the sentinel away here instead of carrying it into the comparison.
    fn pos_value(&mut self) -> Result<usize, ParseError> {
        let value = self.scalar()?;
        match value.parse::<usize>() {
            Ok(position) => Ok(position),
            Err(_) if value.parse::<i64>().is_ok_and(i64::is_negative) => Ok(0),
            Err(e) => Err(e.into()),
        }
    }

    fn pos_values(&mut self) -> Result<Vec<usize>, ParseError> {
        let mut values = Vec::new();
        self.sequence_start()?;
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let value = self.pos_value()?;
            values.push(value);
        }
        self.sequence_end()?;
        Ok(values)
    }

    fn cursor_position(&mut self) -> Result<CursorPosition, ParseError> {
        let pos = match self.events.peek() {
            Some(Ok((Event::SequenceStart(..), _))) => {
                self.sequence_start()?;
                let p = CursorPosition::Tuple(self.usize_value()?, self.usize_value()?);
                self.sequence_end()?;
                p
            }
            _ => CursorPosition::Single(self.usize_value()?),
        };
        Ok(pos)
    }

    fn translation_mode_value(&mut self) -> Result<TranslationMode, ParseError> {
        let value = self.scalar()?;
        let mode = match &*value {
            "noContractions" => TranslationMode::NoContractions,
            "compbrlAtCursor" => TranslationMode::CompbrlAtCursor,
            "compbrlLeftCursor" => TranslationMode::CompbrlLeftCursor,
            "noUndefined" => TranslationMode::NoUndefined,
            "partialTrans" => TranslationMode::PartialTrans,
            v => {
                return Err(ParseError::InvalidMode(v.to_string()));
            }
        };
        Ok(mode)
    }

    fn translation_modes(&mut self) -> Result<TranslationModes, ParseError> {
        let mut modes = TranslationModes::empty();
        self.sequence_start()?;
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let mode = self.translation_mode_value()?;
            modes.insert(mode);
        }
        self.sequence_end()?;
        Ok(modes)
    }

    fn test(&mut self) -> Result<Test, ParseError> {
        self.sequence_start()?;
        // FIXME: liblouis test files use \xHHHH escape sequences inside single-quoted YAML
        // strings. Single-quoted strings are literal in standard YAML and should not be
        // unescaped, so the test files are technically wrong. We apply the unescaping anyway
        // to stay compatible with liblouis's own test runner. The real fix is to update the
        // liblouis test files to use double-quoted strings or literal Unicode characters.
        let mut input = unescape(&self.scalar()?, EscapingContext::Default)?;
        let mut expected = unescape(&self.scalar()?, EscapingContext::Default)?;
        // the YAML format is way too flexible: You can have two
        // scalars in which case those are input and expected. But you
        // can also have 3 scalars so that (description, input, expected)
        if let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let _description = Some(input);
            input = expected;
            expected = unescape(&self.scalar()?, EscapingContext::Default)?;
        }
        let mut xfail = ExpectedFailure::default();
        let mut emphasis = Emphasis::default();
        let mut expected_emphasis: Option<Emphasis> = None;
        let mut input_pos: Vec<usize> = Vec::new();
        let mut output_pos: Vec<usize> = Vec::new();
        let mut cursor_pos = None;
        let mut modes = TranslationModes::empty();
        let mut max_output_length = None;
        let mut real_input_length = None;
        if let Some(Ok((Event::MappingStart(..), _))) = self.events.peek() {
            self.mapping_start()?;
            while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
                let value = self.scalar()?;
                match &*value {
                    "xfail" => {
                        xfail = self.xfail()?;
                    }
                    "typeform" => {
                        emphasis = self.emphasis_value()?;
                    }
                    "expected_typeform" => {
                        expected_emphasis = Some(self.emphasis_value()?);
                    }
                    "inputPos" => {
                        input_pos = self.pos_values()?;
                    }
                    "outputPos" => {
                        output_pos = self.pos_values()?;
                    }
                    "cursorPos" => {
                        let pos = self.cursor_position()?;
                        cursor_pos = Some(pos);
                    }
                    "mode" => {
                        modes = self.translation_modes()?;
                    }
                    "maxOutputLength" => {
                        let length = self.usize_value()?;
                        max_output_length = Some(length);
                    }
                    "realInputLength" => {
                        let length = self.usize_value()?;
                        real_input_length = Some(length);
                    }
                    _ => {
                        return Err(ParseError::InvalidToken(value));
                    }
                }
            }
            self.mapping_end()?;
        };
        self.sequence_end()?;
        Ok(Test::new(
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
        ))
    }

    fn tests(&mut self) -> Result<Vec<Test>, ParseError> {
        let mut tests: Vec<Test> = Vec::new();
        self.sequence_start()?;
        while let Some(Ok((Event::SequenceStart(..), _))) = self.events.peek() {
            tests.push(self.test()?);
        }
        self.sequence_end()?;
        Ok(tests)
    }

    fn stream_start(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::StreamStart, _) => Ok(()),
            e => Err(Self::unexpected("stream start", e)),
        }
    }

    fn stream_end(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::StreamEnd, _) => Ok(()),
            e => Err(Self::unexpected("stream end", e)),
        }
    }

    fn document_start(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::DocumentStart(..), _) => Ok(()),
            e => Err(Self::unexpected("document start", e)),
        }
    }

    fn document_end(&mut self) -> Result<(), ParseError> {
        match self.next_event()? {
            (Event::DocumentEnd, _) => Ok(()),
            e => Err(Self::unexpected("document end", e)),
        }
    }

    pub fn yaml(&mut self) -> Result<Vec<TestResult>, ParseError> {
        let mut results = Vec::new();
        let mut current_display_table = None;
        let mut current_tables = Vec::new();
        let mut test_mode: TestMode = TestMode::Forward;
        let mut previous_event_was_table = false;

        self.stream_start()?;
        self.document_start()?;
        self.mapping_start()?;
        while let Some(Ok((Event::Scalar(..), _))) = self.events.peek() {
            let value = self.scalar()?;
            match &*value {
                "display" => {
                    current_display_table = Some(self.display_table()?);
                    previous_event_was_table = false;
                }
                "table" => {
                    // if multiple table definitions are in sequence then we
                    // collect all tables and run the following tests for all of
                    // them. Otherwise we replace the previous table definition
                    // with the current one.
                    if previous_event_was_table {
                        current_tables.push(self.table()?);
                    } else {
                        current_tables.clear();
                        current_tables.push(self.table()?);
                    }
                    previous_event_was_table = true;
                }
                "flags" => {
                    test_mode = self.flags()?;
                    previous_event_was_table = false;
                }
                "tests" => {
                    let tests = self.tests()?;
                    let suite = TestMatrix::new(
                        &current_display_table,
                        &current_tables,
                        &test_mode,
                        &tests,
                    );
                    // A block whose table will not load is reported as one error
                    // among the results rather than aborting the whole file.
                    match suite.check() {
                        Ok(test_results) => results.extend(test_results),
                        Err(error) => results.push(TestResult::Error(error.to_string())),
                    }
                    // flags apply to the next tests block only
                    test_mode = TestMode::Forward;
                    previous_event_was_table = false;
                }
                _ => {
                    return Err(ParseError::InvalidToken(value));
                }
            }
        }
        self.mapping_end()?;
        self.document_end()?;
        self.stream_end()?;
        Ok(results)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use crate::test::{FailureReason, PositionDiff};
    use louis::Direction;

    fn run(name: &str, yaml: &str) -> Result<Vec<TestResult>, ParseError> {
        let path =
            std::env::temp_dir().join(format!("louis-rs-{}-{}.yaml", std::process::id(), name));
        File::create(&path)
            .unwrap()
            .write_all(yaml.as_bytes())
            .unwrap();
        let results = YAMLParser::new(File::open(&path).unwrap())?.yaml();
        let _ = std::fs::remove_file(&path);
        results
    }

    /// liblouis leaves its position array at the `-1` it was pre-filled with when the output is
    /// empty, i.e. when there is no output position to point at. We express that as position 0,
    /// so such an expectation has to match rather than be reported as a mismatch.
    #[test]
    fn the_no_output_position_sentinel_matches_an_empty_output() {
        let yaml = "table: |\n  letter f 124\n  noback correct [\"f\"] ?\ntests:\n  - - f\n    - \"\"\n    - inputPos: []\n      outputPos: [-1]\n";
        let results = run("empty_output_sentinel", yaml).unwrap();
        assert_eq!(results.len(), 1);
        assert!(results[0].is_success());
    }

    const AB_TABLE: &str = "table: |\n  letter a 1\n  letter b 12\n";

    #[test]
    fn flags_apply_to_the_next_tests_block_only() {
        let yaml = format!(
            "{AB_TABLE}flags: {{testmode: backward}}\ntests:\n  - [⠁⠃, ab]\ntests:\n  - [ab, ⠁⠃]\n"
        );
        let results = run("flags_per_block", &yaml).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|result| result.is_success()));
    }

    #[test]
    fn matching_positions_are_not_reported() {
        let yaml = format!(
            "{AB_TABLE}tests:\n  - - ab\n    - ⠁⠃\n    - inputPos: [0, 1]\n      outputPos: [0, 1]\n      cursorPos: [1, 1]\n"
        );
        let results = run("matching_positions", &yaml).unwrap();
        assert_eq!(results.len(), 1);
        assert!(results[0].is_success());
    }

    #[test]
    fn mismatching_positions_are_reported_separately() {
        let yaml = format!(
            "{AB_TABLE}tests:\n  - - ab\n    - ⠁⠃\n    - inputPos: [1, 1]\n      outputPos: [0, 0]\n      cursorPos: [1, 0]\n"
        );
        let results = run("mismatching_positions", &yaml).unwrap();
        assert!(results[0].is_position_failure());
        let TestResult::Failure(FailureReason::Position(mismatch)) = &results[0] else {
            panic!("expected a position failure");
        };
        assert_eq!(mismatch.input(), "ab");
        assert_eq!(mismatch.direction(), Direction::Forward);
        assert_eq!(
            mismatch.diffs(),
            &[
                PositionDiff::InputPos {
                    expected: vec![1, 1],
                    actual: vec![0, 1]
                },
                PositionDiff::OutputPos {
                    expected: vec![0, 0],
                    actual: vec![0, 1]
                },
                PositionDiff::Cursor {
                    expected: 0,
                    actual: 1
                },
            ]
        );
    }

    #[test]
    fn single_cursor_position_is_not_checked() {
        let yaml = format!("{AB_TABLE}tests:\n  - - ab\n    - ⠁⠃\n    - cursorPos: 1\n");
        let results = run("single_cursor", &yaml).unwrap();
        assert!(results[0].is_success());
    }

    #[test]
    fn positions_are_not_checked_when_the_translation_fails() {
        let yaml = format!("{AB_TABLE}tests:\n  - - ab\n    - ⠁⠁\n    - inputPos: [5, 5]\n");
        let results = run("failed_translation", &yaml).unwrap();
        assert!(results[0].is_translation_failure());
    }

    #[test]
    fn expected_failures_report_their_reason() {
        let yaml = "table: |\n  letter a 1\n  letter b 12\nflags: {testmode: bothDirections}\ntests:\n  - [ab, ⠃⠁, {xfail: {forward: wrong order, backward: true}}]\n  - [ab, ⠃⠁, {xfail: also wrong}]\n";
        let results = run("xfail_reasons", yaml).unwrap();
        let reasons: Vec<_> = results
            .iter()
            .map(|result| match result {
                TestResult::ExpectedFailure { reason, .. } => reason.as_deref(),
                other => panic!("expected failure, got {other:?}"),
            })
            .collect();
        // all forward results first, then all backward ones
        assert_eq!(
            reasons,
            [
                Some("wrong order"),
                Some("also wrong"),
                None,
                Some("also wrong")
            ]
        );
    }

    #[test]
    fn both_directions_swap_the_expected_positions() {
        let yaml = "table: |\n  letter a 1\n  letter b 12\n  always ab 123\nflags: {testmode: bothDirections}\ntests:\n  - - ab\n    - ⠇\n    - inputPos: [0]\n      outputPos: [0, 0]\n";
        let results = run("both_directions", yaml).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|r| r.is_success()));
    }
}
