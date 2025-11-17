//! A parser for [liblouis](https://liblouis.io) YAML test files

use std::{collections::HashMap, fs::File, iter::Peekable, num::ParseIntError, path::PathBuf};

use crate::{parser, parser::EscapingContext, parser::unescape};

use enumset::EnumSet;
use libyaml::{Encoding, Event, Parser, ParserIter};

use crate::parser::Direction;
use crate::test::{
    CursorPosition, Directions, Display, ExpectedFailure, Table, TableQuery, Test, TestError,
    TestMatrix, TestMode, TestResult, TranslationMode, Typeform,
};

type YAMLEventError = Option<Result<Event, libyaml::ParserError>>;

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Scalar expected, got {0:?}")]
    ScalarExpected(YAMLEventError),
    #[error("YAML parse error")]
    YAMLError(#[from] libyaml::ParserError),
    #[error("Stream start expected")]
    StreamStartExpected,
    #[error("Stream end expected")]
    StreamEndExpected,
    #[error("Document start expected")]
    DocumentStartExpected,
    #[error("Document end expected")]
    DocumentEndExpected,
    #[error("Sequence start expected")]
    SequenceStartExpected,
    #[error("Sequence end expected, got {0:?}")]
    SequenceEndExpected(YAMLEventError),
    #[error("Mapping start expected, got {0:?}")]
    MappingStartExpected(YAMLEventError),
    #[error("Mapping end expected")]
    MappingEndExpected,
    #[error("Invalid test mode")]
    InvalidTestMode,
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("Encoding {0:?} not supported")]
    InvalidEncoding(Encoding),
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

pub struct YAMLParser<'a> {
    events: Peekable<ParserIter<'a>>,
}

impl YAMLParser<'_> {
    pub fn new(reader: File) -> Result<Self, ParseError> {
        let parser = Parser::new(reader)?;
        Ok(Self {
            events: parser.into_iter().peekable(),
        })
    }

    fn scalar(&mut self) -> Result<String, ParseError> {
        match self.events.next() {
            Some(Ok(Event::Scalar { value, .. })) => Ok(value),
            e => Err(ParseError::ScalarExpected(e)),
        }
    }

    fn mapping_start(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::MappingStart { .. })) => Ok(()),
            e => Err(ParseError::MappingStartExpected(e)),
        }
    }

    fn mapping_end(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::MappingEnd)) => Ok(()),
            _ => Err(ParseError::MappingEndExpected),
        }
    }

    fn sequence_start(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::SequenceStart { .. })) => Ok(()),
            _ => Err(ParseError::SequenceStartExpected),
        }
    }

    fn sequence_end(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::SequenceEnd)) => Ok(()),
            e => Err(ParseError::SequenceEndExpected(e)),
        }
    }

    fn display_table(&mut self) -> Result<Display, ParseError> {
        let value = self.scalar()?;
        let table = if value.contains(',') {
            let tables = value.split(',').map(|s| s.into()).collect();
            Display::List(tables)
        } else if value.contains('\n') {
            Display::Inline(value)
        } else {
            Display::Simple(value.into())
        };
        Ok(table)
    }

    fn table_query(&mut self) -> Result<TableQuery, ParseError> {
        self.mapping_start()?;
        let mut query: HashMap<String, String> = HashMap::new();
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
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
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let table = self.scalar()?;
            tables.push(table.into());
        }
        self.sequence_end()?;
        Ok(tables)
    }

    fn table(&mut self) -> Result<Table, ParseError> {
        let table = match self.events.peek() {
            Some(Ok(Event::MappingStart { .. })) => {
                let query = self.table_query()?;
                if query.contains_key("__assert-match") {
                    Table::Simple(query.get("__assert-match").unwrap().into())
                } else {
                    Table::Query(query)
                }
            }
            Some(Ok(Event::SequenceStart { .. })) => Table::List(self.table_list()?),
            Some(Ok(Event::Scalar { .. })) => {
                let value = self.scalar()?;
                // if the scalar contains newlines we assume it is an inline table
                if value.contains('\n') {
                    Table::Inline(value)
                } else {
                    Table::Simple(value.into())
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

    fn xfail_value(&mut self) -> Result<ExpectedFailure, ParseError> {
        let value = self.scalar()?;
        let value = match &*value {
            "off" | "false" => ExpectedFailure::Simple(false),
            "on" | "true" => ExpectedFailure::Simple(true),
            reason => ExpectedFailure::Reason(reason.to_string()),
        };
        Ok(value)
    }

    fn xfail_mapping_value(&mut self) -> Result<Directions, ParseError> {
        let mut directions = Directions::empty();
        self.mapping_start()?;
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let direction = self.scalar()?;
            let value = self.xfail_value()?;
            match &*direction {
                "forward" => match value {
                    ExpectedFailure::Simple(true) | ExpectedFailure::Reason(_) => {
                        directions.insert(Direction::Forward);
                    }
                    _ => (),
                },
                "backward" => match value {
                    ExpectedFailure::Simple(true) | ExpectedFailure::Reason(_) => {
                        directions.insert(Direction::Backward);
                    }
                    _ => (),
                },
                _ => {
                    return Err(ParseError::InvalidXFail);
                }
            }
        }
        self.mapping_end()?;
        Ok(directions)
    }

    fn xfail(&mut self) -> Result<ExpectedFailure, ParseError> {
        match self.events.peek() {
            Some(Ok(Event::MappingStart { .. })) => {
                Ok(ExpectedFailure::Direction(self.xfail_mapping_value()?))
            }
            Some(Ok(Event::Scalar { .. })) => self.xfail_value(),
            _ => Err(ParseError::InvalidXFail),
        }
    }

    fn typeform_value(&mut self) -> Result<Typeform, ParseError> {
        let mut typeform = HashMap::new();
        self.mapping_start()?;
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let key = self.scalar()?;
            let value = self.scalar()?;
            typeform.insert(key, value);
        }
        self.mapping_end()?;
        Ok(typeform)
    }

    fn u16_value(&mut self) -> Result<u16, ParseError> {
        let value = self.scalar()?;
        let value = value.parse::<u16>()?;
        Ok(value)
    }

    fn pos_values(&mut self) -> Result<Vec<u16>, ParseError> {
        let mut values = Vec::new();
        self.sequence_start()?;
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let value = self.u16_value()?;
            values.push(value);
        }
        self.sequence_end()?;
        Ok(values)
    }

    fn cursor_position(&mut self) -> Result<CursorPosition, ParseError> {
        let pos = match self.events.peek() {
            Some(Ok(Event::SequenceStart { .. })) => {
                self.sequence_start()?;
                let p = CursorPosition::Tuple(self.u16_value()?, self.u16_value()?);
                self.sequence_end()?;
                p
            }
            _ => CursorPosition::Single(self.u16_value()?),
        };
        Ok(pos)
    }

    fn translation_mode_value(&mut self) -> Result<TranslationMode, ParseError> {
        let value = self.scalar()?;
        let mode = match &*value {
            "noContractions" => TranslationMode::NoContractions,
            "compbrlAtCursor" => TranslationMode::CompbrlAtCursor,
            "dotsIO" => TranslationMode::DotsIo,
            "compbrlLeftCursor" => TranslationMode::CompbrlLeftCursor,
            "ucBrl" => TranslationMode::UcBrl,
            "noUndefined" => TranslationMode::NoUndefined,
            "partialTrans" => TranslationMode::PartialTrans,
            v => {
                return Err(ParseError::InvalidMode(v.to_string()));
            }
        };
        Ok(mode)
    }

    fn translation_modes(&mut self) -> Result<EnumSet<TranslationMode>, ParseError> {
        let mut modes = EnumSet::new();
        self.sequence_start()?;
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let mode = self.translation_mode_value()?;
            modes.insert(mode);
        }
        self.sequence_end()?;
        Ok(modes)
    }

    fn test(&mut self) -> Result<Test, ParseError> {
        self.sequence_start()?;
        let mut input = unescape(&self.scalar()?, EscapingContext::Default)?;
        let mut expected = self.scalar()?;
        // the YAML format is way too flexible: You can have two
        // scalars in which case those are input and expected. But you
        // can also have 3 scalars so that (description, input, expected)
        if let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let _description = Some(input);
            input = expected;
            expected = self.scalar()?;
        }
        // let input = self.scalar()?;
        // let expected = self.scalar()?;
        let mut xfail = ExpectedFailure::Simple(false);
        let mut typeform = HashMap::new();
        let mut input_pos: Vec<u16> = Vec::new();
        let mut output_pos: Vec<u16> = Vec::new();
        let mut cursor_pos = None;
        let mut modes = EnumSet::new();
        let mut max_output_length = None;
        let mut real_input_length = None;
        if let Some(Ok(Event::MappingStart { .. })) = self.events.peek() {
            self.mapping_start()?;
            while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
                let value = self.scalar()?;
                match &*value {
                    "xfail" => {
                        xfail = self.xfail()?;
                    }
                    "typeform" => {
                        typeform = self.typeform_value()?;
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
                        let length = self.u16_value()?;
                        max_output_length = Some(length);
                    }
                    "realInputLength" => {
                        let length = self.u16_value()?;
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
            typeform,
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
        while let Some(Ok(Event::SequenceStart { .. })) = self.events.peek() {
            tests.push(self.test()?);
        }
        self.sequence_end()?;
        Ok(tests)
    }

    fn stream_start(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::StreamStart {
                encoding: Some(encoding),
            })) => {
                if encoding == Encoding::Utf8 {
                    Ok(())
                } else {
                    Err(ParseError::InvalidEncoding(encoding))
                }
            }
            _ => Err(ParseError::StreamStartExpected),
        }
    }

    fn stream_end(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::StreamEnd)) => Ok(()),
            _ => Err(ParseError::StreamEndExpected),
        }
    }

    fn document_start(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::DocumentStart { .. })) => Ok(()),
            _ => Err(ParseError::DocumentStartExpected),
        }
    }

    fn document_end(&mut self) -> Result<(), ParseError> {
        match self.events.next() {
            Some(Ok(Event::DocumentEnd { .. })) => Ok(()),
            _ => Err(ParseError::DocumentEndExpected),
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
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
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
                    results.extend(suite.check()?);
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
