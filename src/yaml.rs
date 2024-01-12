use std::{collections::HashMap, fs::File, iter::Peekable, num::ParseIntError, path::PathBuf};

use enumset::EnumSet;
use libyaml::{Encoding, Event, Parser, ParserIter};

use crate::parser::Direction;
use crate::test::{
    CursorPosition, Directions, ExpectedFailure, Table, TableQuery, Test, TestError, TestMode,
    TestResult, TestSuite, TranslationMode, Typeform,
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
    #[error("Invalid token {0:?}")]
    InvalidToken(String),
    #[error("Error while running tests")]
    TestError(#[from] TestError),
}

pub struct YAMLParser<'a> {
    events: Peekable<ParserIter<'a>>,
}

impl<'a> YAMLParser<'a> {
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

    fn display_table(&mut self) -> Result<PathBuf, ParseError> {
        let value = self.scalar()?;
        Ok(value.into())
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

    fn table_inline(&mut self) -> Result<String, ParseError> {
        let table = self.scalar()?;
        Ok(table)
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
            _ => Table::Inline(self.table_inline()?),
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

    fn xfail_value(&mut self) -> Result<bool, ParseError> {
        let value = self.scalar()?;
        if value == "off" || value == "false" {
            Ok(false)
        } else {
            Ok(true)
        }
    }

    fn xfail_mapping_value(&mut self) -> Result<Directions, ParseError> {
        let mut directions = Directions::empty();
        self.mapping_start()?;
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let direction = self.scalar()?;
            let value = self.xfail_value()?;
            match &*direction {
                "forward" => {
                    if value {
                        directions.insert(Direction::Forward);
                    };
                }
                "backward" => {
                    if value {
                        directions.insert(Direction::Forward);
                    };
                }
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
            Some(Ok(Event::Scalar { .. })) => Ok(ExpectedFailure::Simple(self.xfail_value()?)),
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
        let pos = if let Some(Ok(Event::SequenceStart { .. })) = self.events.peek() {
            self.sequence_start()?;
            let p = CursorPosition::Tuple(self.u16_value()?, self.u16_value()?);
            self.sequence_end()?;
            p
        } else {
            CursorPosition::Single(self.u16_value()?)
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
        let mut input = self.scalar()?;
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
        if let Some(Ok(Event::StreamStart {
            encoding: Some(encoding),
        })) = self.events.next()
        {
            if encoding == Encoding::Utf8 {
                Ok(())
            } else {
                Err(ParseError::InvalidEncoding(encoding))
            }
        } else {
            Err(ParseError::StreamStartExpected)
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

        self.stream_start()?;
        self.document_start()?;
        self.mapping_start()?;
        while let Some(Ok(Event::Scalar { .. })) = self.events.peek() {
            let value = self.scalar()?;
            match &*value {
                "display" => {
                    current_display_table = Some(self.display_table()?);
                }
                "table" => {
                    current_tables.push(self.table()?);
                }
                "flags" => {
                    test_mode = self.flags()?;
                }
                "tests" => {
                    let tests = self.tests()?;
                    for table in &current_tables {
                        let suite =
                            TestSuite::new(&current_display_table, table, &test_mode, &tests);
                        results.extend(suite.check()?);
                    }
                    current_tables.clear();
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
