use std::{collections::HashSet, iter::Peekable, num::ParseIntError, str::Chars};

use super::BrailleChars;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected:?}, got {found:?}")]
    CharExpected { expected: char, found: Option<char> },
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("Expected a number, a range or a dot")]
    QuantifierExpected,
    #[error("Unknown error")]
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Test {
    only_at_beginning: bool,
    only_at_end: bool,
    tests: Vec<TestInstruction>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Attribute {
    Digit,
    Litdigit,
    Letter,
    Sign,
    Space,
    Math,
    Punctuation,
    Uppercase,
    Lowercase,
    Class1,
    Class2,
    Class3,
    Class4,
    Any,
}

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
}

#[derive(Debug, PartialEq, Eq)]
enum Quantifier {
    Number(u8),
    Range(u8, u8),
    Any,
}

#[derive(Debug, PartialEq, Eq)]
enum TestInstruction {
    Lookback {
        len: u8,
    },
    Variable {
        var: u8,
        op: Operator,
        number: u8,
    },
    String {
        s: String,
    },
    Dots {
        dots: BrailleChars,
    },
    Attributes {
        attrs: HashSet<Attribute>,
        quantifier: Option<Quantifier>,
    },
    Class {
        name: String,
        quantifier: Option<Quantifier>,
    },
    Negate {
        instr: Box<TestInstruction>,
    },
    Replace {
        instr: Vec<TestInstruction>,
    },
}

pub struct TestParser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> TestParser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }

    fn ascii_number(&mut self) -> Result<u8, ParseError> {
        let mut s = String::new();
        while self.chars.peek().filter(|c| c.is_ascii_digit()).is_some() {
            s.push(self.chars.next().unwrap());
        }
        let number = s.parse::<u8>()?;
        Ok(number)
    }

    fn consume(&mut self, expected: char) -> Result<(), ParseError> {
        match self.chars.next() {
            Some(e) if e == expected => Ok(()),
            Some(c) => Err(ParseError::CharExpected {
                expected: '_',
                found: Some(c),
            }),
            _ => Err(ParseError::CharExpected {
                expected: '_',
                found: None,
            }),
        }
    }

    fn quantifier(&mut self) -> Result<Option<Quantifier>, ParseError> {
        match self.chars.peek() {
            Some('.') => {
                self.chars.next();
                Ok(Some(Quantifier::Any))
            }
            Some(c) if c.is_ascii_digit() => {
                let min = self.ascii_number()?;
                if self.chars.next_if_eq(&'-').is_some() {
                    let max = self.ascii_number()?;
                    Ok(Some(Quantifier::Range(min, max)))
                } else {
                    Ok(Some(Quantifier::Number(min)))
                }
            }
            _ => Ok(None),
        }
    }

    fn class(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('%')?;
        let mut name = String::new();
        while self.chars.peek().filter(|c| c.is_alphabetic()).is_some() {
            name.push(self.chars.next().unwrap());
        }
        Ok(TestInstruction::Class {
            name,
            quantifier: self.quantifier()?,
        })
    }

    fn lookback(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('_')?;
        let n = match self.chars.peek() {
            Some(c) if c.is_ascii_digit() => self.ascii_number()?,
            _ => 1,
        };
        Ok(TestInstruction::Lookback { len: n })
    }

    pub fn test(&mut self) -> Result<TestInstruction, ParseError> {
        match self.chars.peek() {
            Some('_') => Ok(self.lookback()?),
            Some('%') => Ok(self.class()?),
            _ => Err(ParseError::Unknown),
        }
    }

    pub fn tests(&mut self) -> Result<Test, ParseError> {
        let only_at_beginning = self.chars.next_if_eq(&'`').is_some();
        let mut tests: Vec<TestInstruction> = Vec::new();
        while let Some(c) = self.chars.peek() {
            tests.push(self.test()?);
        }
        let only_at_end = self.chars.next_if_eq(&'~').is_some();
        Ok(Test {
            only_at_beginning,
            only_at_end,
            tests,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascii_number_test() {
        assert_eq!(TestParser::new("123").ascii_number(), Ok(123));
        assert_eq!(TestParser::new("1").ascii_number(), Ok(1));
        assert_eq!(TestParser::new("1 ").ascii_number(), Ok(1));
        assert_eq!(TestParser::new("1abc").ascii_number(), Ok(1));
        assert_ne!(TestParser::new(" 1").ascii_number(), Ok(1));
        assert_ne!(TestParser::new("a1").ascii_number(), Ok(1));
    }

    #[test]
    fn look_back_test() {
        assert_eq!(
            TestParser::new("_123").lookback(),
            Ok(TestInstruction::Lookback { len: 123 })
        );
        assert_eq!(
            TestParser::new("_7").lookback(),
            Ok(TestInstruction::Lookback { len: 7 })
        );
        assert_eq!(
            TestParser::new("_ ").lookback(),
            Ok(TestInstruction::Lookback { len: 1 })
        );
        assert_eq!(
            TestParser::new("_abc").lookback(),
            Ok(TestInstruction::Lookback { len: 1 })
        );
        assert_eq!(
            TestParser::new("_12abc").lookback(),
            Ok(TestInstruction::Lookback { len: 12 })
        );
        assert_eq!(
            TestParser::new(" _ ").lookback(),
            Err(ParseError::CharExpected {
                expected: '_',
                found: Some(' ')
            })
        );
    }
}
