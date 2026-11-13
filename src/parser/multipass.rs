//! Parsers for the test and action operands of context and multipass opcodes
use std::{iter::Peekable, num::ParseIntError, str::Chars};

use super::braille;

pub use action::Action;
pub use test::Test;

pub mod action;
pub mod test;

/// An error that occurred during parsing of a multipass or context rule
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("expected {expected:?}, got {found:?}")]
    CharExpected { expected: char, found: Option<char> },
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("invalid range: min ({min}) >= max ({max})")]
    InvalidRange { min: u8, max: u8 },
    #[error("invalid braille")]
    InvalidBraille(#[from] braille::ParseError),
    #[error("invalid test")]
    InvalidTest { found: Option<char> },
    #[error("empty test")]
    EmptyTest,
    #[error("invalid action")]
    InvalidAction { found: Option<char> },
    #[error("empty action")]
    EmptyAction,
    #[error("invalid class name")]
    InvalidClass,
    #[error("invalid attribute")]
    InvalidAttribute { found: Option<char> },
    #[error("invalid variable name")]
    InvalidVariableName,
    #[error("invalid quantifier, expected a number, a range or a dot")]
    InvalidQuantifier,
    #[error("invalid operator")]
    InvalidOperator { found: Option<char> },
    #[error("invalid escape sequence")]
    InvalidEscapeSequence { found: Option<char> },
    #[error("double negation ('!!') is not allowed")]
    DoubleNegation,
    #[error("negating a replacement ('![...]') is not allowed")]
    NegatedReplacement,
    #[error(
        "'_' must come before the tests it steps back over, or after them at the end, and step over tests of fixed width only"
    )]
    MisplacedLookback,
}

/// The characters of a test or action operand, read one at a time by both operand
/// parsers, which also decodes the escape sequences in a quoted string.
struct Scanner<'a>(Peekable<Chars<'a>>);

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Self(source.chars().peekable())
    }

    fn peek(&mut self) -> Option<&char> {
        self.0.peek()
    }

    fn next_if_eq(&mut self, expected: &char) -> Option<char> {
        self.0.next_if_eq(expected)
    }

    fn consume(&mut self, expected: char) -> Result<(), ParseError> {
        match self.0.next() {
            Some(e) if e == expected => Ok(()),
            Some(c) => Err(ParseError::CharExpected {
                expected,
                found: Some(c),
            }),
            _ => Err(ParseError::CharExpected {
                expected,
                found: None,
            }),
        }
    }

    fn ascii_number(&mut self) -> Result<u8, ParseError> {
        let mut s = String::new();
        while self.0.peek().filter(|c| c.is_ascii_digit()).is_some() {
            s.push(self.0.next().unwrap());
        }
        let number = s.parse::<u8>()?;
        Ok(number)
    }

    /// Returns the character `c` stands for, reading the rest of the escape sequence
    /// when `c` is a backslash.
    fn handle_escape_sequence(&mut self, c: char) -> Result<char, ParseError> {
        if c != '\\' {
            return Ok(c);
        }
        match self.0.peek().copied() {
            Some('x') => {
                self.0.next();
                self.unescape_unicode(4)
            }
            Some('y') => {
                self.0.next();
                self.unescape_unicode(5)
            }
            Some(escaped) => {
                let decoded = if escaped == '"' {
                    Some('"')
                } else {
                    super::simple_escape(escaped)
                };
                if let Some(decoded) = decoded {
                    self.0.next();
                    Ok(decoded)
                } else {
                    // Preserve unknown escapes. The next character is still part of the string.
                    Ok(c)
                }
            }
            None => Err(ParseError::InvalidEscapeSequence { found: None }),
        }
    }

    fn unescape_unicode(&mut self, len: usize) -> Result<char, ParseError> {
        let mut s = String::new();
        for _ in 0..len {
            match self.0.next() {
                Some(c) => s.push(c),
                None => return Err(ParseError::InvalidEscapeSequence { found: None }),
            }
        }
        u32::from_str_radix(&s, 16)
            .ok()
            .and_then(char::from_u32)
            .ok_or(ParseError::InvalidEscapeSequence { found: None })
    }
}

impl Iterator for Scanner<'_> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.0.next()
    }
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ConversionError {
    #[error("cannot convert test operand to string")]
    TestNotLiteral,
    #[error("cannot convert action operand to string")]
    ActionNotLiteral,
}

/// Determines if the `test` or `action` operand of a `context` or `multipass` opcode represents
/// just a literal string match.
///
/// Such operands can be optimized to simple string matching operations.
pub trait IsLiteral {
    /// Returns `true` if this operand matches only literal characters.
    fn is_literal(&self) -> bool;
}
#[cfg(test)]
mod tests {
    use super::{Scanner, action, test};

    #[test]
    fn ascii_number() {
        assert_eq!(Scanner::new("123").ascii_number(), Ok(123));
        assert_eq!(Scanner::new("1").ascii_number(), Ok(1));
        assert_eq!(Scanner::new("1 ").ascii_number(), Ok(1));
        assert_eq!(Scanner::new("1abc").ascii_number(), Ok(1));
        assert_ne!(Scanner::new(" 1").ascii_number(), Ok(1));
        assert_ne!(Scanner::new("a1").ascii_number(), Ok(1));
    }

    #[test]
    fn quoted_operands_decode_the_same_escapes() {
        for (operand, expected) in [
            (r#""a\e\f\n\r\t\vb""#, "a\u{1b}\u{c}\n\r\t\u{b}b"),
            (r#""\s\"\\\x0061\y1f600λ""#, " \"\\a😀λ"),
        ] {
            let tests = test::Parser::new(operand).tests().unwrap();
            let actions = action::Parser::new(operand).actions().unwrap();
            assert_eq!(String::try_from(&tests).unwrap(), expected);
            assert_eq!(String::try_from(&actions).unwrap(), expected);
        }
    }
}
