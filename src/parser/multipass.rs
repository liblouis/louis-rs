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
    #[error("Expected {expected:?}, got {found:?}")]
    CharExpected { expected: char, found: Option<char> },
    #[error("Invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("Invalid range: min ({min}) >= max ({max})")]
    InvalidRange { min: u8, max: u8 },
    #[error("Invalid braille")]
    InvalidBraille(#[from] braille::ParseError),
    #[error("Invalid test")]
    InvalidTest { found: Option<char> },
    #[error("Empty test")]
    EmptyTest,
    #[error("Invalid action")]
    InvalidAction { found: Option<char> },
    #[error("Empty action")]
    EmptyAction,
    #[error("Invalid class name")]
    InvalidClass,
    #[error("Invalid attribute")]
    InvalidAttribute { found: Option<char> },
    #[error("Invalid variable name")]
    InvalidVariableName,
    #[error("Invalid quantifier, expected a number, a range or a dot")]
    InvalidQuantifier,
    #[error("Invalid operator")]
    InvalidOperator { found: Option<char> },
    #[error("Invalid escape sequence")]
    InvalidEscapeSequence { found: Option<char> },
    #[error("double negation ('!!') is not allowed")]
    DoubleNegation,
    #[error("negating a replacement ('![...]') is not allowed")]
    NegatedReplacement,
}

// The opening and closing quotes are consumed by the operand parser.
fn string_char(c: char, chars: &mut Peekable<Chars<'_>>) -> Result<char, ParseError> {
    if c != '\\' {
        return Ok(c);
    }
    match chars.peek().copied() {
        Some(prefix @ ('x' | 'y')) => {
            chars.next();
            let len = if prefix == 'x' { 4 } else { 5 };
            let mut digits = String::new();
            for _ in 0..len {
                digits.push(
                    chars
                        .next()
                        .ok_or(ParseError::InvalidEscapeSequence { found: None })?,
                );
            }
            u32::from_str_radix(&digits, 16)
                .ok()
                .and_then(char::from_u32)
                .ok_or(ParseError::InvalidEscapeSequence { found: None })
        }
        Some(escaped) => {
            let decoded = if escaped == '"' {
                Some('"')
            } else {
                super::simple_escape(escaped)
            };
            if let Some(decoded) = decoded {
                chars.next();
                Ok(decoded)
            } else {
                // Preserve unknown escapes. The next character is still part of the string.
                Ok(c)
            }
        }
        None => Err(ParseError::InvalidEscapeSequence { found: None }),
    }
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ConversionError {
    #[error("Cannot convert test operand to string")]
    TestNotLiteral,
    #[error("Cannot convert action operand to string")]
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

/// Determines if the `test` operand (or an `action` operand in the case of back-translation) of a
/// `context` or `multipass` opcode consumes any input.
pub trait ConsumesInput {
    /// Returns `true` if this operand consumes any input.
    fn consumes_input(&self) -> bool;
}

#[cfg(test)]
mod tests {
    use super::{action, test};

    #[test]
    fn quoted_operands_decode_the_same_escapes() {
        for (operand, expected) in [
            (r#""a\e\f\n\r\t\vb""#, "a\u{1b}\u{c}\n\r\t\u{b}b"),
            (r#""\s\"\\\x0061\y1f600λ""#, " \"\\a😀λ"),
            (r#""\q\w""#, r"\q\w"),
        ] {
            let tests = test::Parser::new(operand).tests().unwrap();
            let actions = action::Parser::new(operand).actions().unwrap();
            assert_eq!(String::try_from(&tests).unwrap(), expected);
            assert_eq!(String::try_from(&actions).unwrap(), expected);
        }
    }
}
