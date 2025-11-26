//! Parsers for the test and action operands of context and multipass opcodes
use std::num::ParseIntError;

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
