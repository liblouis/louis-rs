//! Text formatting attributes for braille translation.
//!
//! This is what liblouis calls a "typeform" — per-character metadata that
//! drives emphasis indication (italic, bold, etc.) in braille output. In
//! the liblouis C API, the equivalent type is `formtype` (a `u16` bitmask)
//! and the individual bit values are defined in the `typeforms` enum.
//!
//! A [`TextAttributes`] value (an [`EnumSet<TextAttribute>`]) is attached to
//! each input character. The translator inspects these to decide which
//! emphasis indicator cells to insert.

use enumset::{EnumSet, EnumSetType};

/// A single text formatting attribute.
///
/// In liblouis terminology this is one bit of a `typeform` / `formtype` value.
#[derive(EnumSetType, Debug)]
pub enum TextAttribute {
    Italic,
    Underline,
    Bold,
    /// Table-defined emphasis class 4
    Emph4,
    /// Table-defined emphasis class 5
    Emph5,
    /// Table-defined emphasis class 6
    Emph6,
    /// Table-defined emphasis class 7
    Emph7,
    /// Table-defined emphasis class 8
    Emph8,
    /// Table-defined emphasis class 9
    Emph9,
    /// Table-defined emphasis class 10
    Emph10,
    ComputerBraille,
    PassageBreak,
    WordReset,
    Script,
    TransNote,
    TransNote1,
    TransNote2,
    TransNote3,
    TransNote4,
    TransNote5,
}

/// The set of text attributes active at a single character position.
///
/// Equivalent to one element of the `typeform` array in the liblouis C API
/// (`formtype`, a `u16` bitmask). One `TextAttributes` per input character is
/// passed to the translator alongside the input string.
pub type TextAttributes = EnumSet<TextAttribute>;
