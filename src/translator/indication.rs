//! Braille indication
//!
//! Braille indicators are dot patterns which are inserted into the braille text
//! to indicate such things as capitalization, italic type, computer braille,
//! etc.
//!
//! Braille indication is handled with the help of a number of simple
//! state machines that keep track in which state a translation
//! currently is. When given the next pending character(s) to
//! translate, they keep track of state changes and will notify the
//! caller whether an indication is required by optionally returning
//! an [`Indication`].
//!
//! There are multiple state machines to keep track of different indication
//! requirements:
//! * [`numeric::Indicator`]: knowns whether the translation is in numeric mode
//! * [`uppercase::Indicator`]: knowns whether the translation is in uppercase mode

pub mod uppercase;
pub mod numeric;

/// Possible indication events that the indicator state machine(s) support
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Indication {
    NumericStart,
    NumericEnd,
    UppercaseStart,
    UppercaseEnd,
    EmphasisStart,
    EmphasisEnd,
}
