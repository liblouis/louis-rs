//! Braille indication
//!
//! Braille indicators are dot patterns which are inserted into the braille text
//! to indicate such things as capitalization, italic type, computer braille,
//! etc.
//!
//! Braille indication is handled with the help of a number of simple
//! state machines that keep track which state a translation currently
//! is in. When given the next pending character(s) to translate, they
//! keep track of state changes and will notify the caller whether an
//! indication is required by optionally returning a
//! [`Translation`](crate::translator::Translation).
//!
//! There are multiple state machines to keep track of different indication
//! requirements:
//! * [`numeric::Indicator`]: knowns whether the translation is in numeric mode
//! * [`uppercase::Indicator`]: knowns whether the translation is in uppercase mode
//! * [`lettersign::Indicator`]: indicates that the following braille cells are not to be read as a contraction
//! * [`nocontract::Indicator`]: indicates that the following braille cells are not to be read as a contraction

pub mod lettersign;
pub mod nocontract;
pub mod numeric;
pub mod uppercase;
