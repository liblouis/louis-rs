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
//! [`ResolvedTranslation`](crate::translator::ResolvedTranslation).
//!
//! There are multiple state machines to keep track of different indication
//! requirements:
//! * [`numeric::Indicator`]: knowns whether the translation is in numeric mode
//! * [`uppercase::Indicator`]: knowns whether the translation is in uppercase mode
//! * [`lettersign::Indicator`]: indicates that the following braille cells are not to be read as a contraction
//! * [`nocontract::Indicator`]: indicates that the following braille cells are not to be read as a contraction

use crate::translator::ResolvedTranslation;

pub mod lettersign;
pub mod nocontract;
pub mod numeric;
pub mod uppercase;

#[derive(Debug, Clone)]
pub enum Indicator {
    LetterSign(lettersign::Indicator),
    NoContract(nocontract::Indicator),
    Numeric(numeric::Indicator),
    Uppercase(uppercase::Indicator),
}

impl Indicator {
    pub fn next(&mut self, s: &str, prev: Option<char>) -> Option<ResolvedTranslation> {
        match self {
            Self::LetterSign(i) => i.next(s, prev),
            Self::NoContract(i) => i.next(s, prev),
            Self::Numeric(i) => i.next(s),
            Self::Uppercase(i) => i.next(s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Indicators(Vec<Indicator>);

impl Indicators {
    pub fn new(indicators: Vec<Indicator>) -> Indicators {
        Indicators(indicators)
    }

    pub fn next(&mut self, s: &str, prev: Option<char>) -> Vec<ResolvedTranslation> {
        self.0.iter_mut().flat_map(|i| i.next(s, prev)).collect()
    }
}
