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
use events::IndicationEvents;

pub mod events;
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

    pub fn precompute(&self, input: &str) -> IndicationEvents {
        match self {
            Self::LetterSign(i) => i.precompute(input),
            Self::NoContract(i) => i.precompute(input),
            Self::Numeric(i) => i.precompute(input),
            Self::Uppercase(i) => i.precompute(input),
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

    pub fn precompute(&self, input: &str) -> IndicationEvents {
        self.0
            .iter()
            .map(|i| i.precompute(input))
            .fold(IndicationEvents::default(), |acc, e| acc | e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{AnchoredRule, RuleParser};
    use crate::translator::indication::events::{IndicationEvent, IndicationEvents};
    use std::collections::HashSet;

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    #[test]
    fn precompute_combines_indicators() {
        let mut num_builder = numeric::IndicatorBuilder::new();
        num_builder.numeric_characters(HashSet::from(['1', '2', '3']));
        num_builder.numsign("⠼", &rule("numsign 3456"));
        let numeric = Indicator::Numeric(num_builder.build().unwrap());

        let mut cap_builder = uppercase::IndicatorBuilder::new();
        cap_builder.capsletter("⠠", &rule("capsletter 6"));
        cap_builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        cap_builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let uppercase = Indicator::Uppercase(cap_builder.build().unwrap());

        let indicators = Indicators::new(vec![numeric, uppercase]);

        assert_eq!(
            indicators.precompute("A1"),
            IndicationEvents::from(vec![
                IndicationEvent::UppercaseStart.into(),                          // 'A'
                IndicationEvent::NumberStart | IndicationEvent::DontContract,   // '1'
            ])
        );
    }
}
