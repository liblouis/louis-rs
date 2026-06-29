//! Braille indication
//!
//! Braille indicators are dot patterns which are inserted into the braille text
//! to indicate such things as capitalization, italic type, computer braille,
//! etc.
//!
//! Braille indication is handled by a preprocessing pass over the input text.
//! Before translation begins each indicator analyses the full input and records,
//! at each character position, which translations to emit and which rule-selection
//! flags apply. The result is a [`PrecomputedIndications`] value that the
//! translation loop consults by index rather than running stateful logic in
//! parallel with rule selection.
//!
//! The indicator types are:
//! * [`emphasis::Indicator`]: detects emphasis runs and emits letter/word/phrase indicators
//! * [`numeric::Indicator`]: detects digit sequences and emits number-sign indicators
//! * [`uppercase::Indicator`]: detects uppercase runs and emits capitalisation indicators
//! * [`lettersign::Indicator`]: detects contractions that require a letter sign
//! * [`nocontract::Indicator`]: detects contractions that require a no-contract sign

use crate::emphasis::EmphasisSpan;
use crate::translator::ResolvedTranslation;

pub mod emphasis;
pub mod lettersign;
pub mod nocontract;
pub mod numeric;
pub mod uppercase;

#[derive(Debug, Clone)]
pub enum Indicator {
    Emphasis(emphasis::Indicator),
    LetterSign(lettersign::Indicator),
    NoContract(nocontract::Indicator),
    Numeric(numeric::Indicator),
    Uppercase(uppercase::Indicator),
}

#[derive(Debug, Clone)]
pub struct Indicators(Vec<Indicator>);

/// The result of pre-computing indications for a given input.
///
/// Holds per-position braille translations to emit, indexed by
/// character position (with one extra slot at index `n` for translations that
/// must appear after all input characters, e.g. `endemph` at end of string).
pub struct PrecomputedIndications(Vec<Vec<ResolvedTranslation>>);

impl PrecomputedIndications {
    pub fn translations_at(&self, pos: usize) -> Vec<ResolvedTranslation> {
        self.0.get(pos).cloned().unwrap_or_default()
    }
}

impl Indicators {
    pub fn new(indicators: Vec<Indicator>) -> Indicators {
        Indicators(indicators)
    }

    pub fn precompute(&self, input: &str, spans: &[EmphasisSpan]) -> PrecomputedIndications {
        let n = input.chars().count();
        let mut indications: Vec<Vec<ResolvedTranslation>> = vec![vec![]; n + 1];

        for indicator in &self.0 {
            let pairs: Vec<(usize, ResolvedTranslation)> = match indicator {
                Indicator::Emphasis(i) => i.precompute(input, spans),
                Indicator::Numeric(i) => i.precompute(input),
                Indicator::Uppercase(i) => i.precompute(input),
                Indicator::LetterSign(i) => i.precompute(input),
                Indicator::NoContract(i) => i.precompute(input),
            };
            for (pos, t) in pairs {
                indications[pos].push(t);
            }
        }

        PrecomputedIndications(indications)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{AnchoredRule, RuleParser};
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
        let indications = indicators.precompute("A1", &[]);

        let outputs_at = |pos| {
            indications
                .translations_at(pos)
                .into_iter()
                .map(|t| t.output().to_string())
                .collect::<Vec<_>>()
        };
        assert_eq!(outputs_at(0), vec!["⠠"]); // capsletter for 'A'
        assert_eq!(outputs_at(1), vec!["⠼"]); // numsign for '1'
    }
}
