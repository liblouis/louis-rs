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

pub mod computer_braille;
pub mod emphasis;
pub mod lettersign;
pub mod nocontract;
pub mod numeric;
pub mod uppercase;

/// Finds maximal spans within `chars[start..end)` where `predicate` holds.
///
/// Used by indicators that need to segment a range into alternating "matching" and
/// "non-matching" runs — e.g. [`uppercase::Indicator`] splitting text into
/// letter/capsmodechar words, or [`emphasis::Indicator`] splitting an active span into
/// words separated by whitespace (or `emphmodechars`).
fn find_spans(
    chars: &[char],
    start: usize,
    end: usize,
    predicate: impl Fn(char) -> bool,
) -> Vec<(usize, usize)> {
    let mut spans = Vec::new();
    let mut pos = start;
    while pos < end {
        while pos < end && !predicate(chars[pos]) {
            pos += 1;
        }
        if pos >= end {
            break;
        }
        let span_start = pos;
        while pos < end && predicate(chars[pos]) {
            pos += 1;
        }
        spans.push((span_start, pos));
    }
    spans
}

#[derive(Debug, Clone)]
pub enum Indicator {
    ComputerBraille(computer_braille::Indicator),
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
                Indicator::ComputerBraille(i) => i.precompute(input, spans),
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
    use crate::{
        parser::{AnchoredRule, RuleParser},
        translator::table::TableContext,
    };
    use std::collections::HashSet;

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    #[test]
    fn find_spans_returns_maximal_matching_runs() {
        let chars: Vec<char> = "  ab cd  e".chars().collect();
        assert_eq!(
            find_spans(&chars, 0, chars.len(), |c| c != ' '),
            vec![(2, 4), (5, 7), (9, 10)]
        );
    }

    #[test]
    fn find_spans_respects_the_given_range() {
        let chars: Vec<char> = "aabbaabb".chars().collect();
        assert_eq!(find_spans(&chars, 2, 6, |c| c == 'b'), vec![(2, 4)]);
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
        cap_builder.letter_characters(HashSet::from(['a', 'b', 'c', 'A', 'B', 'C']));
        let uppercase = Indicator::Uppercase(cap_builder.build(&TableContext::default()).unwrap());

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
