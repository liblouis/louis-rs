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
//! * [`computer_braille::Indicator`]: wraps computer-braille spans with begcomp/endcomp
//! * [`emphasis::Indicator`]: detects emphasis runs and emits letter/word/phrase indicators
//! * [`numeric::Indicator`]: detects digit sequences and emits number-sign indicators
//! * [`uppercase::Indicator`]: detects uppercase runs and emits capitalisation indicators
//! * [`mode::Indicator`]: detects character class runs and emits mode indicators
//! * [`lettersign::Indicator`]: detects isolated letters that require a letter sign
//!
//! # Rewriters
//!
//! An indicator sees only the text. Indication that depends on which rule translation
//! chose belongs in a rewriter instead: a function from the chosen
//! [`ResolvedTranslation`]s to new ones, run at the end of the main stage, which may
//! insert translations or change them. Don't guess the choice from the text in an
//! indicator. Unlike the pipeline's pass2 to pass4, which apply table rules to a string,
//! rewriters are built in and see each translation's rule. They are:
//! * [`contraction::ContractionSigns`]: puts the `nocontractsign` and letter sign in
//!   front of a `contraction` rule chosen (forward)
//! * [`largesign::LargeSigns`]: drops the spaces between two `largesign` rules chosen
//!   (forward), and puts them back (backward)
//! * [`uppercase::BackwardCaps`]: capitalizes after a `capsletter`, in the words a
//!   `begcapsword` starts and in a capitals passage, as written, with the table's
//!   `base uppercase` (backward)
//!
//! A table holds its rewriters as [`Rewriters`], applied in turn.

use std::cmp::Reverse;
use std::ops::Range;

use crate::emphasis::EmphasisSpan;
use crate::parser::Rule;
use crate::translator::ResolvedTranslation;

mod class_indicator;
pub mod computer_braille;
pub mod contraction;
pub mod emphasis;
pub mod largesign;
pub mod lettersign;
pub mod mode;
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
    Mode(mode::Indicator),
    Numeric(numeric::Indicator),
    Uppercase(uppercase::Indicator),
}

impl Indicator {
    /// The indications at their positions, each with its place among those at the same
    /// position
    fn precompute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
    ) -> Vec<(usize, IndicationOrder, ResolvedTranslation)> {
        let (pairs, order): (Vec<_>, fn(&ResolvedTranslation) -> IndicationOrder) = match self {
            // the emphasis knows how its runs nest, and places its indications itself
            Indicator::Emphasis(i) => return i.precompute(input, spans),
            Indicator::ComputerBraille(i) => (i.precompute(input, spans), |_| {
                IndicationOrder::ComputerBraille
            }),
            Indicator::LetterSign(i) => (i.precompute(input), |_| IndicationOrder::LetterSign),
            Indicator::Uppercase(i) => (i.precompute(input), |t| match t.rule() {
                Some(
                    Rule::Endcapsword { .. } | Rule::Endcapsphrase { .. } | Rule::Endcaps { .. },
                ) => IndicationOrder::CapitalsEnd,
                _ => IndicationOrder::Capitals,
            }),
            Indicator::Mode(i) => (i.precompute(input), |t| match t.rule() {
                Some(
                    Rule::Endmodeword { .. } | Rule::Endmodephrase { .. } | Rule::Endmode { .. },
                ) => IndicationOrder::ModeEnd,
                _ => IndicationOrder::Mode,
            }),
            Indicator::Numeric(i) => (i.precompute(input), |_| IndicationOrder::Numeric),
        };
        pairs
            .into_iter()
            .map(|(pos, t)| (pos, order(&t), t))
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct Indicators(Vec<Indicator>);

/// The place of an indication among those at the same position, first to last (ADR-0026).
///
/// Indicators nest, as liblouis's `insertEmphasesAt` nests them and UEB 8.6.2 prefers:
/// what closes there closes innermost first, then what opens there opens outermost first,
/// capitals and modes closest to the word. The other indicators keep their places around
/// them. Each group belongs to one indicator, so the order of [`Indicators`] doesn't
/// matter; the fields break ties within a group, and an equal key keeps the order in
/// which the indicator gave its indications.
///
/// It is the same kind of key as [`Rank`], which picks the one translation that wins at a
/// position; this one keeps all the indications and puts them in order, lowest first.
///
/// [`Rank`]: crate::translator::translation::Rank
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IndicationOrder {
    /// `begcomp` and `endcomp`, before everything else
    ComputerBraille,
    /// The end of capitals: opened closest to the word, they close first
    CapitalsEnd,
    /// The end of a mode, as for capitals, which liblouis treats as the first mode
    ModeEnd,
    /// The end of emphasis: the most recently opened first, then in declaration order
    EmphasisEnd {
        /// Where the emphasis began, its first character that isn't a space: the
        /// latest first, as it is the innermost
        opened: Reverse<usize>,
        /// The index of the class among the `emphclass` rules: on a tie, the class
        /// declared first closes first
        definition_order: usize,
    },
    /// The begin of emphasis: the longest first, as it closes last, then the
    /// last-declared class first
    EmphasisBegin {
        /// Where the emphasis ends, after its last character that isn't a space: the
        /// latest first, as it is the outermost
        until: Reverse<usize>,
        /// The index of the class among the `emphclass` rules: on a tie, the class
        /// declared last opens first
        definition_order: Reverse<usize>,
    },
    /// The letter sign goes before the capitals, so "B" gives ⠰⠠⠃ and not ⠠⠰⠃
    LetterSign,
    /// The begin of capitals, or a capital letter
    Capitals,
    /// The begin of a mode
    Mode,
    /// Last, as in liblouis: "1A" must give ⠠⠰ for en-ueb-g2's pass2 rule `_$D[@6-56]$U`
    /// to drop the nonumsign in front of a capital
    Numeric,
}

/// The result of pre-computing indications for a given input.
///
/// Holds per-position braille translations to emit, indexed by
/// character position (with one extra slot at index `n` for translations that
/// must appear after all input characters, e.g. `endemph` at end of string).
pub struct PrecomputedIndications {
    indications: Vec<Vec<ResolvedTranslation>>,
    /// Where an emphasis, capitals or mode indicator falls, see [`Self::emphasis_in`]
    emphasis: Vec<bool>,
}

impl PrecomputedIndications {
    pub fn translations_at(&self, pos: usize) -> Vec<ResolvedTranslation> {
        self.indications.get(pos).cloned().unwrap_or_default()
    }

    /// Whether an indicator falls on any position in `range`
    pub fn any_in(&self, range: Range<usize>) -> bool {
        self.indications
            .get(range)
            .is_some_and(|positions| positions.iter().any(|indicators| !indicators.is_empty()))
    }

    /// Whether an emphasis, capitals or mode indicator falls on any position in `range`:
    /// where emphasis or capitals begin, end or are indicated again. Not a number sign or
    /// a letter sign.
    pub fn emphasis_in(&self, range: Range<usize>) -> bool {
        self.emphasis
            .get(range)
            .is_some_and(|positions| positions.contains(&true))
    }
}

impl Indicators {
    pub fn new(indicators: Vec<Indicator>) -> Indicators {
        Indicators(indicators)
    }

    pub fn precompute(&self, input: &str, spans: &[EmphasisSpan]) -> PrecomputedIndications {
        let n = input.chars().count();
        let mut ordered: Vec<(usize, IndicationOrder, ResolvedTranslation)> = self
            .0
            .iter()
            .flat_map(|indicator| indicator.precompute(input, spans))
            .collect();
        // stable, so an equal key keeps the order in which the indicator gave them
        ordered.sort_by_key(|(pos, order, _)| (*pos, *order));

        let mut indications: Vec<Vec<ResolvedTranslation>> = vec![vec![]; n + 1];
        let mut emphasis = vec![false; n + 1];
        for (pos, order, t) in ordered {
            emphasis[pos] |= !matches!(
                order,
                IndicationOrder::ComputerBraille
                    | IndicationOrder::LetterSign
                    | IndicationOrder::Numeric
            );
            indications[pos].push(t);
        }

        PrecomputedIndications {
            indications,
            emphasis,
        }
    }
}

/// Rewrites the chosen translations, see [Rewriters](self#rewriters).
#[derive(Debug, Clone)]
pub enum Rewriter {
    /// Puts the `nocontractsign` and letter sign in front of a `contraction` (forward).
    ContractionSigns(contraction::ContractionSigns),
    /// Drops the spaces between two `largesign`s (forward).
    LargeSigns(largesign::LargeSigns),
    /// Capitalizes after a `capsletter`, in the words a `begcapsword` starts and in a
    /// capitals passage, as written (backward).
    BackwardCaps(uppercase::BackwardCaps),
    /// Puts the spaces back between `largesign`s written together (backward), after the
    /// capitals are read.
    BackwardLargeSigns(largesign::LargeSigns),
}

impl Rewriter {
    fn apply(&self, translations: Vec<ResolvedTranslation>) -> Vec<ResolvedTranslation> {
        match self {
            Rewriter::ContractionSigns(a) => a.apply(translations),
            Rewriter::LargeSigns(a) => a.apply(translations),
            Rewriter::BackwardCaps(a) => a.apply(translations),
            Rewriter::BackwardLargeSigns(a) => a.apply_backward(translations),
        }
    }
}

/// A collection of [`Rewriter`]s that are applied in turn.
#[derive(Debug, Clone)]
pub struct Rewriters(Vec<Rewriter>);

impl Rewriters {
    pub fn new(rewriters: Vec<Rewriter>) -> Self {
        Rewriters(rewriters)
    }

    pub fn apply(&self, translations: Vec<ResolvedTranslation>) -> Vec<ResolvedTranslation> {
        self.0
            .iter()
            .fold(translations, |t, rewriter| rewriter.apply(t))
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
