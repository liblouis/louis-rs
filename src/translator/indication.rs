//! Braille indication
//!
//! Braille indicators are dot patterns which are inserted into the braille text
//! to indicate such things as capitalization, italic type, computer braille,
//! etc.
//!
//! Braille indication is handled by a preprocessing pass over the input text.
//! Before translation begins each indicator analyses the full input and records,
//! at each character position, which indication events fire there. The result is
//! a [`PrecomputedIndications`] value that the translation loop consults by
//! index rather than running stateful logic in parallel with rule selection.
//!
//! The indicator types are:
//! * [`numeric::Indicator`]: detects digit sequences and records number-sign events
//! * [`uppercase::Indicator`]: detects uppercase runs and records capitalisation events
//! * [`lettersign::Indicator`]: detects contractions that require a letter sign
//! * [`nocontract::Indicator`]: detects contractions that require a no-contract sign

use crate::text_attribute::{TextAttribute, TextAttributes};
use crate::translator::ResolvedTranslation;
use events::{IndicationEvent, IndicationEvents};
use std::collections::HashMap;

pub mod emphasis;
pub mod events;
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

impl Indicator {
    pub fn precompute(&self, input: &str) -> IndicationEvents {
        match self {
            Self::Emphasis(_) => IndicationEvents::new(input.chars().count()),
            Self::LetterSign(i) => i.precompute(input),
            Self::NoContract(i) => i.precompute(input),
            Self::Numeric(i) => i.precompute(input),
            Self::Uppercase(i) => i.precompute(input),
        }
    }

    pub fn event_translations(&self) -> Vec<(IndicationEvent, ResolvedTranslation)> {
        match self {
            Self::Emphasis(_) => vec![],
            Self::LetterSign(i) => i.event_translations(),
            Self::NoContract(i) => i.event_translations(),
            Self::Numeric(i) => i.event_translations(),
            Self::Uppercase(i) => i.event_translations(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Indicators(Vec<Indicator>);

/// The result of pre-computing indications for a given input.
///
/// Combines the pre-computed events per character position with the
/// event-to-translation map, so callers can directly ask for the braille
/// translations to emit at any given character position.
///
/// `direct_translations` holds per-position translations produced by
/// the emphasis indicator (which needs context to decide letter vs word vs
/// phrase indicators). It has `n + 1` slots where `n` is the input length;
/// the extra slot at index `n` is for translations emitted after all chars.
pub struct PrecomputedIndications {
    events: IndicationEvents,
    translations: HashMap<IndicationEvent, ResolvedTranslation>,
    direct_translations: Vec<Vec<ResolvedTranslation>>,
}

impl PrecomputedIndications {
    pub fn translations_at(&self, pos: usize) -> Vec<ResolvedTranslation> {
        // Emphasis indicators (direct) must precede capitalization/numeric indicators
        // (events) so that begemphword appears before capsletter in the output.
        let mut result: Vec<ResolvedTranslation> = self
            .direct_translations
            .get(pos)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
            .to_vec();
        result.extend(
            self.events
                .get(pos)
                .into_iter()
                .filter_map(|event| self.translations.get(&event).cloned()),
        );
        result
    }
}

impl Indicators {
    pub fn new(indicators: Vec<Indicator>) -> Indicators {
        Indicators(indicators)
    }

    fn event_translations(&self) -> HashMap<IndicationEvent, ResolvedTranslation> {
        self.0
            .iter()
            .flat_map(|i| i.event_translations())
            .collect()
    }

    pub fn precompute(&self, input: &str, typeforms: &[TextAttributes]) -> PrecomputedIndications {
        let n = input.chars().count();
        let events = self
            .0
            .iter()
            .filter(|i| !matches!(i, Indicator::Emphasis(_)))
            .map(|i| i.precompute(input))
            .fold(IndicationEvents::from(typeforms), |acc, e| acc | e);

        // Emphasis indicators produce direct per-position translations.
        let mut direct_translations: Vec<Vec<ResolvedTranslation>> = vec![Vec::new(); n + 1];
        for indicator in &self.0 {
            if let Indicator::Emphasis(e) = indicator {
                let emph_result = e.precompute(input, typeforms);
                for (pos, translations) in emph_result.into_iter().enumerate() {
                    if pos < direct_translations.len() {
                        direct_translations[pos].extend(translations);
                    }
                }
            }
        }

        PrecomputedIndications {
            events,
            translations: self.event_translations(),
            direct_translations,
        }
    }
}

impl From<TextAttribute> for IndicationEvent {
    fn from(attr: TextAttribute) -> Self {
        match attr {
            TextAttribute::Italic => Self::Italic,
            TextAttribute::Underline => Self::Underline,
            TextAttribute::Bold => Self::Bold,
            TextAttribute::Emph4 => Self::Emph4,
            TextAttribute::Emph5 => Self::Emph5,
            TextAttribute::Emph6 => Self::Emph6,
            TextAttribute::Emph7 => Self::Emph7,
            TextAttribute::Emph8 => Self::Emph8,
            TextAttribute::Emph9 => Self::Emph9,
            TextAttribute::Emph10 => Self::Emph10,
            TextAttribute::ComputerBraille => Self::ComputerBraille,
            TextAttribute::PassageBreak => Self::PassageBreak,
            TextAttribute::WordReset => Self::WordReset,
            TextAttribute::Script => Self::Script,
            TextAttribute::TransNote => Self::TransNote,
            TextAttribute::TransNote1 => Self::TransNote1,
            TextAttribute::TransNote2 => Self::TransNote2,
            TextAttribute::TransNote3 => Self::TransNote3,
            TextAttribute::TransNote4 => Self::TransNote4,
            TextAttribute::TransNote5 => Self::TransNote5,
        }
    }
}

impl From<&[TextAttributes]> for IndicationEvents {
    fn from(typeforms: &[TextAttributes]) -> Self {
        let mut events = IndicationEvents::new(typeforms.len());
        for (pos, &attrs) in typeforms.iter().enumerate() {
            for attr in attrs {
                events.insert(pos, attr.into());
            }
        }
        events
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

    #[test]
    fn typeforms_convert_to_indication_events() {
        use crate::text_attribute::TextAttribute;
        use enumset::EnumSet;

        let typeforms: Vec<TextAttributes> = vec![
            TextAttribute::Italic.into(),
            TextAttribute::Bold | TextAttribute::Underline,
            EnumSet::empty(),
        ];
        assert_eq!(
            IndicationEvents::from(typeforms.as_slice()),
            IndicationEvents::from(vec![
                IndicationEvent::Italic.into(),
                IndicationEvent::Bold | IndicationEvent::Underline,
                EnumSet::empty(),
            ])
        );
    }
}
