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

use crate::text_attribute::{TextAttribute, TextAttributes};
use crate::translator::ResolvedTranslation;
use events::{IndicationEvent, IndicationEvents};
use std::collections::HashMap;

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
    pub fn precompute(&self, input: &str) -> IndicationEvents {
        match self {
            Self::LetterSign(i) => i.precompute(input),
            Self::NoContract(i) => i.precompute(input),
            Self::Numeric(i) => i.precompute(input),
            Self::Uppercase(i) => i.precompute(input),
        }
    }

    pub fn event_translations(&self) -> Vec<(IndicationEvent, ResolvedTranslation)> {
        match self {
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
pub struct PrecomputedIndications {
    events: IndicationEvents,
    translations: HashMap<IndicationEvent, ResolvedTranslation>,
}

impl PrecomputedIndications {
    pub fn translations_at(&self, pos: usize) -> Vec<ResolvedTranslation> {
        self.events
            .get(pos)
            .into_iter()
            .filter_map(|event| self.translations.get(&event).cloned())
            .collect()
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
        let events = self
            .0
            .iter()
            .map(|i| i.precompute(input))
            .fold(IndicationEvents::from(typeforms), |acc, e| acc | e);
        PrecomputedIndications { events, translations: self.event_translations() }
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
