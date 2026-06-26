//! Numeric Braille indication
//!
//! [`Indicator`] is a simple state machine to keep track of the state of a translation. As soon
//! as a character is encountered that is in the set of [`Indicator::numeric_chars`] the state is
//! changed to [`State::Numeric`]. When a character is encountered that is neither in
//! [`Indicator::numeric_chars`] nor in [`Indicator::extra_numeric_chars`] the state is changed back
//! to [`State::Default`]
//!
//! An indication for a start is only emitted if there is a [`Indicator::start_translation`] and the
//! state is changed to `State::Numeric`.
//!
//! Indication for the end is only emitted if there is a [`Indicator::end_translation`], the state is
//! changed to `State::Default` and the character encountered is in the set of
//! [`Indicator::terminating_chars`].

use crate::{
    parser::AnchoredRule,
    translator::{ResolvedTranslation, TranslationStage},
};

use super::events::{IndicationEvent, IndicationEvents};
use std::collections::HashSet;

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder(Indicator);

impl IndicatorBuilder {
    pub fn new() -> Self {
        IndicatorBuilder(Indicator {
            numeric_chars: HashSet::default(),
            extra_numeric_chars: HashSet::default(),
            mid_numeric_chars: HashSet::default(),
            start_translation: None,
            end_translation: None,
            terminating_chars: HashSet::default(),
        })
    }

    pub fn build(self) -> Option<Indicator> {
        if self.0.start_translation.is_some() && !self.0.numeric_chars.is_empty() {
            Some(self.0)
        } else {
            None
        }
    }

    pub fn numsign(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn nonumsign(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.end_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn midnum(&mut self, s: &str) {
        self.0.mid_numeric_chars.extend(s.chars());
    }

    pub fn numericnocontchars(&mut self, s: &str) {
        self.0.terminating_chars = HashSet::from_iter(s.chars());
    }

    pub fn numericmodechars(&mut self, s: &str) {
        self.0.extra_numeric_chars = HashSet::from_iter(s.chars());
    }

    pub fn numeric_characters(&mut self, chars: HashSet<char>) {
        self.0.numeric_chars = chars;
    }
}

/// A very simple state machine to keep track when an numeric indication is
/// required
///
/// The state is changed to `State::Numeric` as soon as a character is
/// encountered that is a member of the set of `numeric_chars`. And if a
/// character is encountered that is neither in the set of `numeric_chars` nor
/// in the set of `extra_numeric_chars` the state is changed to
/// `State::Default`.
///
/// An indication for a start is emitted if the `start_indicator` is not None.
/// Indication for the end is emitted if `end_indicator` is not None and the
/// character encountered is in the set of terminating_chars.
#[derive(Debug, Clone)]
pub struct Indicator {
    numeric_chars: HashSet<char>,
    /// The set of characters that will prevent a state change to the
    /// [State::Default] mode
    extra_numeric_chars: HashSet<char>,
    /// The set of `midnum` characters. They keep the indicator in
    /// [State::Numeric] only when sandwiched between two numeric characters.
    mid_numeric_chars: HashSet<char>,
    /// The translation to indicate the start of a sequence of numerical characters
    start_translation: Option<ResolvedTranslation>,
    /// The characters to indicate the end of a sequence of numerical characters
    end_translation: Option<ResolvedTranslation>,
    /// The characters that will trigger the indication of the end of a sequence of numerical characters
    terminating_chars: HashSet<char>,
}

impl Indicator {
    pub fn event_translations(&self) -> Vec<(IndicationEvent, ResolvedTranslation)> {
        let mut v = Vec::new();
        if let Some(t) = &self.start_translation {
            v.push((IndicationEvent::NumberStart, t.clone()));
        }
        if let Some(t) = &self.end_translation {
            v.push((IndicationEvent::NumberEnd, t.clone()));
        }
        v
    }

    pub fn precompute(&self, input: &str) -> IndicationEvents {
        let chars: Vec<char> = input.chars().collect();
        let mut events = IndicationEvents::new(chars.len());

        if self.start_translation.is_none() {
            return events;
        }

        let mut in_numeric = false;

        for (pos, &c) in chars.iter().enumerate() {
            let is_numeric = self.numeric_chars.contains(&c);
            match (is_numeric, in_numeric) {
                (false, false) => {}
                (true, false) => {
                    in_numeric = true;
                    events.insert(pos, IndicationEvent::NumberStart);
                    events.insert(pos, IndicationEvent::DontContract);
                }
                (true, true) => {
                    events.insert(pos, IndicationEvent::Number);
                    events.insert(pos, IndicationEvent::DontContract);
                }
                (false, true) if self.extra_numeric_chars.contains(&c) => {
                    events.insert(pos, IndicationEvent::Number);
                    events.insert(pos, IndicationEvent::DontContract);
                }
                (false, true)
                    if self.extra_numeric_chars.is_empty()
                        && self.mid_numeric_chars.contains(&c)
                        && chars
                            .get(pos + 1)
                            .is_some_and(|nc| self.numeric_chars.contains(nc)) =>
                {
                    events.insert(pos, IndicationEvent::Number);
                    events.insert(pos, IndicationEvent::DontContract);
                }
                (false, true) => {
                    in_numeric = false;
                    if self.terminating_chars.contains(&c) {
                        events.insert(pos, IndicationEvent::NumberEnd);
                    }
                }
            }
        }

        events
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::RuleParser;
    use crate::translator::indication::events::{IndicationEvent, IndicationEvents};
    use enumset::EnumSet;

    use super::*;

    #[test]
    fn precompute_number() {
        let rule = RuleParser::new("numsign 3456").rule().unwrap();
        let rule = AnchoredRule::new(rule, None, 0);
        let mut builder = IndicatorBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.numsign("⠼", &rule);
        builder.numericnocontchars("abc");
        let indicator = builder.build().unwrap();

        assert_eq!(
            indicator.precompute("ab12 a"),
            IndicationEvents::from(vec![
                EnumSet::empty(),                                                     // 'a'
                EnumSet::empty(),                                                     // 'b'
                IndicationEvent::NumberStart | IndicationEvent::DontContract,        // '1'
                IndicationEvent::Number      | IndicationEvent::DontContract,        // '2'
                EnumSet::empty(),                                                     // ' ' not a terminating char
                EnumSet::empty(),                                                     // 'a'
            ])
        );
    }

    #[test]
    fn precompute_end_indication() {
        let rule = RuleParser::new("numsign 3456").rule().unwrap();
        let numsign_rule = AnchoredRule::new(rule, None, 0);
        let rule = RuleParser::new("nonumsign 56").rule().unwrap();
        let nonumsign_rule = AnchoredRule::new(rule, None, 0);
        let mut builder = IndicatorBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.numsign("⠼", &numsign_rule);
        builder.nonumsign("⠰", &nonumsign_rule);
        builder.numericnocontchars("abc");
        let indicator = builder.build().unwrap();

        assert_eq!(
            indicator.precompute("ab12a"),
            IndicationEvents::from(vec![
                EnumSet::empty(),                                                     // 'a'
                EnumSet::empty(),                                                     // 'b'
                IndicationEvent::NumberStart | IndicationEvent::DontContract,        // '1'
                IndicationEvent::Number      | IndicationEvent::DontContract,        // '2'
                IndicationEvent::NumberEnd.into(),                                    // 'a' terminating char
            ])
        );
    }
}
