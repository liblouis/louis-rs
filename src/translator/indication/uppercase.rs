//! Uppercase Braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins.
//! For each character position it records which indication events apply:
//!
//! - [`IndicationEvent::UppercaseStart`] at a single isolated uppercase character.
//! - [`IndicationEvent::AllCapsStart`] at the first character of a run of two or more uppercase
//!   characters.
//! - [`IndicationEvent::AllCaps`] at every subsequent uppercase character inside such a run.
//! - [`IndicationEvent::AllCapsEnd`] at the first terminating letter that follows a
//!   multi-character uppercase run.

use crate::{
    parser::AnchoredRule,
    translator::{ResolvedTranslation, TranslationStage},
};

use super::events::{IndicationEvent, IndicationEvents};
use std::collections::HashSet;

/// States used locally within the [`Indicator::precompute`] pass
#[derive(Debug, Clone)]
enum State {
    Default,
    UppercaseSingle,
    UppercaseMulti,
}

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder(Indicator);

impl IndicatorBuilder {
    pub fn new() -> Self {
        IndicatorBuilder(Indicator {
            uppercase_chars: HashSet::default(),
            extra_uppercase_chars: HashSet::default(),
            start_translation: None,
            end_translation: None,
            terminating_chars: HashSet::default(),
            start_letter_translation: None,
            start_word_translation: None,
            end_word_translation: None,
        })
    }

    pub fn build(self) -> Option<Indicator> {
        if self.0.is_indicating() {
            Some(self.0)
        } else {
            None
        }
    }

    pub fn capsletter(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_letter_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn begcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_word_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn endcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.end_word_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn begcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn endcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.end_translation = Some(ResolvedTranslation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn capsmodechars(&mut self, s: &str) {
        self.0.extra_uppercase_chars = HashSet::from_iter(s.chars());
    }

    pub fn uppercase_characters(&mut self, chars: HashSet<char>) {
        self.0.uppercase_chars = chars;
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.0.terminating_chars = chars;
    }
}

#[derive(Debug, Clone)]
pub struct Indicator {
    uppercase_chars: HashSet<char>,
    extra_uppercase_chars: HashSet<char>,
    start_letter_translation: Option<ResolvedTranslation>,
    start_word_translation: Option<ResolvedTranslation>,
    end_word_translation: Option<ResolvedTranslation>,
    start_translation: Option<ResolvedTranslation>,
    end_translation: Option<ResolvedTranslation>,
    terminating_chars: HashSet<char>,
}

impl Indicator {
    fn is_indicating(&self) -> bool {
        self.start_letter_translation.is_some()
            || self.start_word_translation.is_some()
            || self.start_translation.is_some()
    }

    pub fn event_translations(&self) -> Vec<(IndicationEvent, ResolvedTranslation)> {
        let mut v = Vec::new();
        if let Some(t) = &self.start_letter_translation {
            v.push((IndicationEvent::UppercaseStart, t.clone()));
        }
        if let Some(t) = self.start_word_translation.as_ref().or(self.start_letter_translation.as_ref()) {
            v.push((IndicationEvent::AllCapsStart, t.clone()));
        }
        if let Some(t) = &self.end_word_translation {
            v.push((IndicationEvent::AllCapsEnd, t.clone()));
        }
        v
    }

    pub fn precompute(&self, input: &str) -> IndicationEvents {
        let chars: Vec<char> = input.chars().collect();
        let mut events = IndicationEvents::new(chars.len());

        if !self.is_indicating() {
            return events;
        }

        let mut state = State::Default;

        for (pos, &c) in chars.iter().enumerate() {
            let is_upper = self.uppercase_chars.contains(&c);
            match (&state, is_upper) {
                (State::Default, true) => {
                    let next_is_upper = chars
                        .get(pos + 1)
                        .is_some_and(|nc| self.uppercase_chars.contains(nc));
                    if next_is_upper {
                        state = State::UppercaseMulti;
                        events.insert(pos, IndicationEvent::AllCapsStart);
                    } else {
                        state = State::UppercaseSingle;
                        events.insert(pos, IndicationEvent::UppercaseStart);
                    }
                }
                (State::UppercaseSingle, false) => {
                    state = State::Default;
                }
                (State::UppercaseMulti, true) => {
                    events.insert(pos, IndicationEvent::AllCaps);
                }
                (State::UppercaseMulti, false) => {
                    state = State::Default;
                    if self.terminating_chars.contains(&c) {
                        events.insert(pos, IndicationEvent::AllCapsEnd);
                    }
                }
                _ => {}
            }
        }

        events
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;
    use crate::translator::indication::events::{IndicationEvent, IndicationEvents};
    use enumset::EnumSet;

    #[test]
    fn precompute_indicator() {
        let rule = RuleParser::new("capsletter 123").rule().unwrap();
        let rule = AnchoredRule::new(rule, None, 0);
        let mut builder = IndicatorBuilder::new();
        builder.capsletter("⠇", &rule);
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let indicator = builder.build().unwrap();

        assert_eq!(
            indicator.precompute("Abc "),
            IndicationEvents::from(vec![
                IndicationEvent::UppercaseStart.into(), // 'A' — single uppercase
                EnumSet::empty(),                       // 'b'
                EnumSet::empty(),                       // 'c'
                EnumSet::empty(),                       // ' '
            ])
        );
    }

    #[test]
    fn precompute_end_indication() {
        let rule = RuleParser::new("begcapsword 123").rule().unwrap();
        let begcapsword_rule = AnchoredRule::new(rule, None, 0);
        let rule = RuleParser::new("endcapsword 6").rule().unwrap();
        let endcapsword_rule = AnchoredRule::new(rule, None, 0);
        let mut builder = IndicatorBuilder::new();
        builder.begcapsword("⠇", &begcapsword_rule);
        builder.endcapsword("⠠", &endcapsword_rule);
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let indicator = builder.build().unwrap();

        assert_eq!(
            indicator.precompute("ABCa"),
            IndicationEvents::from(vec![
                IndicationEvent::AllCapsStart.into(), // 'A' — multi uppercase starts
                IndicationEvent::AllCaps.into(),      // 'B'
                IndicationEvent::AllCaps.into(),      // 'C'
                IndicationEvent::AllCapsEnd.into(),   // 'a' — terminating char
            ])
        );
    }
}
