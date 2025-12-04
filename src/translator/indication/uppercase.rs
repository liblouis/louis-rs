//! Uppercase Braille indication
//!
//! [`Indicator`] is a simple state machine to keep track of the state of a translation. As soon as
//! a character is encountered that is in the set of [`Indicator::uppercase_chars`] the state is
//! changed to an uppercase state ([`State::UppercaseSingle`] or [`State::UppercaseMulti`]). When a
//! character is encountered that is not in [`Indicator::uppercase_chars`] the state is changed back
//! to [`State::Default`].
//!
//! An indication for a start is only emitted if there is any indicators listed in the braille table
//! [`Indicator::is_indicating`] and the state is changed to `State::Uppercase`.
//!
//! An indication for the end is only emitted if there is any indicators listed in the braille table
//! [`Indicator::is_indicating`] and the state is changed to `State::Default`.

use crate::{
    parser::AnchoredRule,
    translator::{Translation, TranslationStage},
};

use std::collections::HashSet;

/// Possible states for the [`Indicator`] state machine
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
            state: State::Default,
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

    pub fn build(self) -> Indicator {
        self.0
    }

    pub fn capsletter(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_letter_translation = Some(Translation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn begcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_word_translation = Some(Translation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn endcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.end_word_translation = Some(Translation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn begcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.start_translation = Some(Translation::new(
            "",
            s,
            1,
            TranslationStage::Main,
            origin.clone(),
        ));
    }

    pub fn endcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.end_translation = Some(Translation::new(
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
    state: State,
    uppercase_chars: HashSet<char>,
    extra_uppercase_chars: HashSet<char>,
    start_letter_translation: Option<Translation>,
    start_word_translation: Option<Translation>,
    end_word_translation: Option<Translation>,
    start_translation: Option<Translation>,
    end_translation: Option<Translation>,
    terminating_chars: HashSet<char>,
}

impl Indicator {
    /// The transition method of the uppercase indication state machine.
    ///
    /// Takes a string slice to examine the next character(s). Typically the
    /// indicator only looks at the next character, but there are cases where
    /// the indicator wants a bigger look-ahead, so we take a `&str` as input
    /// instead of just a `char`.
    pub fn next(&mut self, s: &str) -> Option<Translation> {
        let mut chars = s.chars();
        let c = chars.next();
        if !self.is_indicating() || c.is_none() {
            return None;
        }
        match (&self.state, self.uppercase_chars.contains(&c.unwrap())) {
            (State::Default, true) => {
                // OK, so we hit an uppercase char. Are we looking at just a
                // single char or are we looking at a whole capitalized word?
                if let Some(next_char) = chars.next()
                    && self.uppercase_chars.contains(&next_char)
                {
                    // well, at least two uppercase chars
                    // FIXME: Not sure if we can deduce that the whole word is
                    // uppercase by just looking at the first two characters
                    self.state = State::UppercaseMulti;
                    self.start_word_translation
                        .clone()
                        .or(self.start_letter_translation.clone())
                } else {
                    // looks like it was just a single uppercase letter
                    self.state = State::UppercaseSingle;
                    self.start_letter_translation.clone()
                }
            }
            (State::UppercaseSingle, false) => {
                self.state = State::Default;
                // no need to indicate anything
                None
            }
            (State::UppercaseMulti, false) => {
                self.state = State::Default;
                // only indicate the end of uppercase if there is an end_indicator and the next char
                // is a letter
                if self.terminating_chars.contains(&c.unwrap()) {
                    self.end_word_translation.clone()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn is_indicating(&self) -> bool {
        self.start_letter_translation.is_some()
            || self.start_word_translation.is_some()
            || self.start_translation.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;

    #[test]
    fn indicator() {
        let rule = RuleParser::new("capsletter 123").rule().unwrap();
        let rule = AnchoredRule::new(rule, None, 0);
        let mut builder = IndicatorBuilder::new();
        builder.capsletter("⠇", &rule);
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let mut indicator = builder.build();
        assert_eq!(
            indicator.next("Abc ".into()),
            Some(Translation::new("", "⠇", 1, TranslationStage::Main, rule,))
        );
        assert_eq!(indicator.next("bc ".into()), None);
        assert_eq!(indicator.next("c ".into()), None);
        assert_eq!(indicator.next(" ".into()), None);
        assert_eq!(indicator.next("".into()), None);
    }

    #[test]
    fn end_indication() {
        let rule = RuleParser::new("begcapsword 123").rule().unwrap();
        let begcapsword_rule = AnchoredRule::new(rule, None, 0);
        let rule = RuleParser::new("endcapsword 6").rule().unwrap();
        let endcapsword_rule = AnchoredRule::new(rule, None, 0);
        let mut builder = IndicatorBuilder::new();
        builder.begcapsword("⠇", &begcapsword_rule);
        builder.endcapsword("⠠", &endcapsword_rule);
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let mut indicator = builder.build();
        assert_eq!(
            indicator.next("ABCa".into()),
            Some(Translation::new(
                "",
                "⠇",
                1,
                TranslationStage::Main,
                begcapsword_rule,
            ))
        );
        assert_eq!(indicator.next("BCa".into()), None);
        assert_eq!(indicator.next("Ca".into()), None);
        assert_eq!(
            indicator.next("a".into()),
            Some(Translation::new(
                "",
                "⠠",
                1,
                TranslationStage::Main,
                endcapsword_rule,
            ))
        );
        assert_eq!(indicator.next("".into()), None);
    }
}
