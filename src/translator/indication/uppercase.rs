//! Uppercase Braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins.
//! For each character position it emits the appropriate capitalisation indicator:
//!
//! - [`capsletter`](IndicatorBuilder::capsletter) at a single isolated uppercase character.
//! - [`begcapsword`](IndicatorBuilder::begcapsword) (falling back to `capsletter`) at the
//!   first character of a run of two or more uppercase characters.
//! - [`endcapsword`](IndicatorBuilder::endcapsword) at the first terminating letter that
//!   follows a multi-character uppercase run.

use crate::{
    parser::AnchoredRule,
    translator::{ResolvedTranslation, TranslationStage},
};

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

    /// Returns sparse `(position, translation)` pairs.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut translations: Vec<(usize, ResolvedTranslation)> = Vec::new();

        if !self.is_indicating() {
            return translations;
        }

        let start_word_t = self.start_word_translation.as_ref()
            .or(self.start_letter_translation.as_ref());

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
                        if let Some(t) = start_word_t {
                            translations.push((pos, t.clone()));
                        }
                    } else {
                        state = State::UppercaseSingle;
                        if let Some(t) = &self.start_letter_translation {
                            translations.push((pos, t.clone()));
                        }
                    }
                }
                (State::UppercaseSingle, false) => {
                    state = State::Default;
                }
                (State::UppercaseMulti, true) => {}
                (State::UppercaseMulti, false) => {
                    state = State::Default;
                    if self.terminating_chars.contains(&c) {
                        if let Some(t) = &self.end_word_translation {
                            translations.push((pos, t.clone()));
                        }
                    }
                }
                _ => {}
            }
        }

        translations
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn pairs(t: &[(usize, ResolvedTranslation)]) -> Vec<(usize, String)> {
        t.iter().map(|(pos, r)| (*pos, r.output().to_string())).collect()
    }

    #[test]
    fn precompute_indicator() {
        let mut builder = IndicatorBuilder::new();
        builder.capsletter("⠇", &rule("capsletter 123"));
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let indicator = builder.build().unwrap();

        assert_eq!(pairs(&indicator.precompute("Abc ")), vec![(0, "⠇".to_string())]);
    }

    #[test]
    fn precompute_end_indication() {
        let mut builder = IndicatorBuilder::new();
        builder.begcapsword("⠇", &rule("begcapsword 123"));
        builder.endcapsword("⠠", &rule("endcapsword 6"));
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C']));
        builder.letter_characters(HashSet::from(['a', 'b', 'c']));
        let indicator = builder.build().unwrap();

        assert_eq!(pairs(&indicator.precompute("ABCa")), vec![
            (0, "⠇".to_string()), (3, "⠠".to_string()),
        ]);
    }
}
