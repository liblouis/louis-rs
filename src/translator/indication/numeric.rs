//! Numeric Braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins:
//!
//! - Emits the numsign translation at the first digit of a numeric run.
//! - Emits the non-numsign translation at the first non-numeric terminating character.

use crate::{
    parser::AnchoredRule,
    translator::{ResolvedTranslation, TranslationStage},
};

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

#[derive(Debug, Clone)]
pub struct Indicator {
    /// Characters that trigger entry into a numeric run (digits)
    numeric_chars: HashSet<char>,
    /// Characters that extend a numeric run without being digits themselves
    /// (e.g. `numericmodechars`)
    extra_numeric_chars: HashSet<char>,
    /// Characters that extend a numeric run only when sandwiched between two
    /// digits (`midnum`)
    mid_numeric_chars: HashSet<char>,
    /// Translation emitted at the start of a numeric run (the numsign)
    start_translation: Option<ResolvedTranslation>,
    /// Translation emitted at the end of a numeric run (the non-numsign)
    end_translation: Option<ResolvedTranslation>,
    /// Characters that trigger end-of-run indication when the run exits into them
    terminating_chars: HashSet<char>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut translations: Vec<(usize, ResolvedTranslation)> = Vec::new();

        if self.start_translation.is_none() {
            return translations;
        }

        let mut in_numeric = false;

        for (pos, &c) in chars.iter().enumerate() {
            let is_numeric = self.numeric_chars.contains(&c);
            match (is_numeric, in_numeric) {
                (false, false) => {}
                (true, false) => {
                    in_numeric = true;
                    if let Some(t) = &self.start_translation {
                        translations.push((pos, t.clone()));
                    }
                }
                (true, true) => {}
                (false, true) if self.extra_numeric_chars.contains(&c) => {}
                (false, true)
                    if self.extra_numeric_chars.is_empty()
                        && self.mid_numeric_chars.contains(&c)
                        && chars
                            .get(pos + 1)
                            .is_some_and(|nc| self.numeric_chars.contains(nc)) => {}
                (false, true) => {
                    in_numeric = false;
                    if self.terminating_chars.contains(&c)
                        && let Some(t) = &self.end_translation
                    {
                        translations.push((pos, t.clone()));
                    }
                }
            }
        }

        translations
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::RuleParser;

    use super::*;

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn pairs(t: &[(usize, ResolvedTranslation)]) -> Vec<(usize, String)> {
        t.iter()
            .map(|(pos, r)| (*pos, r.output().to_string()))
            .collect()
    }

    #[test]
    fn precompute_number() {
        let mut builder = IndicatorBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.numsign("⠼", &rule("numsign 3456"));
        builder.numericnocontchars("abc");
        let indicator = builder.build().unwrap();

        assert_eq!(
            pairs(&indicator.precompute("ab12 a")),
            vec![(2, "⠼".to_string())]
        );
    }

    #[test]
    fn precompute_end_indication() {
        let mut builder = IndicatorBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.numsign("⠼", &rule("numsign 3456"));
        builder.nonumsign("⠰", &rule("nonumsign 56"));
        builder.numericnocontchars("abc");
        let indicator = builder.build().unwrap();

        assert_eq!(
            pairs(&indicator.precompute("ab12a")),
            vec![(2, "⠼".to_string()), (4, "⠰".to_string())],
        );
    }
}
