//! Generic mode braille indication
//!
//! Some tables need to mark a run of text as being in a particular "mode" the same way
//! capitalization is marked, but for an arbitrary character class rather than just
//! uppercase — e.g. the Dutch table's alphabet-change indicator, which puts letters from
//! other alphabets in a class named `foreign` and marks entering/leaving it just like a
//! capitalized run. `modeletter`/`begmodeword`/`endmodeword`/`begmode`/`endmode`/
//! `begmodephrase`/`endmodephrase`/`lenmodephrase` are that generic form: each takes an
//! `attribute` naming the class that drives it (most commonly a custom class declared
//! with `class`, or `digit`).
//!
//! [`Indicator`] shares its tier-selection logic with the built-in `capsletter`/
//! `begcapsword`/... family in [`uppercase`](super::uppercase) — see
//! [`class_indicator`](super::class_indicator) for how a run becomes a letter, word, or
//! passage indicator. The one thing specific to this module is [`IndicatorBuilder::build`]
//! deciding, per attribute name, what bounds a mode word: a `digit` run is its own word;
//! any other custom class is bounded by `letter`, like uppercase minus the
//! `capsmodechars` transparency (no generic opcode for that).

use std::collections::HashMap;

use crate::{
    parser::{AnchoredRule, CharacterClass, Position},
    translator::{ResolvedTranslation, table::TableContext},
};

use super::class_indicator::ClassIndicator;

/// Builder for [`Indicator`].
#[derive(Debug)]
pub struct IndicatorBuilder {
    classes: HashMap<String, ClassIndicator>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            classes: HashMap::new(),
        }
    }

    fn class_mut(&mut self, name: &str) -> &mut ClassIndicator {
        self.classes
            .entry(name.to_string())
            .or_insert_with(ClassIndicator::new)
    }

    pub fn modeletter(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).modeletter(dots, origin);
    }

    pub fn begmodeword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begmodeword(dots, origin);
    }

    pub fn endmodeword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endmodeword(dots, origin);
    }

    pub fn begmode(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begmode(dots, origin);
    }

    pub fn endmode(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endmode(dots, origin);
    }

    pub fn begmodephrase(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begmodephrase(dots, origin);
    }

    pub fn endmodephrase(
        &mut self,
        name: &str,
        dots: &str,
        position: &Position,
        origin: &AnchoredRule,
    ) {
        self.class_mut(name).endmodephrase(dots, position, origin);
    }

    pub fn lenmodephrase(&mut self, name: &str, len: usize) {
        self.class_mut(name).lenmodephrase(len);
    }

    pub fn build(self, ctx: &TableContext) -> Option<Indicator> {
        let classes: Vec<ClassIndicator> = self
            .classes
            .into_iter()
            .map(|(name, mut indicator)| {
                indicator.set_character_classes(ctx.character_classes().clone());
                let active_chars = ctx
                    .character_classes()
                    .get(&CharacterClass::from(name.as_str()))
                    .unwrap_or_default();
                // "digit" is self-bounded (liblouis hardcodes this; a digit run is its own
                // word). Everything else is letter-bounded, like uppercase minus the
                // capsmodechars carve-out (no generic opcode for it). See class_indicator's
                // module doc.
                let letter_chars = if name == "digit" {
                    active_chars.clone()
                } else {
                    ctx.character_classes()
                        .get(&CharacterClass::Letter)
                        .unwrap_or_default()
                };
                indicator.active_characters(active_chars);
                indicator.letter_characters(letter_chars);
                indicator
            })
            .filter(|indicator| indicator.is_indicating())
            .collect();
        if classes.is_empty() {
            None
        } else {
            Some(Indicator { classes })
        }
    }
}

/// Compiled mode indicator for one or more mode classes.
#[derive(Debug, Clone)]
pub struct Indicator {
    classes: Vec<ClassIndicator>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs for the given input.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut result: Vec<(usize, ResolvedTranslation)> = self
            .classes
            .iter()
            .flat_map(|class| class.precompute(&chars))
            .collect();
        result.sort_by_key(|(pos, _)| *pos);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{CharacterClass, CharacterClasses, RuleParser};

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn outputs_at(result: &[(usize, ResolvedTranslation)], pos: usize) -> Vec<String> {
        result
            .iter()
            .filter(|(p, _)| *p == pos)
            .map(|(_, t)| t.output().to_string())
            .collect()
    }

    fn digit_ctx() -> TableContext {
        let cc = CharacterClasses::new(&[
            (CharacterClass::Digit, &['1', '2', '3']),
            (CharacterClass::Space, &[' ']),
        ]);
        TableContext::new(cc, CharacterClasses::default(), Default::default())
    }

    #[test]
    fn begmodeword_mirrors_numericmode_test() {
        // see liblouis/tests/yaml/numericmode.yaml
        let mut b = IndicatorBuilder::new();
        b.begmodeword("digit", "⠼", &rule("begmodeword digit 3456"));
        b.endmodeword("digit", "⠰", &rule("endmodeword digit 56"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("123a");
        assert_eq!(outputs_at(&result, 0), vec!["⠼"]);
        assert!(outputs_at(&result, 3).is_empty());

        let result = indicator.precompute("123 ");
        assert_eq!(outputs_at(&result, 0), vec!["⠼"]);
        assert!(outputs_at(&result, 3).is_empty());
    }

    #[test]
    fn modeletter_for_single_char() {
        let mut b = IndicatorBuilder::new();
        b.modeletter("digit", "⠸", &rule("modeletter digit 6"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("a1b");
        assert_eq!(outputs_at(&result, 1), vec!["⠸"]);
        assert!(outputs_at(&result, 0).is_empty());
    }

    #[test]
    fn modeletter_repeats_when_no_word_tier() {
        let mut b = IndicatorBuilder::new();
        b.modeletter("digit", "⠸", &rule("modeletter digit 6"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("123");
        assert_eq!(
            outputs_at(&result, 0)
                .into_iter()
                .chain(outputs_at(&result, 1))
                .chain(outputs_at(&result, 2))
                .collect::<Vec<_>>(),
            vec!["⠸", "⠸", "⠸"]
        );
    }

    #[test]
    fn begmode_endmode_general_passage() {
        let mut b = IndicatorBuilder::new();
        b.begmode("digit", "⠼⠼", &rule("begmode digit 3456-3456"));
        b.endmode("digit", "⠰", &rule("endmode digit 56"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("a123b");
        assert_eq!(outputs_at(&result, 1), vec!["⠼⠼"]);
        assert_eq!(outputs_at(&result, 4), vec!["⠰"]);
    }

    #[test]
    fn phrase_tier_used_once_length_threshold_reached() {
        let mut b = IndicatorBuilder::new();
        b.begmodeword("digit", "⠼", &rule("begmodeword digit 3456"));
        b.begmodephrase("digit", "⠼⠼⠼", &rule("begmodephrase digit 3456-3456-3456"));
        b.endmode("digit", "⠰", &rule("endmode digit 56"));
        b.lenmodephrase("digit", 2);
        let indicator = b.build(&digit_ctx()).unwrap();

        // Two separate digit words -> reaches the (default-lowered) threshold, uses
        // begmodephrase, falls back to the shared endmode closer (no endmodephrase set).
        let result = indicator.precompute("1 2");
        assert_eq!(outputs_at(&result, 0), vec!["⠼⠼⠼"]);
        assert_eq!(outputs_at(&result, 3), vec!["⠰"]);
    }

    #[test]
    fn generic_custom_class_is_letter_bounded_and_needs_endmodeword_mid_word() {
        // Unlike "digit" (self-bounded), a genuine custom class is letter-bounded, like
        // uppercase: an active run ending mid-word (followed by another letter within the
        // same word) needs an explicit endmodeword, and doesn't get one silently.
        let mut b = IndicatorBuilder::new();
        b.begmodeword("vowel", "⠸", &rule("begmodeword vowel 6"));
        b.endmodeword("vowel", "⠐", &rule("endmodeword vowel 5"));
        let cc = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'e', 'i', 'o', 'u', 'x', 'y']),
            (
                CharacterClass::UserDefined("vowel".to_string()),
                &['a', 'e', 'i', 'o', 'u'],
            ),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        let indicator = b.build(&ctx).unwrap();

        // "aeiox": vowel run "aeio" ends mid-word before the consonant 'x' -> endmodeword.
        let result = indicator.precompute("aeiox");
        assert_eq!(outputs_at(&result, 0), vec!["⠸"]);
        assert_eq!(outputs_at(&result, 4), vec!["⠐"]);
    }
}
