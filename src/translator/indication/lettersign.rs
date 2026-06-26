//! Lettersign braille indication
//!
//! Sometimes a sequence of letters happens to be the same as a braille contraction. In that case an
//! indication is needed to clarify the meaning. The lettersign indicator indicates that the
//! following braille cells are not a contraction.
//!
//! Other than the rest of the indicators this [`Indicator`] is not a state machine. It keeps a list
//! of contractions and matches the input against it. If a contraction appears in the input an
//! indication is emitted.

use crate::parser::CharacterClasses;
use crate::translator::TranslationStage;
use crate::translator::trie::{Boundary, Transition};
use crate::{
    parser::{AnchoredRule, Direction, Precedence},
    translator::{ResolvedTranslation, trie::Trie},
};

use log::warn;

use super::events::{IndicationEvent, IndicationEvents};
use std::collections::HashMap;

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder {
    lettersign: Option<(String, AnchoredRule)>,
    contractions: HashMap<String, AnchoredRule>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            lettersign: None,
            contractions: HashMap::new(),
        }
    }

    /// Build an [`Indicator`]
    pub fn build(self, ctx: CharacterClasses) -> Option<Indicator> {
        if !self.contractions.is_empty() && self.lettersign.is_some() {
            let (lettersign, origin) = self.lettersign.unwrap();
            let start_translation = ResolvedTranslation::new(
                "", &lettersign, 1, TranslationStage::Main, origin.clone(),
            );
            let mut trie = Trie::new().with_context(ctx);
            for (contraction, _origin) in self.contractions {
                trie.insert(
                    &contraction,
                    &lettersign,
                    Some(Transition::Start(Boundary::Word)),
                    Some(Transition::End(Boundary::Word)),
                    Direction::Forward,
                    Precedence::Default,
                    TranslationStage::Main,
                    &origin,
                );
            }
            Some(Indicator { contractions: trie, start_translation })
        } else if !self.contractions.is_empty() && self.lettersign.is_none() {
            warn!("Table contains contractions but no letsign");
            None
        } else if self.contractions.is_empty() && self.lettersign.is_some() {
            warn!("Table contains letsign but no contractions");
            None
        } else {
            None
        }
    }

    pub fn letsign(&mut self, s: &str, origin: &AnchoredRule) {
        self.lettersign = Some((s.to_string(), origin.clone()));
    }

    pub fn contraction(&mut self, s: &str, origin: &AnchoredRule) {
        self.contractions.insert(s.to_string(), origin.clone());
    }
}

#[derive(Debug, Clone)]
pub struct Indicator {
    contractions: Trie,
    start_translation: ResolvedTranslation,
}

impl Indicator {
    pub fn event_translations(&self) -> Vec<(IndicationEvent, ResolvedTranslation)> {
        vec![(IndicationEvent::LetterSign, self.start_translation.clone())]
    }

    pub fn precompute(&self, input: &str) -> IndicationEvents {
        let char_count = input.chars().count();
        let mut events = IndicationEvents::new(char_count);
        let mut prev: Option<char> = None;
        let mut char_pos = 0;

        for (byte_pos, c) in input.char_indices() {
            if self.next(&input[byte_pos..], prev).is_some() {
                events.insert(char_pos, IndicationEvent::LetterSign);
            }
            prev = Some(c);
            char_pos += 1;
        }

        events
    }

    pub fn next(&self, s: &str, prev: Option<char>) -> Option<ResolvedTranslation> {
        let translations = self.contractions.find_translations(s, prev);
        if !translations.is_empty() {
            let translation = translations.first().unwrap();
            // the translations contain the contraction as input, so set that one to the empty
            // string
            Some(translation.clone().with_input("").with_weight(1))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::{CharacterClass, RuleParser},
        translator::TranslationStage,
    };
    use crate::translator::indication::events::{IndicationEvent, IndicationEvents};
    use enumset::EnumSet;

    fn rule(rule: &str) -> AnchoredRule {
        let rule = RuleParser::new(rule).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn indicator() {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        builder.contraction("ab", &rule("contraction ab"));
        builder.contraction("cd", &rule("contraction cd"));
        let ctx = CharacterClasses::new(&[(CharacterClass::Letter, &['a', 'b', 'c', 'd'])]);
        let indicator = builder.build(ctx).unwrap();
        assert_eq!(indicator.next("aa".into(), None), None);
        assert_eq!(
            indicator.next("ab".into(), None),
            Some(ResolvedTranslation::new(
                "".into(),
                "⠠".into(),
                1,
                TranslationStage::Main,
                rule("letsign 6")
            ))
        );
        assert_eq!(
            indicator.next("cd".into(), None),
            Some(ResolvedTranslation::new(
                "".into(),
                "⠠".into(),
                1,
                TranslationStage::Main,
                rule("letsign 6")
            ))
        );
    }

    #[test]
    fn precompute_indicator() {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        builder.contraction("ab", &rule("contraction ab"));
        builder.contraction("cd", &rule("contraction cd"));
        let ctx = CharacterClasses::new(&[(CharacterClass::Letter, &['a', 'b', 'c', 'd'])]);
        let indicator = builder.build(ctx).unwrap();

        assert_eq!(
            indicator.precompute("aa"),
            IndicationEvents::from(vec![EnumSet::empty(), EnumSet::empty()])
        );
        assert_eq!(
            indicator.precompute("ab"),
            IndicationEvents::from(vec![IndicationEvent::LetterSign.into(), EnumSet::empty()])
        );
    }
}
