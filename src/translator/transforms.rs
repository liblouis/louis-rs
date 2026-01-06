use std::collections::HashSet;

use crate::parser::CharacterClasses;
use crate::parser::multipass::ConsumesInput;
use crate::translator::ResolvedTranslation;
use crate::translator::context_pattern::ContextPatterns;
use crate::translator::swap::SwapClasses;
use crate::translator::translation::TranslationSubset;
use crate::{
    Direction,
    parser::{AnchoredRule, Rule},
    translator::{TranslationError, TranslationStage},
};

#[derive(Debug, Default)]
pub struct TransformationTable {
    patterns: ContextPatterns,
    stage: TranslationStage,
    direction: Direction,
}

/// A builder for [`TransformationTable`]
#[derive(Debug)]
struct TransformationTableBuilder {
    patterns: ContextPatterns,
}

impl TransformationTableBuilder {
    fn new() -> Self {
        Self {
            patterns: ContextPatterns::new(),
        }
    }

    fn build(self, direction: Direction, stage: TranslationStage) -> TransformationTable {
        TransformationTable {
            direction,
            stage: stage,
            patterns: self.patterns,
        }
    }
}

impl TransformationTable {
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    pub fn compile(
        rules: &[&AnchoredRule],
        direction: Direction,
        stage: TranslationStage,
        character_classes: &CharacterClasses,
        swap_classes: &SwapClasses,
    ) -> Result<Self, TranslationError> {
        let mut builder = TransformationTableBuilder::new();

        for &rule in rules {
            match &rule.rule {
                Rule::Correct { test, action, .. }
                | Rule::Pass2 { test, action, .. }
                | Rule::Pass3 { test, action, .. }
                | Rule::Pass4 { test, action, .. } => {
                    if direction == Direction::Backward && !action.consumes_input() {
                        // Correct rules can map to an empty string, i.e. to drop some
                        // characters. Such rules cannot be used for backtranslation (you cannot
                        // translate the empty string to something)
                        continue;
                    }
                    builder.patterns.insert(
                        test,
                        action,
                        rule,
                        stage,
                        character_classes,
                        swap_classes,
                    )?;
                }
                _ => (),
            }
        }

        Ok(builder.build(direction, stage))
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input).iter().map(|t| t.output()).collect()
    }

    fn translation_candidates(&self, input: &str, prev: Option<char>) -> Vec<ResolvedTranslation> {
        self.patterns.find_translations(input).into_iter().collect()
    }

    pub fn trace(&self, input: &str) -> Vec<ResolvedTranslation> {
        let mut translations: Vec<ResolvedTranslation> = Vec::new();
        let mut chars = input.chars();
        let mut prev: Option<char> = None;
        let mut seen: HashSet<TranslationSubset> = HashSet::default();

        loop {
            // given an input query the trie for matching translations
            let candidates = self.translation_candidates(chars.as_str(), prev);

            // use the longest translation
            let candidate = candidates
                .iter()
                .cloned()
                // drop translation candidates that we have applied already at this position in the
                // input
                .filter(|t| !seen.contains(&TranslationSubset::from(t)))
                .max_by_key(|translation| translation.weight());
            if let Some(t) = candidate {
                if t.length() == 0 {
                    // if there is a zero-length translation candiate we run the risk of an infinite
                    // loop, so remember the current translation so we only apply it once
                    seen.insert(TranslationSubset::from(&t));
                } else {
                    seen.clear();
                }
                // there is a matching translation rule
                let translation = t.clone();
                // move the iterator forward by the number of characters in the translation
                chars.nth(t.length() - 1);
                prev = translation.input().chars().last();
                translations.push(translation);
            } else if let Some(next_char) = chars.next() {
                prev = Some(next_char);
                // no translation rule found, just pass the character through
                let translation = ResolvedTranslation::new(
                    &next_char.to_string(),
                    &next_char.to_string(),
                    1,
                    self.stage,
                    None,
                );
                translations.push(translation);
            } else {
                // the chars iterator is exhausted
                break;
            }
        }
        translations
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::RuleParser;

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn correct() {
        let rules = [&parse_rule("correct \"corect\" \"correct\"")];
        let character_classes = CharacterClasses::default();
        let swap_classes = SwapClasses::default();
        let table = TransformationTable::compile(
            &rules,
            Direction::Forward,
            TranslationStage::Pre,
            &character_classes,
            &swap_classes,
        )
        .unwrap();
        assert_eq!(table.translate("foobar"), "foobar");
        assert_eq!(table.translate("corect"), "correct");
        assert_eq!(table.translate("🐂"), "🐂");
    }

    #[test]
    fn pass2() {
        let rules = [&parse_rule("pass2 @123 @12")];
        let character_classes = CharacterClasses::default();
        let swap_classes = SwapClasses::default();
        let table = TransformationTable::compile(
            &rules,
            Direction::Forward,
            TranslationStage::Post1,
            &character_classes,
            &swap_classes,
        )
        .unwrap();
        assert_eq!(table.translate("⠇"), "⠃");
        assert_eq!(table.translate("⠙"), "⠙");
        assert_eq!(table.translate("🐂"), "🐂");
    }

    #[test]
    fn pass3() {
        let rules = [&parse_rule("pass3 @123 @12")];
        let character_classes = CharacterClasses::default();
        let swap_classes = SwapClasses::default();
        let table = TransformationTable::compile(
            &rules,
            Direction::Forward,
            TranslationStage::Post2,
            &character_classes,
            &swap_classes,
        )
        .unwrap();
        assert_eq!(table.translate("⠇"), "⠃");
        assert_eq!(table.translate("⠙"), "⠙");
        assert_eq!(table.translate("🐂"), "🐂");
    }

    #[test]
    fn pass4() {
        let rules = [&parse_rule("pass4 @123 @12")];
        let character_classes = CharacterClasses::default();
        let swap_classes = SwapClasses::default();
        let table = TransformationTable::compile(
            &rules,
            Direction::Forward,
            TranslationStage::Post3,
            &character_classes,
            &swap_classes,
        )
        .unwrap();
        assert_eq!(table.translate("⠇"), "⠃");
        assert_eq!(table.translate("⠙"), "⠙");
        assert_eq!(table.translate("🐂"), "🐂");
    }
}
