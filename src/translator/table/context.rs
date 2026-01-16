use std::collections::HashSet;

use crate::parser::multipass::ConsumesInput;
use crate::translator::ResolvedTranslation;
use crate::translator::context_pattern::{ContextPatterns, ContextPatternsBuilder};
use crate::translator::table::TableContext;
use crate::translator::translation::TranslationSubset;
use crate::{
    Direction,
    parser::{AnchoredRule, Rule},
    translator::{TranslationError, TranslationStage},
};

#[derive(Debug)]
pub struct ContextTable {
    patterns: ContextPatterns,
    stage: TranslationStage,
    direction: Direction,
}

/// A builder for [`ContextTable`]
#[derive(Debug)]
struct ContextTableBuilder {
    patterns: ContextPatternsBuilder,
}

impl ContextTableBuilder {
    fn new() -> Self {
        Self {
            patterns: ContextPatternsBuilder::new(),
        }
    }

    fn build(self, direction: Direction, stage: TranslationStage) -> ContextTable {
        ContextTable {
            direction,
            stage: stage,
            patterns: self.patterns.build(),
        }
    }
}

impl ContextTable {
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    pub fn compile(
        rules: &[AnchoredRule],
        direction: Direction,
        stage: TranslationStage,
        ctx: &TableContext,
    ) -> Result<Self, TranslationError> {
        let mut builder = ContextTableBuilder::new();

        for rule in rules {
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
                    builder.patterns.insert(test, action, rule, stage, ctx)?;
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
        self.patterns.find(input).into_iter().collect()
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
        let rules = [parse_rule("correct \"corect\" \"correct\"")];
        let ctx = TableContext::default();
        let transform =
            ContextTable::compile(&rules, Direction::Forward, TranslationStage::Pre, &ctx).unwrap();
        assert_eq!(transform.translate("foobar"), "foobar");
        assert_eq!(transform.translate("corect"), "correct");
        assert_eq!(transform.translate("🐂"), "🐂");
    }

    #[test]
    fn pass2() {
        let rules = [parse_rule("pass2 @123 @12")];
        let ctx = TableContext::default();
        let transform =
            ContextTable::compile(&rules, Direction::Forward, TranslationStage::Post1, &ctx)
                .unwrap();
        assert_eq!(transform.translate("⠇"), "⠃");
        assert_eq!(transform.translate("⠙"), "⠙");
        assert_eq!(transform.translate("🐂"), "🐂");
    }

    #[test]
    fn pass3() {
        let rules = [parse_rule("pass3 @123 @12")];
        let ctx = TableContext::default();
        let transform =
            ContextTable::compile(&rules, Direction::Forward, TranslationStage::Post2, &ctx)
                .unwrap();
        assert_eq!(transform.translate("⠇"), "⠃");
        assert_eq!(transform.translate("⠙"), "⠙");
        assert_eq!(transform.translate("🐂"), "🐂");
    }

    #[test]
    fn pass4() {
        let rules = [parse_rule("pass4 @123 @12")];
        let ctx = TableContext::default();
        let transform =
            ContextTable::compile(&rules, Direction::Forward, TranslationStage::Post3, &ctx)
                .unwrap();
        assert_eq!(transform.translate("⠇"), "⠃");
        assert_eq!(transform.translate("⠙"), "⠙");
        assert_eq!(transform.translate("🐂"), "🐂");
    }
}
