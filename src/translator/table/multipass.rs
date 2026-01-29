use std::collections::HashSet;

use crate::parser::multipass::ConsumesInput;
use crate::translator::ResolvedTranslation;
use crate::translator::context_pattern::{ContextPatterns, ContextPatternsBuilder};
use crate::translator::effect::Environment;
use crate::translator::table::TableContext;
use crate::translator::translation::TranslationSubset;
use crate::{
    Direction,
    parser::{AnchoredRule, Rule},
    translator::{TranslationError, TranslationStage},
};

#[derive(Debug)]
pub struct MultipassTable {
    patterns: ContextPatterns,
    stage: TranslationStage,
    direction: Direction,
}

/// A builder for [`MultipassTable`]
#[derive(Debug)]
struct MultipassTableBuilder {
    patterns: ContextPatternsBuilder,
}

impl MultipassTableBuilder {
    fn new() -> Self {
        Self {
            patterns: ContextPatternsBuilder::new(),
        }
    }

    fn build(self, direction: Direction, stage: TranslationStage) -> MultipassTable {
        MultipassTable {
            direction,
            stage,
            patterns: self.patterns.build(),
        }
    }
}

impl MultipassTable {
    pub fn compile(
        rules: &[AnchoredRule],
        direction: Direction,
        stage: TranslationStage,
        ctx: &TableContext,
    ) -> Result<Self, TranslationError> {
        let mut builder = MultipassTableBuilder::new();

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

    fn translation_candidates(
        &self,
        input: &str,
        prev: Option<char>,
        env: &Environment,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.patterns.find(input, env)
            .into_iter()
            .partition(|t| t.offset() == 0)
    }

    fn update_offsets(
        &self,
        translations: Vec<ResolvedTranslation>,
        decrement: usize,
    ) -> Vec<ResolvedTranslation> {
        translations
            .into_iter()
	    // drop translations where the offet is smaller than the decrement
            .filter(|t| t.offset() >= decrement)
            .map(|t| t.decrement_offset(decrement))
            .collect()
    }

    fn partition_delayed_translations(
        &self,
        delayed: Vec<ResolvedTranslation>,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        delayed.into_iter().partition(|t| t.offset() == 0)
    }

    pub fn trace(&self, input: &str) -> Vec<ResolvedTranslation> {
        let mut translations: Vec<ResolvedTranslation> = Vec::new();
        let mut delayed_translations: Vec<ResolvedTranslation> = Vec::new();
        let mut env = Environment::new();
        let mut chars = input.chars();
        let mut prev: Option<char> = None;
        let mut seen: HashSet<TranslationSubset> = HashSet::default();

        loop {
	    // given an input query the patterns for matching translations. Then split off the
            // translations that are delayed, i.e. have an offset because they have a pre-pattern
            let (mut candidates, delayed) = self.translation_candidates(chars.as_str(), prev, &env);
            delayed_translations.extend(delayed);

            // move delayed_translations with zero offset into candidates
            let (current, delayed) = self.partition_delayed_translations(delayed_translations);
            delayed_translations = delayed;
            candidates.extend(current);

            // use the longest translation
            let candidate = candidates
                .iter()
                // drop translation candidates that we have applied already at this position in the
                // input
                .filter(|&t| !seen.contains(&TranslationSubset::from(t)))
                .cloned()
                .max_by_key(|translation| translation.weight());
            if let Some(t) = candidate {
                if t.length() == 0 {
                    // if there is a zero-length translation candiate we run the risk of an infinite
                    // loop, so remember the current translation so we only apply it once
                    seen.insert(TranslationSubset::from(&t));
                } else {
                    seen.clear();
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(t.length() - 1);
                    prev = t.input().chars().last();
                }
                // there is a matching translation rule
                let translation = t.clone();
                translations.push(translation);
                delayed_translations = self.update_offsets(delayed_translations, t.length());

                // update the environment if needed
                if !t.effects().is_empty() {
                    for effect in t.effects() {
                        env.apply(effect);
                    }
                }
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
                delayed_translations = self.update_offsets(delayed_translations, 1);
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
            MultipassTable::compile(&rules, Direction::Forward, TranslationStage::Pre, &ctx)
                .unwrap();
        assert_eq!(transform.translate("foobar"), "foobar");
        assert_eq!(transform.translate("corect"), "correct");
        assert_eq!(transform.translate("🐂"), "🐂");
    }

    #[test]
    fn pass2() {
        let rules = [parse_rule("pass2 @123 @12")];
        let ctx = TableContext::default();
        let transform =
            MultipassTable::compile(&rules, Direction::Forward, TranslationStage::Post1, &ctx)
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
            MultipassTable::compile(&rules, Direction::Forward, TranslationStage::Post2, &ctx)
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
            MultipassTable::compile(&rules, Direction::Forward, TranslationStage::Post3, &ctx)
                .unwrap();
        assert_eq!(transform.translate("⠇"), "⠃");
        assert_eq!(transform.translate("⠙"), "⠙");
        assert_eq!(transform.translate("🐂"), "🐂");
    }

    #[test]
    fn effects() {
        let rules = [
            // when encountering @123 set var 1 to 1
            parse_rule("pass2 @123 @123#1=1"),
            // while var 1 is 1 transform @1 to @6
            parse_rule("pass2 #1=1@1 @6"),
            // when encountering @456 set var 1 back to 0
            parse_rule("pass2 @456 @456#1=0"),
        ];
        let ctx = TableContext::default();
        let transform =
            MultipassTable::compile(&rules, Direction::Forward, TranslationStage::Post1, &ctx)
                .unwrap();
        dbg!(&transform);
        assert_eq!(transform.translate("⠁⠁⠇⠁⠁⠸⠁⠁"), "⠁⠁⠇⠠⠠⠸⠁⠁");
        assert_eq!(transform.translate("🐂"), "🐂");
    }
}
