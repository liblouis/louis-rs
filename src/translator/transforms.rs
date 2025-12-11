use crate::parser::IsLiteral;
use crate::translator::Translation;
use crate::{
    Direction,
    parser::{AnchoredRule, Precedence, Rule},
    translator::{
        TranslationError, TranslationStage,
        trie::{Boundary, Trie},
    },
};

#[derive(Debug, Default)]
pub struct TransformationTable {
    /// A prefix tree that contains all the translation rules and their [`Translations`](Translation)
    trie: Trie,
    stage: TranslationStage,
    direction: Direction,
}

/// A builder for [`TransformationTable`]
#[derive(Debug)]
struct TransformationTableBuilder {
    trie: Trie,
}

impl TransformationTableBuilder {
    fn new() -> Self {
        Self { trie: Trie::new() }
    }

    fn build(self, direction: Direction, stage: TranslationStage) -> TransformationTable {
        TransformationTable {
            direction,
            stage,
            trie: self.trie,
        }
    }
}

impl TransformationTable {
    pub fn is_empty(&self) -> bool {
        self.trie.is_empty()
    }

    pub fn compile(
        rules: &[&AnchoredRule],
        direction: Direction,
        stage: TranslationStage,
    ) -> Result<Self, TranslationError> {
        let mut builder = TransformationTableBuilder::new();

        for rule in rules {
            match &rule.rule {
                Rule::Correct { test, action, .. } => {
                    // FIXME: For now we just use the correct rules that use literal regexps since
                    // we do not really support the whole range of weirdness that liblouis regexps
                    // encompass
                    if test.is_literal() && action.is_literal() {
                        let from: String = test.try_into().unwrap();
                        let to: String = action.try_into().unwrap();
                        if direction == Direction::Backward && to.is_empty() {
                            // Correct rules can map to an empty string, i.e. to drop some
                            // characters. Such rules cannot be used for backtranslation (you cannot
                            // translate the empty string to something)
                            continue;
                        }
                        builder.trie.insert(
                            &from,
                            &to,
                            Boundary::None,
                            Boundary::None,
                            direction,
                            Precedence::Default,
                            stage,
                            rule,
                        );
                    }
                }
                Rule::Pass2 { test, action, .. } => {
                    // FIXME: For now we just use the correct rules that use literal regexps since
                    // we do not really support the whole range of weirdness that liblouis regexps
                    // encompass
                    if test.is_literal() && action.is_literal() {
                        let from: String = test.try_into().unwrap();
                        let to: String = action.try_into().unwrap();
                        if direction == Direction::Backward && to.is_empty() {
                            continue;
                        }
                        builder.trie.insert(
                            &from,
                            &to,
                            Boundary::None,
                            Boundary::None,
                            direction,
                            Precedence::Default,
                            stage,
                            rule,
                        );
                    }
                }
                Rule::Pass3 { test, action, .. } => {
                    // FIXME: For now we just use the correct rules that use literal regexps since
                    // we do not really support the whole range of weirdness that liblouis regexps
                    // encompass
                    if test.is_literal() && action.is_literal() {
                        let from: String = test.try_into().unwrap();
                        let to: String = action.try_into().unwrap();
                        if direction == Direction::Backward && to.is_empty() {
                            continue;
                        }
                        builder.trie.insert(
                            &from,
                            &to,
                            Boundary::None,
                            Boundary::None,
                            direction,
                            Precedence::Default,
                            stage,
                            rule,
                        );
                    }
                }
                Rule::Pass4 { test, action, .. } => {
                    // FIXME: For now we just use the correct rules that use literal regexps since
                    // we do not really support the whole range of weirdness that liblouis regexps
                    // encompass
                    if test.is_literal() && action.is_literal() {
                        let from: String = test.try_into().unwrap();
                        let to: String = action.try_into().unwrap();
                        if direction == Direction::Backward && to.is_empty() {
                            continue;
                        }
                        builder.trie.insert(
                            &from,
                            &to,
                            Boundary::None,
                            Boundary::None,
                            direction,
                            Precedence::Default,
                            stage,
                            rule,
                        );
                    }
                }
                _ => (),
            }
        }

        Ok(builder.build(direction, stage))
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input).iter().map(|t| t.output()).collect()
    }

    fn translation_candidates(&self, input: &str, prev: Option<char>) -> Vec<Translation> {
        self.trie
            .find_translations(input, prev)
            .into_iter()
            .collect()
    }

    pub fn trace(&self, input: &str) -> Vec<Translation> {
        let mut translations: Vec<Translation> = Vec::new();
        let mut chars = input.chars();
        let mut prev: Option<char> = None;

        loop {
            // given an input query the trie for matching translations
            let candidates = self.translation_candidates(chars.as_str(), prev);

            // use the longest translation
            let candidate = candidates
                .iter()
                .max_by_key(|translation| translation.weight());
            if let Some(t) = candidate {
                // there is a matching translation rule
                let translation = t.clone();
                // move the iterator forward by the number of characters in the translation
                chars.nth(t.length() - 1);
                prev = translation.input().chars().last();
                translations.push(translation);
            } else if let Some(next_char) = chars.next() {
                prev = Some(next_char);
                // no translation rule found, just pass the character through
                let translation = Translation::new(
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
        let table = TransformationTable::compile(&rules, Direction::Forward, TranslationStage::Pre)
            .unwrap();
        assert_eq!(table.translate("foobar"), "foobar");
        assert_eq!(table.translate("corect"), "correct");
        assert_eq!(table.translate("🐂"), "🐂");
    }

    #[test]
    fn pass2() {
        let rules = [&parse_rule("pass2 @123 @12")];
        let table =
            TransformationTable::compile(&rules, Direction::Forward, TranslationStage::Post1)
                .unwrap();
        assert_eq!(dbg!(&table).translate("⠇"), "⠃");
        assert_eq!(table.translate("⠙"), "⠙");
        assert_eq!(table.translate("🐂"), "🐂");
    }

    #[test]
    fn pass3() {
        let rules = [&parse_rule("pass3 @123 @12")];
        let table =
            TransformationTable::compile(&rules, Direction::Forward, TranslationStage::Post2)
                .unwrap();
        assert_eq!(dbg!(&table).translate("⠇"), "⠃");
        assert_eq!(table.translate("⠙"), "⠙");
        assert_eq!(table.translate("🐂"), "🐂");
    }

    #[test]
    fn pass4() {
        let rules = [&parse_rule("pass4 @123 @12")];
        let table =
            TransformationTable::compile(&rules, Direction::Forward, TranslationStage::Post3)
                .unwrap();
        assert_eq!(dbg!(&table).translate("⠇"), "⠃");
        assert_eq!(table.translate("⠙"), "⠙");
        assert_eq!(table.translate("🐂"), "🐂");
    }
}
