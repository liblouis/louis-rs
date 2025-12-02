use crate::translator::trie::{Boundary, Trie};

use crate::parser::{AnchoredRule, Direction, Rule};
use crate::parser::{HasDirection, Precedence};
use crate::translator::{Translation, TranslationError, TranslationStage};

use crate::parser::IsLiteral;

#[derive(Debug, Default)]
pub struct TranslationTable {
    /// A prefix tree that contains all the translation rules and their [`Translations`](Translation)
    trie: Trie,
    direction: Direction,
}

/// A builder for [`TranslationTable`]
#[derive(Debug)]
struct TranslationTableBuilder {
    trie: Trie,
}

impl TranslationTableBuilder {
    fn new() -> Self {
        Self { trie: Trie::new() }
    }

    fn build(self, direction: Direction) -> TranslationTable {
        TranslationTable {
            direction,
            trie: self.trie,
        }
    }
}

impl TranslationTable {
    pub fn is_empty(&self) -> bool {
        self.trie.is_empty()
    }

    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> Result<Self, TranslationError> {
        let mut builder = TranslationTableBuilder::new();

        let rules = rules.iter().filter(|r| r.is_direction(direction));

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
                            TranslationStage::Pre,
                            rule,
                        );
                    }
                }
                _ => (),
            }
        }

        Ok(builder.build(direction))
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input)
            .iter()
            .map(|t| t.output.as_str())
            .collect()
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
                .max_by_key(|translation| translation.weight);
            if let Some(t) = candidate {
                // there is a matching translation rule
                let translation = t.clone();
                // move the iterator forward by the number of characters in the translation
                chars.nth(t.length - 1);
                prev = translation.input.chars().last();
                translations.push(translation);
            } else if let Some(next_char) = chars.next() {
                prev = Some(next_char);
                // no translation rule found, just pass the character through
                let translation =
                    Translation::new(&next_char.to_string(), &next_char.to_string(), 1, None);
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
    fn translate() {
        let rules = vec![parse_rule(r#"correct "corect" "correct""#)];
        let table = TranslationTable::compile(&rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("foobar"), "foobar");
        assert_eq!(table.translate("corect"), "correct");
        assert_eq!(table.translate("🐂"), "🐂");
    }
}
