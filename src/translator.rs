//! A braille translator that uses [liblouis](https://liblouis.io) braille tables

use std::{collections::HashMap, io};

pub use pipeline::TranslationPipeline;
pub use translation::{ResolvedTranslation, TranslationStage};

use crate::{
    Direction,
    parser::{AnchoredRule, Braille, HasDirection, Rule, dots_to_unicode},
};

mod boundaries;
mod context_pattern;
mod indication;
mod match_pattern;
mod nfa;
mod pipeline;
mod regexp;
mod swap;
mod table;
mod translation;
mod trie;

#[derive(thiserror::Error, Debug)]
pub enum TranslationError {
    #[error("Implicit character {0:?} not defined")]
    ImplicitCharacterNotDefined(char),
    #[error("Character in base rule not defined: derived: {derived:?}, base: {base:?}")]
    BaseCharacterNotDefined { base: char, derived: char },
    #[error("Swap class {0} not defined")]
    SwapClassNotDefined(String),
    #[error(transparent)]
    HyphenationTableIoError(#[from] io::Error),
    #[error(transparent)]
    HyphenationTableLoadError(#[from] hyphenation::load::Error),
}

#[derive(Debug, Default, Clone)]
struct CharacterDefinition(HashMap<char, String>);

impl CharacterDefinition {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, from: char, to: &str) {
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            self.0.entry(from).or_insert(to.to_string());
        } else {
            // last rule wins
            self.0.insert(from, to.to_string());
        }
    }

    fn get(&self, from: &char) -> Option<&String> {
        self.0.get(from)
    }

    fn resolve_implicit_dots(&self, chars: &str) -> Result<String, TranslationError> {
        chars
            .chars()
            .map(|c| {
                self.get(&c)
                    .ok_or(TranslationError::ImplicitCharacterNotDefined(c))
                    .map(|t| t.to_string())
            })
            .collect()
    }

    /// Convert braille dots to Unicode characters.
    ///
    /// Convert given braille `dots` to Unicode characters. If the dots are
    /// [explicit](Braille::Explicit) then simply delegate to the [`dots_to_unicode`] function.
    /// Otherwise, if the dots are [implicit](Braille::Implicit) convert the given `chars` to
    /// braille with the given `character_definitions` and using the [`Self::resolve_implicit_dots`]
    /// function.
    ///
    /// Returns the braille Unicode characters or [`TranslationError`] if the implicit characters
    /// could not be converted.
    fn braille_to_unicode(&self, dots: &Braille, chars: &str) -> Result<String, TranslationError> {
        let dots = match dots {
            Braille::Implicit => self.resolve_implicit_dots(chars)?,
            Braille::Explicit(dots) => dots_to_unicode(dots),
        };
        Ok(dots)
    }
}

#[derive(Debug)]
pub struct DisplayTable {
    dots_to_char: HashMap<char, char>,
}

impl DisplayTable {
    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> DisplayTable {
        let mut mapping = HashMap::new();
        let rules: Vec<_> = rules
            .into_iter()
            .filter(|r| r.is_direction(direction))
            .collect();

        for rule in rules {
            match &rule.rule {
                Rule::Display {
                    character, dots, ..
                } => {
                    if cfg!(feature = "backwards_compatibility") {
                        // first rule wins
                        let key = dots_to_unicode(&dots).chars().nth(0).unwrap();
                        mapping.entry(key).or_insert(*character);
                    } else {
                        // last rule wins
                        mapping.insert(dots_to_unicode(&dots).chars().nth(0).unwrap(), *character);
                    }
                }
                _ => (), // ignore all other rules for display tables
            }
        }
        DisplayTable {
            dots_to_char: mapping,
        }
    }

    /// Map the `input` to the output using the display rules in the
    /// `DisplayTable`.
    ///
    /// If the `DisplayTable` does not contain a mapping for a
    /// specific char then the original character is returned
    pub fn translate(&self, input: &str) -> String {
        input
            .chars()
            .map(|ref c| *self.dots_to_char.get(c).unwrap_or(c))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        parser::RuleParser, translator::table::TableContext,
        translator::table::primary::PrimaryTable,
    };

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn resolve_implicit_dots() {
        let char_defs = CharacterDefinition::new();
        assert!(char_defs.resolve_implicit_dots("xs").is_err());
        let mut char_defs = CharacterDefinition::new();
        char_defs.insert('a', "A");
        char_defs.insert('h', "H");
        assert_eq!(char_defs.resolve_implicit_dots("haha").unwrap(), "HAHA");
    }

    #[test]
    fn display_table() {
        let display_rules = [parse_rule("display a 1"), parse_rule("display \\s 0")];
        let display_table = DisplayTable::compile(&display_rules, Direction::Forward);
        assert_eq!(display_table.translate("⠁"), "a");
        assert_eq!(display_table.translate("⠀"), " ");
        assert_eq!(display_table.translate(""), "");
        assert_eq!(display_table.translate("x"), "x"); // unknown chars are translated to themselves
    }

    #[test]
    fn translate_with_display() {
        let display_rules = [parse_rule("display A 1"), parse_rule("display \\s 0")];
        let rules = [parse_rule("letter a 1"), parse_rule("space \\s 0")];
        let display_table = DisplayTable::compile(&display_rules, Direction::Forward);
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(display_table.translate(&table.translate("a")), "A");
        assert_eq!(display_table.translate(&table.translate(" ")), " ");
        assert_eq!(display_table.translate(&table.translate("a a")), "A A");
    }
}
