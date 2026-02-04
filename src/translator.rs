//! A braille translator that uses [liblouis](https://liblouis.io) braille tables

use std::{collections::HashMap, io};

pub use pipeline::TranslationPipeline;
pub use translation::{ResolvedTranslation, TranslationStage};

use crate::parser::Braille;

mod boundaries;
mod context_pattern;
mod effect;
mod indication;
mod match_pattern;
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
            Braille::Explicit(dots) => dots.to_string(),
        };
        Ok(dots)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{AnchoredRule, RuleParser};

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
}
