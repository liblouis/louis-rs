//! A braille translator that uses [liblouis](https://liblouis.io) braille tables

use std::collections::HashMap;

pub use options::{TranslationMode, TranslationModes, TranslationOptions};
pub use pipeline::TranslationPipeline;
pub use positions::PositionMap;
pub use table::display::DisplayTable;
pub use translation::{ResolvedTranslation, TranslationStage};
pub use trie::ClassConstraints;

use crate::parser::{Braille, Rule};

mod beg_word;
mod context_pattern;
mod effect;
mod end_word;
mod indication;
mod match_pattern;
mod options;
mod pipeline;
mod position_constraints;
mod positions;
mod regexp;
mod standing_alone;
mod swap;
mod table;
mod translation;
mod trie;

/// An error in compiling the rules of a table.
///
/// Every line of the table parsed, but a rule refers to something that no rule in the
/// table or its includes defines, such as a character or a swap class. Errors in reading
/// or parsing a table are [`TableError`](crate::TableError)s instead.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum CompileError {
    /// A rule uses a character's dots implicitly (`=`), but no rule defines that character
    #[error("Implicit character {0:?} not defined")]
    ImplicitCharacterNotDefined(char),
    /// A `base` rule's base character is not defined in either direction
    #[error("Base character {base:?} of {derived:?} not defined")]
    BaseCharacterNotDefined {
        /// the base character
        base: char,
        /// the character derived from it
        derived: char,
    },
    /// A rule refers to a swap class that no rule defines
    #[error("Swap class {0} not defined")]
    SwapClassNotDefined(String),
    #[error("{0} is neither a class nor a swap")]
    ClassNotDefined(String),
}

#[derive(Debug, Default, Clone)]
pub struct CharacterDefinition(HashMap<char, String>);

impl CharacterDefinition {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, from: char, to: &str) {
        // first rule wins, as in liblouis
        self.0.entry(from).or_insert(to.to_string());
    }

    pub fn get(&self, from: &char) -> Option<&String> {
        self.0.get(from)
    }

    /// The dots of every definition.
    pub fn dots(&self) -> impl Iterator<Item = &str> {
        self.0.values().map(String::as_str)
    }

    /// Whether some character is defined with exactly these dots.
    pub fn defines_dots(&self, dots: &str) -> bool {
        self.0.values().any(|d| d == dots)
    }

    fn resolve_implicit_dots(&self, chars: &str) -> Result<String, CompileError> {
        chars
            .chars()
            .map(|c| {
                self.get(&c)
                    .ok_or(CompileError::ImplicitCharacterNotDefined(c))
                    .map(|t| t.to_string())
            })
            .collect()
    }

    /// Convert braille dots to Unicode characters.
    ///
    /// Convert given braille `dots` to Unicode characters. If the dots are
    /// [explicit](Braille::Explicit) then simply delegate to `dots.to_string()`.
    /// Otherwise, if the dots are [implicit](Braille::Implicit) convert the given `chars` to
    /// braille with the given `character_definitions` and using the [`Self::resolve_implicit_dots`]
    /// function.
    ///
    /// Returns the braille Unicode characters or [`CompileError`] if the implicit characters
    /// could not be converted.
    fn braille_to_unicode(&self, dots: &Braille, chars: &str) -> Result<String, CompileError> {
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
