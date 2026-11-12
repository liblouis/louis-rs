//! A braille translator that uses [liblouis](https://liblouis.io) braille tables

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

pub use options::{TranslationMode, TranslationModes, TranslationOptions};
pub use pipeline::TranslationPipeline;
pub use positions::PositionMap;
pub use table::display::DisplayTable;
pub use translation::{ResolvedTranslation, TranslationStage};
pub use trie::ClassConstraints;

use crate::parser::{AnchoredRule, Braille, Rule};

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
#[derive(Debug)]
pub struct CompileError {
    path: Option<PathBuf>,
    line: usize,
    kind: CompileErrorKind,
}

impl CompileError {
    /// The table of the rule that failed to compile, or `None` for table source given in
    /// memory.
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    /// The line of the rule that failed to compile, counting from 1.
    pub fn line(&self) -> usize {
        self.line
    }

    /// What is wrong with the rule.
    pub fn kind(&self) -> &CompileErrorKind {
        &self.kind
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{}:{}: {}", path.display(), self.line, self.kind),
            None => write!(f, "line {}: {}", self.line, self.kind),
        }
    }
}

impl std::error::Error for CompileError {}

/// What is wrong with a rule that failed to compile, see [`CompileError`].
#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum CompileErrorKind {
    /// A rule uses a character's dots implicitly (`=`), but no rule defines that character
    #[error("implicit character {0:?} not defined")]
    ImplicitCharacterNotDefined(char),
    /// A `base` rule's base character is not defined in either direction
    #[error("base character {base:?} of {derived:?} not defined")]
    BaseCharacterNotDefined {
        /// the base character
        base: char,
        /// the character derived from it
        derived: char,
    },
    /// A rule refers to a swap class that no rule defines
    #[error("swap class {0} not defined")]
    SwapClassNotDefined(String),
}

impl CompileErrorKind {
    /// The error, at the location of `rule`
    pub(crate) fn at(self, rule: &AnchoredRule) -> CompileError {
        CompileError {
            path: rule.path().map(Path::to_path_buf),
            line: rule.line(),
            kind: self,
        }
    }
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

    fn resolve_implicit_dots(&self, chars: &str) -> Result<String, CompileErrorKind> {
        chars
            .chars()
            .map(|c| {
                self.get(&c)
                    .ok_or(CompileErrorKind::ImplicitCharacterNotDefined(c))
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
    fn braille_to_unicode(&self, dots: &Braille, chars: &str) -> Result<String, CompileErrorKind> {
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
