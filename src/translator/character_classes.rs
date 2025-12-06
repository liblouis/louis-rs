//! Create, modify and query named sets of characters
//!
//! Character classes or character attributes as liblouis also calls them, are named sets of
//! characters. They are mostly used in regular expressions but also for braille indication.

use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CharacterClass {
    Space,
    Digit,
    Letter,
    Uppercase,
    Lowercase,
    Punctuation,
    Sign,
    Seqdelimiter,
    Seqbeforechars,
    Seqafterchars,
    UserDefined(String),
    /// For historical reasons character classes can also be refered to by the number in which the
    /// appear in the source file. This really brittle and the feature should die.
    #[cfg(feature = "backwards_compatibility")]
    InOrderOfAppearance(u8),
}

impl From<&str> for CharacterClass {
    fn from(value: &str) -> Self {
        match value {
            "space" => CharacterClass::Space,
            "digit" => CharacterClass::Digit,
            "letter" => CharacterClass::Letter,
            "uppercase" => CharacterClass::Uppercase,
            "lowercase" => CharacterClass::Lowercase,
            "punctuation" => CharacterClass::Punctuation,
            "sign" => CharacterClass::Sign,
            "seqdelimiter" => CharacterClass::Seqdelimiter,
            "seqbeforechars" => CharacterClass::Seqbeforechars,
            "seqafterchars" => CharacterClass::Seqafterchars,
            class => Self::UserDefined(class.to_string()),
        }
    }
}

impl From<u8> for CharacterClass {
    fn from(value: u8) -> Self {
        CharacterClass::InOrderOfAppearance(value)
    }
}

/// A mapping between a character class and the associated set of characters
#[derive(Debug, Default)]
pub struct CharacterClasses(HashMap<CharacterClass, HashSet<char>>);

impl CharacterClasses {
    pub fn insert(&mut self, class: CharacterClass, c: char) -> bool {
        self.0.entry(class).or_default().insert(c)
    }

    pub fn get(&self, class: &CharacterClass) -> Option<HashSet<char>> {
        self.0.get(class).cloned()
    }
}
