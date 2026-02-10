//! Create, modify and query named sets of characters
//!
//! Character classes or character attributes as liblouis also calls them, are named sets of
//! characters. They are mostly used in regular expressions but also for braille indication.

use std::collections::{HashMap, HashSet};

use crate::parser::braille::{BrailleChar, BrailleChars};
use crate::translator::CharacterDefinition;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CharacterClassReference {
    Class(CharacterClass),
    ByOrder(u8),
    Boundary,
    Any,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CharacterClass {
    Space,
    Digit,
    Litdigit,
    Letter,
    Uppercase,
    Lowercase,
    Punctuation,
    Sign,
    Math,
    Seqdelimiter,
    Seqbeforechars,
    Seqafterchars,
    UserDefined(String),
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

/// A mapping between a character class and the associated set of characters
#[derive(Debug, Default, Clone)]
pub struct CharacterClasses(HashMap<CharacterClass, HashSet<char>>);

impl CharacterClasses {
    pub fn new(mappings: &[(CharacterClass, &[char])]) -> Self {
        Self(HashMap::from_iter(mappings.iter().cloned().map(
            |(class, chars)| (class, HashSet::from_iter(chars.iter().cloned())),
        )))
    }

    pub fn insert(&mut self, class: CharacterClass, c: char) -> bool {
        self.0.entry(class).or_default().insert(c)
    }

    pub fn insert_dot(&mut self, class: CharacterClass, dot: &BrailleChar) -> bool {
        self.0.entry(class).or_default().insert(dot.to_unicode())
    }

    pub fn insert_dots(&mut self, class: CharacterClass, dots: &BrailleChars) -> bool {
        if dots.len() == 1 {
            let dot = dots.iter().next().unwrap();
            self.insert_dot(class, dot)
        } else {
            false
        }
    }

    pub fn insert_associated_dot(
        &mut self,
        class: CharacterClass,
        c: char,
        character_definitions: &CharacterDefinition,
    ) -> bool {
        // if there is an associated single cell braille dot for the character add it to the
        // dots_classes
        if let Some(dots) = character_definitions.get(&c)
            && dots.chars().count() == 1
        {
            let dot = dots.chars().next().unwrap();
            self.insert(class, dot)
        } else {
            false
        }
    }

    pub fn get(&self, class: &CharacterClass) -> Option<HashSet<char>> {
        self.0.get(class).cloned()
    }
}

pub fn is_punctuation(ctx: &CharacterClasses, c: char) -> bool {
    ctx.get(&CharacterClass::Punctuation)
        .is_some_and(|class| class.contains(&c))
}

pub fn is_whitespace(ctx: &CharacterClasses, c: char) -> bool {
    ctx.get(&CharacterClass::Space)
        .is_some_and(|class| class.contains(&c))
}
