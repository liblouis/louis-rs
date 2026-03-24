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

    pub fn is_punctuation(&self, c: char) -> bool {
        self.get(&CharacterClass::Punctuation)
            .is_some_and(|class| class.contains(&c))
    }

    pub fn is_whitespace(&self, c: char) -> bool {
        self.get(&CharacterClass::Space)
            .is_some_and(|class| class.contains(&c))
    }

    pub fn is_numeric(&self, c: char) -> bool {
        [CharacterClass::Litdigit, CharacterClass::Digit]
            .iter()
            .any(|class| self.get(class).is_some_and(|class| class.contains(&c)))
    }

    pub fn is_word(&self, c: char) -> bool {
        [
            CharacterClass::Letter,
            CharacterClass::Uppercase,
            CharacterClass::Lowercase,
        ]
        .iter()
        .any(|class| self.get(class).is_some_and(|class| class.contains(&c)))
    }

    /// Return true if character `c` is at the beginning of a word
    pub fn is_word_start(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (None, Some(c)) => self.is_word(c),
            (Some(p), Some(c)) if self.is_word(c) => !self.is_word(p),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the end of a word
    pub fn is_word_end(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c), None) => self.is_word(c),
            (Some(p), Some(c)) if self.is_word(p) => !self.is_word(c),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the beginning of punctuation
    pub fn is_punctuation_start(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (None, Some(c)) => self.is_punctuation(c),
            (Some(p), Some(c)) if self.is_punctuation(c) => self.is_whitespace(p),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the end of a punctuation
    pub fn is_punctuation_end(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c), None) => self.is_punctuation(c),
            (Some(p), Some(c)) if self.is_punctuation(p) => self.is_whitespace(c),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the boundary between a word and punctuation
    pub fn is_word_punctuation(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c1), Some(c2)) => self.is_word(c1) && self.is_punctuation(c2),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the boundary between punctuation and a word
    pub fn is_punctuation_word(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c1), Some(c2)) => self.is_punctuation(c1) && self.is_word(c2),
            (_, _) => false,
        }
    }

    fn is_number_start(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (None, Some(c)) => self.is_numeric(c),
            (Some(p), Some(c)) if self.is_numeric(c) => !self.is_numeric(p),
            (_, _) => false,
        }
    }

    fn is_number_end(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c), None) => self.is_numeric(c),
            (Some(c1), Some(c2)) => self.is_numeric(c1) && !self.is_numeric(c2),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the boundary between a word and a
    /// number
    pub fn is_word_number(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c1), Some(c2)) => self.is_word(c1) && self.is_numeric(c2),
            (_, _) => false,
        }
    }

    /// Return true if character `c` is at the boundary between a number and a
    /// word
    pub fn is_number_word(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c1), Some(c2)) => self.is_numeric(c1) && self.is_word(c2),
            (_, _) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::CharacterClass;

    use super::*;

    #[test]
    fn word_start() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['c', 'x']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_word_start(Some(' '), Some('c')));
        assert!(ctx.is_word_start(None, Some('c')));
        assert!(!ctx.is_word_start(Some('x'), Some('c')));
        assert!(!ctx.is_word_start(Some('c'), None));
        assert!(!ctx.is_word_start(None, None));
        assert!(!ctx.is_word_start(Some(' '), Some(' ')));
        assert!(!ctx.is_word_start(Some(';'), Some('.')));
        assert!(!ctx.is_word_start(Some('c'), Some(' ')));
    }

    #[test]
    fn word_end() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &[';']),
            (CharacterClass::Letter, &['c', 'x']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_word_end(Some('c'), Some(' ')));
        assert!(ctx.is_word_end(Some('c'), None));
        assert!(ctx.is_word_end(Some('c'), Some('.')));
        assert!(!ctx.is_word_end(Some('x'), Some('c')));
        assert!(!ctx.is_word_end(None, Some('c')));
        assert!(!ctx.is_word_end(None, None));
        assert!(!ctx.is_word_end(Some(' '), Some(' ')));
        assert!(!ctx.is_word_end(Some(';'), Some('.')));
        assert!(!ctx.is_word_end(Some(' '), Some('c')));
    }
    #[test]
    fn number_start() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &[',', '.', ';']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['x']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_number_start(Some(' '), Some('1')));
        assert!(ctx.is_number_start(None, Some('1')));
        assert!(ctx.is_number_start(Some('x'), Some('1')));
        assert!(ctx.is_number_start(Some(','), Some('1')));
        assert!(!ctx.is_number_start(Some('c'), None));
        assert!(!ctx.is_number_start(None, None));
        assert!(!ctx.is_number_start(Some(' '), Some(' ')));
        assert!(!ctx.is_number_start(Some(';'), Some('.')));
        assert!(!ctx.is_number_start(Some('1'), Some(' ')));
    }

    #[test]
    fn number_end() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &[',', '.', ';']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['c']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_number_end(Some('1'), Some(' ')));
        assert!(ctx.is_number_end(Some('1'), None));
        assert!(ctx.is_number_end(Some('1'), Some('c')));
        assert!(!ctx.is_number_end(None, Some('c')));
        assert!(!ctx.is_number_end(None, None));
        assert!(!ctx.is_number_end(Some(' '), Some(' ')));
        assert!(!ctx.is_number_end(Some(';'), Some('.')));
    }

    #[test]
    fn word_number() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_word_number(Some('a'), Some('1')));
        // TODO: '#' is currently not recognized as a word
        //        assert!(word_number(Some('#'), Some('2')));
        assert!(!ctx.is_word_number(Some('1'), Some('1')));
        assert!(!ctx.is_word_number(Some('1'), Some(' ')));
        assert!(!ctx.is_word_number(Some('1'), Some('a')));
        assert!(!ctx.is_word_number(Some(' '), Some('1')));
        assert!(!ctx.is_word_number(Some('a'), Some('a')));
        assert!(!ctx.is_word_number(Some(' '), Some(' ')));
    }

    #[test]
    fn number_word() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_number_word(Some('1'), Some('a')));
        // TODO: '#' and '$' are currently not recognized as a word
        //        assert!(number_word(Some('2'), Some('#')));
        //        assert!(number_word(Some('2'), Some('$')));
        assert!(!ctx.is_number_word(Some('1'), Some('1')));
        assert!(!ctx.is_number_word(Some('1'), Some(' ')));
        assert!(!ctx.is_number_word(Some('a'), Some('1')));
        assert!(!ctx.is_number_word(Some(' '), Some('1')));
        assert!(!ctx.is_number_word(Some('a'), Some('a')));
        assert!(!ctx.is_number_word(Some(' '), Some(' ')));
        assert!(!ctx.is_number_word(None, Some('a')));
    }

    #[test]
    fn punctuation_start() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_punctuation_start(Some(' '), Some('(')));
        assert!(ctx.is_punctuation_start(Some(' '), Some(')')));
        assert!(ctx.is_punctuation_start(None, Some('(')));
        assert!(ctx.is_punctuation_start(None, Some(')')));
        assert!(!ctx.is_punctuation_start(Some('('), Some('(')));
        assert!(!ctx.is_punctuation_start(Some(')'), Some('(')));
        assert!(!ctx.is_punctuation_start(Some('a'), Some('(')));
        assert!(!ctx.is_punctuation_start(Some('1'), Some('(')));
        assert!(!ctx.is_punctuation_start(Some('('), Some(' ')));
        assert!(!ctx.is_punctuation_start(Some('('), Some('a')));
        assert!(!ctx.is_punctuation_start(Some('('), Some('1')));
        assert!(!ctx.is_punctuation_start(Some('('), Some(')')));
    }

    #[test]
    fn punctuation_end() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_punctuation_end(Some(')'), Some(' ')));
        assert!(ctx.is_punctuation_end(Some('('), Some(' ')));
        assert!(ctx.is_punctuation_end(Some('('), None));
        assert!(ctx.is_punctuation_end(Some(')'), None));
        assert!(!ctx.is_punctuation_end(Some('('), Some('(')));
        assert!(!ctx.is_punctuation_end(Some(')'), Some('(')));
        assert!(!ctx.is_punctuation_end(Some('a'), Some('(')));
        assert!(!ctx.is_punctuation_end(Some('1'), Some('(')));
        assert!(!ctx.is_punctuation_end(Some(' '), Some(')')));
        assert!(!ctx.is_punctuation_end(Some('('), Some('a')));
        assert!(!ctx.is_punctuation_end(Some('('), Some('1')));
        assert!(!ctx.is_punctuation_end(Some('('), Some(')')));
    }

    #[test]
    fn word_punctuation() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_word_punctuation(Some('a'), Some(')')));
        assert!(!ctx.is_word_punctuation(Some(' '), Some(')')));
        assert!(!ctx.is_word_punctuation(Some('('), None));
        assert!(!ctx.is_word_punctuation(Some(')'), None));
        assert!(!ctx.is_word_punctuation(Some('('), Some('(')));
        assert!(!ctx.is_word_punctuation(Some(')'), Some('(')));
        assert!(!ctx.is_word_punctuation(Some('1'), Some('(')));
        assert!(!ctx.is_word_punctuation(Some('('), Some('a')));
        assert!(!ctx.is_word_punctuation(Some('('), Some('1')));
        assert!(!ctx.is_word_punctuation(Some('('), Some(')')));
    }

    #[test]
    fn punctuation_word() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(ctx.is_punctuation_word(Some('('), Some('a')));
        assert!(ctx.is_punctuation_word(Some(')'), Some('a')));
        assert!(!ctx.is_punctuation_word(Some(' '), Some('a')));
        assert!(!ctx.is_punctuation_word(Some('('), None));
        assert!(!ctx.is_punctuation_word(Some(')'), None));
        assert!(!ctx.is_punctuation_word(None, Some('(')));
        assert!(!ctx.is_punctuation_word(None, Some(')')));
        assert!(!ctx.is_punctuation_word(Some('('), Some('(')));
        assert!(!ctx.is_punctuation_word(Some(')'), Some('(')));
        assert!(!ctx.is_punctuation_word(Some('1'), Some('(')));
        assert!(!ctx.is_punctuation_word(Some('('), Some('1')));
        assert!(!ctx.is_punctuation_word(Some('('), Some(')')));
    }
}
