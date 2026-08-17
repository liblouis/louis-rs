//! Create, modify and query named sets of characters
//!
//! Character classes or character attributes as liblouis also calls them, are named sets of
//! characters. They are mostly used in regular expressions but also for braille indication.

use std::collections::{HashMap, HashSet};

use crate::parser::braille::{BrailleChar, BrailleChars};
use crate::translator::CharacterDefinition;

// FIXME: isn't this the same as crate::parser::CharacterClass::Attribute?
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
pub struct CharacterClasses {
    classes: HashMap<CharacterClass, HashSet<char>>,
    /// User-defined classes in definition order, so that the `$w`-`$z` shorthands in multipass
    /// tests can refer to the first four of them by position
    user_defined: Vec<CharacterClass>,
}

impl CharacterClasses {
    pub fn new(mappings: &[(CharacterClass, &[char])]) -> Self {
        let mut character_classes = Self::default();
        for (class, chars) in mappings.iter().cloned() {
            for c in chars {
                character_classes.insert(class.clone(), *c);
            }
        }
        character_classes
    }

    /// Record a class in the definition order, without inserting any characters. Inserting also
    /// records, but a class whose characters all lack single-cell dots would otherwise never
    /// claim its position in the dots classes. Numbered attribute classes are excluded: liblouis
    /// keeps those in their own pool, separate from the classes `$w`-`$z` refer to.
    pub fn declare(&mut self, class: &CharacterClass) {
        if let CharacterClass::UserDefined(name) = class
            && !name.chars().all(|c| c.is_ascii_digit())
            && !self.user_defined.contains(class)
        {
            self.user_defined.push(class.clone());
        }
    }

    pub fn insert(&mut self, class: CharacterClass, c: char) -> bool {
        self.declare(&class);
        self.classes.entry(class).or_default().insert(c)
    }

    pub fn insert_dot(&mut self, class: CharacterClass, dot: &BrailleChar) -> bool {
        self.declare(&class);
        self.classes
            .entry(class)
            .or_default()
            .insert(dot.to_unicode())
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
        self.classes.get(class).cloned()
    }

    /// Get the characters of the nth user-defined class, in definition order and starting at 1,
    /// as referenced by the `$w`-`$z` shorthands in multipass tests
    pub fn get_by_order(&self, order: u8) -> Option<HashSet<char>> {
        let class = self.user_defined.get(usize::from(order).checked_sub(1)?)?;
        self.get(class)
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

    pub fn is_sign(&self, c: char) -> bool {
        self.get(&CharacterClass::Sign)
            .is_some_and(|class| class.contains(&c))
    }

    /// Return true if character `c` is at the boundary between a word (or digit) and punctuation.
    ///
    /// Digits are treated equivalently to word characters here so that `postpunc` rules fire
    /// correctly after numeric sequences (e.g. `postpunc , 2` should give the midnum dot pattern
    /// for `,` in `1,`).
    ///
    /// Sign characters (defined via the `sign` opcode) are also treated as word-like so that
    /// `postpunc` fires after script-specific combining characters (e.g. Tamil vowel signs
    /// that end Tamil syllables, making the next punctuation character use postpunc translation).
    pub fn is_word_punctuation(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (Some(c1), Some(c2)) => {
                (self.is_word(c1) || self.is_numeric(c1) || self.is_sign(c1))
                    && self.is_punctuation(c2)
            }
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

    pub fn is_number_start(&self, previous: Option<char>, c: Option<char>) -> bool {
        match (previous, c) {
            (None, Some(c)) => self.is_numeric(c),
            (Some(p), Some(c)) if self.is_numeric(c) => !self.is_numeric(p),
            (_, _) => false,
        }
    }

    pub fn is_number_end(&self, previous: Option<char>, c: Option<char>) -> bool {
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

    /// Return true if the preceding character is space, punctuation, or absent (start of string).
    /// Unlike the other boundary predicates this is lookbehind-only and does not constrain the
    /// current character.
    pub fn is_after_space_or_punct(&self, previous: Option<char>) -> bool {
        previous.is_none_or(|p| self.is_whitespace(p) || self.is_punctuation(p))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::CharacterClass;

    use super::*;

    #[test]
    fn by_order_follows_definition_order() {
        let mut classes = CharacterClasses::default();
        // built-in classes claim no position
        classes.insert(CharacterClass::Letter, 'a');
        classes.insert(CharacterClass::UserDefined("first".to_string()), 'x');
        // a declared but empty class still claims its position
        classes.declare(&CharacterClass::UserDefined("second".to_string()));
        classes.insert(CharacterClass::UserDefined("third".to_string()), 'y');
        assert_eq!(classes.get_by_order(1), Some(HashSet::from(['x'])));
        assert_eq!(classes.get_by_order(2), None);
        assert_eq!(classes.get_by_order(3), Some(HashSet::from(['y'])));
        assert_eq!(classes.get_by_order(4), None);
        assert_eq!(classes.get_by_order(0), None);
    }

    #[test]
    fn numbered_attributes_claim_no_by_order_position() {
        // Numbered attribute classes live in their own pool in liblouis and
        // never occupy the positions the `$w`-`$z` shorthands refer to.
        let mut classes = CharacterClasses::default();
        classes.insert(CharacterClass::UserDefined("1".to_string()), 'n');
        classes.insert(CharacterClass::UserDefined("vowel".to_string()), 'v');
        assert_eq!(classes.get_by_order(1), Some(HashSet::from(['v'])));
    }

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
        assert!(ctx.is_word_punctuation(Some('1'), Some('('))); // digit before punctuation fires postpunc
        assert!(!ctx.is_word_punctuation(Some('('), Some('a')));
        assert!(!ctx.is_word_punctuation(Some('('), Some('1')));
        assert!(!ctx.is_word_punctuation(Some('('), Some(')')));

        // sign characters (e.g. Tamil vowel signs) are treated as word-like for postpunc purposes
        let ctx_sign = CharacterClasses::new(&[
            (CharacterClass::Sign, &['ை']),
            (CharacterClass::Punctuation, &[',']),
        ]);
        assert!(ctx_sign.is_word_punctuation(Some('ை'), Some(',')));
        assert!(!ctx_sign.is_word_punctuation(Some(','), Some('ை')));
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
