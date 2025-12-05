//! Create, modify and query named sets of characters
//!
//! Character classes or character attributes as liblouis also calls them, are named sets of
//! characters. They are mostly used in regular expressions but also for braille indication.

use std::collections::{HashMap, HashSet};

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum CharacterClassError {
    #[error("The class name {0} is reserved or already defined.")]
    AlreadyDefined(String),
    #[error("The class name {0} is not defined.")]
    ClassNotDefined(CharacterClass),
}

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

impl std::fmt::Display for CharacterClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class = match self {
            CharacterClass::Space => "space",
            CharacterClass::Digit => "digit",
            CharacterClass::Letter => "letter",
            CharacterClass::Uppercase => "uppercase",
            CharacterClass::Lowercase => "lowercase",
            CharacterClass::Punctuation => "punctuation",
            CharacterClass::Sign => "sign",
            CharacterClass::Seqdelimiter => "seqdelimiter",
            CharacterClass::Seqbeforechars => "seqbeforechars",
            CharacterClass::Seqafterchars => "seqafterchars",
            CharacterClass::UserDefined(name) => name,
        };
        write!(f, "{}", class)
    }
}

/// A mapping between a character class and the associated set of characters
#[derive(Debug, Default)]
pub struct CharacterClasses(HashMap<CharacterClass, HashSet<char>>);

impl CharacterClasses {
    pub fn new() -> Self {
        let mut mapping = HashMap::new();

        for class in [
            CharacterClass::Space,
            CharacterClass::Digit,
            CharacterClass::Letter,
            CharacterClass::Uppercase,
            CharacterClass::Lowercase,
            CharacterClass::Punctuation,
            CharacterClass::Sign,
            CharacterClass::Seqdelimiter,
            CharacterClass::Seqbeforechars,
            CharacterClass::Seqafterchars,
        ] {
            mapping.insert(class, HashSet::default());
        }

        Self(mapping)
    }

    /// Insert a new character class. Returns yan error if the class `name` has not been defined
    /// before.
    pub fn insert_class(
        &mut self,
        name: &str,
        chars: HashSet<char>,
    ) -> Result<(), CharacterClassError> {
        let class = CharacterClass::from(name);
        // Only add the class if hasn't been defined before
        if self.0.contains_key(&class) {
            return Err(CharacterClassError::AlreadyDefined(name.to_string()));
        }
        self.0.insert(class, chars);
        Ok(())
    }

    pub fn insert(&mut self, class: CharacterClass, c: char) -> Result<(), CharacterClassError> {
        if let Some(set) = self.0.get_mut(&class) {
            set.insert(c);
            Ok(())
        } else {
            Err(CharacterClassError::ClassNotDefined(class))
        }
    }

    pub fn get(&self, class: &CharacterClass) -> Option<HashSet<char>> {
        self.0.get(class).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn insert_class() {
        let mut classes = CharacterClasses::new();
        let chars = HashSet::from(['a', 'b']);

        let result = classes.insert_class("custom", chars.clone());
        assert!(result.is_ok());
        assert_eq!(
            classes.get(&CharacterClass::UserDefined("custom".to_string())),
            Some(chars)
        );
    }

    #[test]
    fn insert_class_already_defined() {
        let mut classes = CharacterClasses::new();
        let chars = HashSet::new();

        // Try to redefine a built-in class
        let result = classes.insert_class("space", chars);
        assert_eq!(
            result,
            Err(CharacterClassError::AlreadyDefined("space".to_string()))
        );

        // Try to define a custom class twice
        classes.insert_class("custom", HashSet::new()).unwrap();
        let result = classes.insert_class("custom", HashSet::new());
        assert_eq!(
            result,
            Err(CharacterClassError::AlreadyDefined("custom".to_string()))
        );
    }

    #[test]
    fn insert_char() {
        let mut classes = CharacterClasses::new();

        let result = classes.insert(CharacterClass::Space, ' ');
        assert!(result.is_ok());

        let expected = HashSet::from([' ']);
        assert_eq!(classes.get(&CharacterClass::Space), Some(expected));
    }

    #[test]
    fn insert_char_class_not_defined() {
        let mut classes = CharacterClasses::new();
        let undefined_class = CharacterClass::UserDefined("undefined".to_string());

        let result = classes.insert(undefined_class.clone(), 'a');
        assert_eq!(
            result,
            Err(CharacterClassError::ClassNotDefined(undefined_class))
        );
    }

    #[test]
    fn get_nonexistent_class() {
        let classes = CharacterClasses::new();
        let result = classes.get(&CharacterClass::UserDefined("nonexistent".to_string()));
        assert_eq!(result, None);
    }
}
