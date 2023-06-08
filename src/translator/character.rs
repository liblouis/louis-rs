use std::collections::{HashMap, HashSet};

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum CharacterAttribute {
    SPACE,
    DIGIT,
    LETTER,
    LOWERCASE,
    UPPERCASE,
    PUNCTUATION,
    SIGN,
    MATH,
    LITDIGIT,
}

#[derive(Debug, Default)]
pub struct CharacterAttributes(HashMap<CharacterAttribute, HashSet<char>>);

impl CharacterAttributes {
    pub fn has_attribute(&self, attr: CharacterAttribute, c: char) -> bool {
        if let Some(s) = self.0.get(&attr) {
            return s.contains(&c);
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn has_attribute_test() {
        let char_attr = CharacterAttributes(HashMap::from([
            (CharacterAttribute::LETTER, HashSet::from(['a', 'b'])),
            (CharacterAttribute::MATH, HashSet::from(['∑', '𝛑'])),
        ]));
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::LETTER, 'a'),
            true
        );
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::SIGN, 'a'),
            false
        );
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::LETTER, 'c'),
            false
        );
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::MATH, 'c'),
            false
        );
        assert_eq!(char_attr.has_attribute(CharacterAttribute::MATH, '𝛑'), true);
    }
}
