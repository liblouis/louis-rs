use std::collections::{HashMap, HashSet};

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum CharacterAttribute {
    Space,
    Digit,
    Letter,
    Lowercase,
    Uppercase,
    Punctuation,
    Sign,
    Math,
    Litdigit,
}

#[derive(Debug, Default, PartialEq)]
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
            (CharacterAttribute::Letter, HashSet::from(['a', 'b'])),
            (CharacterAttribute::Math, HashSet::from(['∑', '𝛑'])),
        ]));
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::Letter, 'a'),
            true
        );
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::Sign, 'a'),
            false
        );
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::Letter, 'c'),
            false
        );
        assert_eq!(
            char_attr.has_attribute(CharacterAttribute::Math, 'c'),
            false
        );
        assert_eq!(char_attr.has_attribute(CharacterAttribute::Math, '𝛑'), true);
    }
}
