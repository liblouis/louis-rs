//! Does a word begin here? Back-translation answers this from the text already produced.
//!
//! It reads the output backward and stops at the first character that decides: a space
//! means a word begins, a letter, digit, math sign or sign means one is under way, and any
//! other character, such as punctuation, is skipped.

use std::collections::HashSet;

use crate::parser::{CharacterClass, CharacterClasses};

#[derive(Default, Debug, Clone)]
pub struct BegWordScanner {
    space: HashSet<char>,
    word: HashSet<char>,
}

impl BegWordScanner {
    pub fn new(classes: &CharacterClasses) -> Self {
        let space = classes.get(&CharacterClass::Space).unwrap_or_default();
        let mut word = HashSet::new();
        for class in [
            CharacterClass::Letter,
            CharacterClass::Digit,
            CharacterClass::Math,
            CharacterClass::Sign,
        ] {
            word.extend(classes.get(&class).unwrap_or_default());
        }
        BegWordScanner { space, word }
    }

    /// Does a word begin after `preceding`, the output produced so far?
    /// Output that runs out without deciding begins a word.
    pub fn is_beg_word(&self, preceding: &str) -> bool {
        for c in preceding.chars().rev() {
            if self.space.contains(&c) {
                return true;
            }
            if self.word.contains(&c) {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_beg_word_scans_until_something_decides() {
        let classes = CharacterClasses::new(&[
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Punctuation, &['(']),
        ]);
        let scanner = BegWordScanner::new(&classes);

        // nothing produced yet
        assert!(scanner.is_beg_word(""));
        assert!(scanner.is_beg_word("a "));
        assert!(!scanner.is_beg_word("a"));
        // punctuation is skipped
        assert!(scanner.is_beg_word("a ("));
        assert!(!scanner.is_beg_word("a("));
    }
}
