//! Does a match stand alone? liblouis's check for `word` and `contraction` rules in a table
//! with a `seqdelimiter` (its `inSequence`).
//!
//! Such a rule applies only when, going back from the match over `seqbeforechars`
//! characters, a space, a `seqdelimiter` or the start comes next, and going on after it over
//! `seqafterchars` characters and `seqafterpattern` strings, a space, a `seqdelimiter` or
//! the end. In UEB, "(about)" stands alone and is the shortform ⠁⠃, while "!about" does
//! not and is spelled out.

use std::collections::HashSet;

use crate::{
    parser::{CharacterClass, CharacterClasses},
    translator::trie::{Transition, Trie},
};

#[derive(Default, Debug, Clone)]
pub struct StandingAloneScanner {
    /// The spaces and `seqdelimiter` characters, which a sequence stops at.
    delimiters: HashSet<char>,
    before_chars: HashSet<char>,
    after_chars: HashSet<char>,
    after_patterns: Vec<String>,
}

impl StandingAloneScanner {
    /// The scanner for a table, or none if it has no `seqdelimiter`: liblouis then only
    /// checks for a space or punctuation on either side.
    pub fn new(classes: &CharacterClasses, after_patterns: Vec<String>) -> Option<Self> {
        let delimiters = classes.get(&CharacterClass::Seqdelimiter)?;
        if delimiters.is_empty() {
            return None;
        }
        Some(Self {
            delimiters: &delimiters | &classes.get(&CharacterClass::Space).unwrap_or_default(),
            before_chars: classes
                .get(&CharacterClass::Seqbeforechars)
                .unwrap_or_default(),
            after_chars: classes
                .get(&CharacterClass::Seqafterchars)
                .unwrap_or_default(),
            after_patterns,
        })
    }

    /// Whether the text before a match lets it stand alone.
    pub fn stands_alone_before(&self, preceding: &str) -> bool {
        preceding
            .chars()
            .rev()
            .find(|c| !self.before_chars.contains(c))
            .is_none_or(|c| self.delimiters.contains(&c))
    }

    /// Whether the text after a match lets it stand alone.
    pub fn stands_alone_after(&self, following: &str) -> bool {
        let mut rest = following;
        loop {
            if let Some(pattern) = self
                .after_patterns
                .iter()
                .find(|p| rest.starts_with(p.as_str()))
            {
                rest = &rest[pattern.len()..];
            }
            match rest.chars().next() {
                None => return true,
                Some(c) if self.after_chars.contains(&c) => rest = &rest[c.len_utf8()..],
                Some(c) => return self.delimiters.contains(&c),
            }
        }
    }
}

/// The forward boundary transitions of a rule that must stand alone, a `contraction` say:
/// the sequence check with a scanner, and a space or punctuation on either side without
/// one, as in liblouis. Also hands the scanner to the `trie` that will check them.
pub fn stand_alone(
    scanner: Option<StandingAloneScanner>,
    trie: Trie,
) -> (Trie, Transition, Transition) {
    match scanner {
        Some(scanner) => (
            trie.with_standing_alone_scanner(scanner),
            Transition::StandsAloneBefore,
            Transition::StandsAloneAfter,
        ),
        None => {
            let boundary = vec![CharacterClass::Space, CharacterClass::Punctuation];
            (
                trie,
                Transition::Start(boundary.clone()),
                Transition::End(boundary),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scanner() -> StandingAloneScanner {
        let classes = CharacterClasses::new(&[
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Seqdelimiter, &['—']),
            (CharacterClass::Seqbeforechars, &['(', '"']),
            (CharacterClass::Seqafterchars, &[')', '.', '"']),
        ]);
        StandingAloneScanner::new(&classes, vec!["'d".to_string()]).unwrap()
    }

    #[test]
    fn before() {
        let s = scanner();
        assert!(s.stands_alone_before(""));
        assert!(s.stands_alone_before("a "));
        assert!(s.stands_alone_before("a (\""));
        assert!(s.stands_alone_before("a—"));
        assert!(!s.stands_alone_before("a!"));
        assert!(!s.stands_alone_before("a!("));
    }

    #[test]
    fn after() {
        let s = scanner();
        assert!(s.stands_alone_after(""));
        assert!(s.stands_alone_after(" b"));
        assert!(s.stands_alone_after(".\") b"));
        assert!(s.stands_alone_after("—b"));
        assert!(s.stands_alone_after("'d"));
        assert!(s.stands_alone_after("'d."));
        assert!(!s.stands_alone_after("("));
        assert!(!s.stands_alone_after("'s"));
    }

    #[test]
    fn no_seqdelimiter_no_scanner() {
        let classes = CharacterClasses::new(&[(CharacterClass::Space, &[' '])]);
        assert!(StandingAloneScanner::new(&classes, vec![]).is_none());
    }
}
