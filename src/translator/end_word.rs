//! Does the word end after a match? Back-translation answers this from the braille that follows.
//!
//! Back-translation does not ask whether the next cell is a letter. It reads the braille
//! that follows and stops at the first cell that decides: a space or a `hyphen` rule ends
//! the word, a letter or a rule producing more than one character continues it, and any
//! other cell is skipped. Only rules whose braille is a single cell count, so whatever each
//! cell can tell you is fixed by the table and worked out once, at compile time.

use std::collections::HashSet;

use crate::parser::{AnchoredRule, Braille, BrailleChars, Direction, HasDirection, Rule};

/// The cells that end a word and the cells that continue one. A cell in neither decides
/// nothing, and the scan reads on.
#[derive(Default, Debug, Clone)]
pub struct EndWordScanner {
    ends_word: HashSet<char>,
    continues_word: HashSet<char>,
}

impl EndWordScanner {
    pub fn compile(rules: &[AnchoredRule]) -> Self {
        let mut space = HashSet::new();
        let mut letter = HashSet::new();
        let mut hyphen = HashSet::new();
        let mut multi_char = HashSet::new();
        let mut postpunc = HashSet::new();
        for rule in rules
            .iter()
            .filter(|rule| rule.rule.is_direction(Direction::Backward))
        {
            let Some((cell, text_len)) = single_cell_rule(&rule.rule) else {
                continue;
            };
            // The letter and space sets come from the character definitions rather than the
            // dots classes, which are widened with contraction cells; a `begword` cell read
            // as a letter would cut the scan short (liblouis#360).
            match &rule.rule {
                Rule::Letter { .. } | Rule::Lowercase { .. } | Rule::Uppercase { .. } => {
                    letter.insert(cell);
                }
                Rule::Space { .. } => {
                    space.insert(cell);
                }
                Rule::Hyphen { .. } => {
                    hyphen.insert(cell);
                }
                // #360: whether these apply is not yet decided, so they decide nothing
                Rule::Begword { .. } | Rule::Midword { .. } => (),
                Rule::Postpunc { .. } => {
                    postpunc.insert(cell);
                }
                _ if text_len > 1 => {
                    multi_char.insert(cell);
                }
                _ => (),
            }
        }
        // a cell claimed by several sets goes to the first of: space, letter, hyphen, then a
        // multi-character rule that no `postpunc` rule on the same cell cancels
        let multi_char = &(&multi_char - &postpunc) - &hyphen;
        let ends_word = &space | &(&hyphen - &letter);
        let continues_word = &(&letter | &multi_char) - &space;
        EndWordScanner {
            ends_word,
            continues_word,
        }
    }

    /// Does the word end before `following`, the braille left unconsumed by the match?
    /// Braille that runs out without deciding ends the word.
    pub fn is_end_word(&self, following: &str) -> bool {
        for cell in following.chars() {
            if self.ends_word.contains(&cell) {
                return true;
            }
            if self.continues_word.contains(&cell) {
                return false;
            }
        }
        true
    }
}

/// If the rule's braille is a single cell, that cell and how many characters the rule
/// translates it to; `None` otherwise.
fn single_cell_rule(rule: &Rule) -> Option<(char, usize)> {
    fn one_cell(dots: &BrailleChars, text_len: usize) -> Option<(char, usize)> {
        if dots.len() != 1 {
            return None;
        }
        Some((dots.to_string().chars().next()?, text_len))
    }
    match rule {
        Rule::Always { chars, dots, .. }
        | Rule::Word { chars, dots, .. }
        | Rule::Syllable { chars, dots, .. }
        | Rule::Sufword { chars, dots, .. }
        | Rule::Prfword { chars, dots, .. }
        | Rule::Begword { chars, dots, .. }
        | Rule::Begmidword { chars, dots, .. }
        | Rule::Midword { chars, dots, .. }
        | Rule::Midendword { chars, dots, .. }
        | Rule::Endword { chars, dots, .. }
        | Rule::Partword { chars, dots, .. }
        | Rule::Endnum { chars, dots, .. } => match dots {
            Braille::Explicit(dots) => one_cell(dots, chars.chars().count()),
            // `=` gives a single cell only for a single character, which never decides
            Braille::Implicit => None,
        },
        Rule::Lowword { chars, dots, .. }
        | Rule::Joinword { chars, dots, .. }
        | Rule::Largesign { chars, dots, .. }
        | Rule::Repeated { chars, dots, .. }
        | Rule::Prepunc { chars, dots, .. }
        | Rule::Postpunc { chars, dots, .. }
        | Rule::Begnum { chars, dots, .. }
        | Rule::Midnum { chars, dots, .. } => one_cell(dots, chars.chars().count()),
        Rule::Hyphen { dots, .. }
        | Rule::Space { dots, .. }
        | Rule::Letter { dots, .. }
        | Rule::Lowercase { dots, .. }
        | Rule::Uppercase { dots, .. } => one_cell(dots, 1),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;

    #[test]
    fn is_end_word_scans_until_something_decides() {
        let rules: Vec<AnchoredRule> = [
            "space \\s 0",
            "letter a 1",
            "always ing 346",
            "always ed 1246",
            "postpunc ! 1246",
            "hyphen - 36",
            "midword aa 2",
        ]
        .iter()
        .map(|source| RuleParser::new(source).rule().unwrap().into())
        .collect();
        let scan = EndWordScanner::compile(&rules);

        // nothing left to read, so the word is over
        assert!(scan.is_end_word(""));
        // a space ends it, a letter continues it, whichever comes first
        assert!(scan.is_end_word("⠀⠁"));
        assert!(!scan.is_end_word("⠁⠀"));
        // a multi-character rule continues it, unless a `postpunc` rule shares the cell
        assert!(!scan.is_end_word("⠬"));
        assert!(scan.is_end_word("⠫"));
        // a hyphen ends it outright
        assert!(scan.is_end_word("⠤⠁"));
        // a `midword` cell decides nothing and is skipped
        assert!(!scan.is_end_word("⠂⠁"));
        assert!(scan.is_end_word("⠂⠀"));
    }
}
