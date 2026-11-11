//! Lettersign braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins. It
//! fires for any isolated letter (not followed by another letter)
//! whose preceding character is neither a space nor a letter — for example, a letter at string
//! start before punctuation (`b:`), or a Tamil consonant followed by a vowel sign or virama.
//! Letters preceded by a space (word boundary) or by another letter (tail of a letter run) do not fire.
//! The `noletsignafter` and `noletsignbefore` opcodes list characters adjacent to a letter that
//! suppress the indicator. The `noletsign` opcode lists letters that must never themselves be
//! preceded by a letter sign, regardless of context, and so does a letter with a `word` or
//! `largesign` rule of its own.
//!
//! The letter sign in front of a `contraction` depends on the rule translation picks, not on
//! the text, so it is added after translation ([`ContractionSigns`](super::contraction::ContractionSigns)).

use std::collections::HashSet;

use crate::parser::{AnchoredRule, CharacterClass};
use crate::translator::{ResolvedTranslation, TranslationStage, table::TableContext};

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder {
    lettersign: Option<(String, AnchoredRule)>,
    noletsign_chars: HashSet<char>,
    noletsignafter_chars: HashSet<char>,
    noletsignbefore_chars: HashSet<char>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            lettersign: None,
            noletsign_chars: HashSet::new(),
            noletsignafter_chars: HashSet::new(),
            noletsignbefore_chars: HashSet::new(),
        }
    }

    /// Build an [`Indicator`]
    pub fn build(self, ctx: &TableContext) -> Option<Indicator> {
        let (lettersign, origin) = self.lettersign?;
        let start_translation =
            ResolvedTranslation::new("", &lettersign, 1, TranslationStage::Main, origin);

        let cc = ctx.character_classes();

        // Collect letter and numeric character sets from the character class context.
        let letter_chars: HashSet<char> = [
            CharacterClass::Letter,
            CharacterClass::Uppercase,
            CharacterClass::Lowercase,
        ]
        .iter()
        .filter_map(|class| cc.get(class))
        .flatten()
        .collect();

        let space_chars: HashSet<char> = cc
            .get(&CharacterClass::Space)
            .into_iter()
            .flatten()
            .collect();

        Some(Indicator {
            start_translation,
            letter_chars,
            space_chars,
            noletsign_chars: self.noletsign_chars,
            noletsignafter_chars: self.noletsignafter_chars,
            noletsignbefore_chars: self.noletsignbefore_chars,
        })
    }

    pub fn letsign(&mut self, s: &str, origin: &AnchoredRule) {
        self.lettersign = Some((s.to_string(), origin.clone()));
    }

    pub fn noletsign(&mut self, s: &str) {
        self.noletsign_chars.extend(s.chars());
    }

    /// A letter with a `word` or `largesign` rule of its own needs no letter sign: like
    /// liblouis, which adds it to the `noletsign` characters after compiling the table.
    pub fn single_letter_word(&mut self, chars: &str) {
        if chars.chars().count() == 1 {
            self.noletsign(chars);
        }
    }

    pub fn noletsignafter(&mut self, s: &str) {
        self.noletsignafter_chars.extend(s.chars());
    }

    pub fn noletsignbefore(&mut self, s: &str) {
        self.noletsignbefore_chars.extend(s.chars());
    }
}

#[derive(Debug, Clone)]
pub struct Indicator {
    start_translation: ResolvedTranslation,
    /// Characters that are considered letters (Letter + Uppercase + Lowercase classes).
    letter_chars: HashSet<char>,
    /// Characters that are considered spaces (Space class).
    space_chars: HashSet<char>,
    /// These letters must never themselves be preceded by a letter sign.
    noletsign_chars: HashSet<char>,
    /// When one of these characters follows a single letter the letsign must NOT be inserted.
    noletsignafter_chars: HashSet<char>,
    /// When one of these characters precedes a single letter the letsign must NOT be inserted.
    noletsignbefore_chars: HashSet<char>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();

        let mut fire: Vec<usize> = Vec::new();

        // ── Isolation-based letsign ───────────────────────────────────────────
        // Fires for a letter that is isolated (not followed by another letter)
        // AND whose preceding character, if any, is neither a space nor a letter.
        // This covers:
        //   - letters at string start before punctuation (e.g. `b:`, prev=None)
        //   - Tamil consonants followed by vowel signs or virama (prev=digit/punct)
        //   - letters that immediately follow a digit or other non-letter, non-space char
        // It does NOT fire when preceded by a space (word boundary: "a", "I'm")
        // or by another letter (tail of a word: `:bc` — 'c' has prev='b').
        for (i, &c) in chars.iter().enumerate() {
            if !self.letter_chars.contains(&c) || self.noletsign_chars.contains(&c) {
                continue;
            }
            let prev = if i > 0 { Some(chars[i - 1]) } else { None };
            let next = if i + 1 < n { Some(chars[i + 1]) } else { None };

            // Skip if preceded by a noletsignbefore character.
            if prev.is_some_and(|p| self.noletsignbefore_chars.contains(&p)) {
                continue;
            }
            // Skip if followed by a noletsignafter character.
            if next.is_some_and(|n| self.noletsignafter_chars.contains(&n)) {
                continue;
            }

            let isolated = next.is_none_or(|n| !self.letter_chars.contains(&n));
            let prev_is_space_or_letter = prev
                .is_some_and(|p| self.space_chars.contains(&p) || self.letter_chars.contains(&p));
            if isolated && !prev_is_space_or_letter {
                fire.push(i);
            }
        }

        fire.into_iter()
            .map(|pos| (pos, self.start_translation.clone()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{CharacterClasses, RuleParser};

    fn rule(rule: &str) -> AnchoredRule {
        let rule = RuleParser::new(rule).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    fn pairs(t: &[(usize, ResolvedTranslation)]) -> Vec<(usize, String)> {
        t.iter()
            .map(|(pos, r)| (*pos, r.output().to_string()))
            .collect()
    }

    fn build_indicator(letter_chars: &[char], noletsignbefore: &[char]) -> Indicator {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        let noletsignbefore_str: String = noletsignbefore.iter().collect();
        builder.noletsignbefore(&noletsignbefore_str);
        let cc = CharacterClasses::new(&[
            (CharacterClass::Letter, letter_chars),
            (CharacterClass::Space, &[' ']),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        builder.build(&ctx).unwrap()
    }

    #[test]
    fn isolated_letter_fires_when_not_preceded_by_space() {
        let indicator = build_indicator(&['a', 'b', 'c'], &[]);
        // isolated letter at string start (prev=None): fires
        assert_eq!(
            pairs(&indicator.precompute("b:")),
            vec![(0, "⠠".to_string())]
        );
        // isolated letter after punctuation (prev is non-space, non-letter): fires
        assert_eq!(
            pairs(&indicator.precompute("2b")),
            vec![(1, "⠠".to_string())]
        );
        // isolated letter preceded by space: does NOT fire
        assert_eq!(pairs(&indicator.precompute("x b")), vec![]);
        assert_eq!(pairs(&indicator.precompute(" b ")), vec![]);
        // letter followed by another letter (not isolated): does NOT fire
        assert_eq!(pairs(&indicator.precompute(":bc")), vec![]);
        // multi-letter word: no letsign
        assert_eq!(pairs(&indicator.precompute("abc")), vec![]);
    }

    #[test]
    fn single_letter_word_is_noletsign() {
        // `word i` makes "i" a noletsign letter, `word ab` changes nothing
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        builder.single_letter_word("i");
        builder.single_letter_word("ab");
        let cc = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'b', 'i']),
            (CharacterClass::Space, &[' ']),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        let indicator = builder.build(&ctx).unwrap();

        assert_eq!(pairs(&indicator.precompute("2i")), vec![]);
        assert_eq!(
            pairs(&indicator.precompute("2a")),
            vec![(1, "⠠".to_string())]
        );
    }

    #[test]
    fn noletsign_suppresses_isolation_letsign() {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        builder.noletsign("i");
        let cc = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'b', 'i']),
            (CharacterClass::Space, &[' ']),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        let indicator = builder.build(&ctx).unwrap();

        // 'i' is listed in noletsign: never gets a letsign, even isolated after a
        // non-space, non-letter character.
        assert_eq!(pairs(&indicator.precompute("2i")), vec![]);
        // other isolated letters are unaffected.
        assert_eq!(
            pairs(&indicator.precompute("2b")),
            vec![(1, "⠠".to_string())]
        );
    }

    #[test]
    fn noletsignbefore_suppresses_isolation_letsign() {
        let indicator = build_indicator(&['a', 'b', 'c'], &['2']);
        // noletsignbefore '2' suppresses the letsign for "2b" -- '2' precedes the letter
        assert_eq!(pairs(&indicator.precompute("2b")), vec![]);
        // other non-space, non-letter chars still trigger it
        assert_eq!(
            pairs(&indicator.precompute("3b")),
            vec![(1, "⠠".to_string())]
        );
    }

    #[test]
    fn noletsignafter_suppresses_isolation_letsign() {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        builder.noletsignafter("2");
        let cc = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'b', 'c']),
            (CharacterClass::Space, &[' ']),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        let indicator = builder.build(&ctx).unwrap();

        // noletsignafter '2' suppresses the letsign for "3b2" -- '2' follows the letter
        assert_eq!(pairs(&indicator.precompute("3b2")), vec![]);
        // a different following char leaves it in place
        assert_eq!(
            pairs(&indicator.precompute("3b3")),
            vec![(1, "⠠".to_string())]
        );
    }
}
