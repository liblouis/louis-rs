//! Lettersign braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins. As
//! in liblouis (`insertLetterSign`), it fires for a letter that doesn't follow another letter
//! and either isn't followed by a letter or follows a digit: a letter standing alone, as in
//! "plan b", a letter before punctuation (`b:`), a letter after a digit (`2b`, `2bc`), or a
//! Tamil consonant followed by a vowel sign or virama. The main stage emits it only where a
//! translation starts, as liblouis checks only there, so a letter inside a longer rule gets
//! none. The `noletsignafter` and `noletsignbefore` opcodes list characters adjacent to a letter that
//! suppress the indicator. The `noletsign` opcode lists letters that must never themselves be
//! preceded by a letter sign, regardless of context, and so does a letter with a `word` or
//! `largesign` rule of its own.
//!
//! The letter sign in front of a `contraction` depends on the rule translation picks, not on
//! the text, so it is added after translation ([`ContractionSigns`](super::contraction::ContractionSigns)).

use std::collections::HashSet;

use crate::parser::{AnchoredRule, CharacterClass, Rule};
use crate::translator::{ResolvedTranslation, table::TableContext};

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
        let start_translation = ResolvedTranslation::indication(&lettersign, &origin);

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

        let digit_chars: HashSet<char> = cc
            .get(&CharacterClass::Digit)
            .into_iter()
            .flatten()
            .collect();

        Some(Indicator {
            start_translation,
            letter_chars,
            digit_chars,
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
    /// Characters that are considered digits (Digit class).
    digit_chars: HashSet<char>,
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

        // A letter that doesn't follow a letter, and either is isolated (not followed by a
        // letter) or follows a digit. The tail of a word (`:bc`, the 'c') doesn't fire.
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
            let after_letter = prev.is_some_and(|p| self.letter_chars.contains(&p));
            let after_digit = prev.is_some_and(|p| self.digit_chars.contains(&p));
            if !after_letter && (isolated || after_digit) {
                fire.push(i);
            }
        }

        fire.into_iter()
            .map(|pos| (pos, self.start_translation.clone()))
            .collect()
    }
}

/// Takes the letter sign back off in front of an `endnum` rule (forward), as liblouis does.
///
/// A letter after a digit gets a letter sign, as in "2b", but liblouis drops it again when
/// the rule that translates the letter is an `endnum`: "1st" is ⠼⠁⠌ in `en-GB-g2.ctb`, not
/// ⠼⠁⠰⠌. liblouis takes off the last cell written, which is the letter sign or, as in
/// `da-dk-g26-lit_1993.ctb`, the same cell written by a `context` rule after it. Which rule
/// translation picks decides it, so this is a rewriter (see [`indication`](super)).
#[derive(Debug, Clone)]
pub struct EndnumLetterSign;

impl EndnumLetterSign {
    pub fn apply(&self, translations: Vec<ResolvedTranslation>) -> Vec<ResolvedTranslation> {
        let mut result: Vec<ResolvedTranslation> = Vec::with_capacity(translations.len());
        for t in translations {
            if matches!(t.rule(), Some(Rule::Endnum { .. })) {
                // the letter sign among the translations of no input right before it, such
                // as a `context` rule that writes a cell there
                let letsign = result
                    .iter()
                    .rev()
                    .take_while(|p| p.input().is_empty())
                    .position(|p| matches!(p.rule(), Some(Rule::Letsign { .. })));
                if let Some(back) = letsign {
                    result.remove(result.len() - 1 - back);
                }
            }
            result.push(t);
        }
        result
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
            (CharacterClass::Digit, &['2', '3']),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        builder.build(&ctx).unwrap()
    }

    #[test]
    fn a_letter_alone_or_after_a_digit_fires() {
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
        // isolated letter preceded by space: fires, as in liblouis ("plan b")
        assert_eq!(
            pairs(&indicator.precompute("a b")),
            vec![(0, "⠠".to_string()), (2, "⠠".to_string())]
        );
        assert_eq!(
            pairs(&indicator.precompute(" b ")),
            vec![(1, "⠠".to_string())]
        );
        // letter followed by another letter (not isolated): does NOT fire
        assert_eq!(pairs(&indicator.precompute(":bc")), vec![]);
        // unless it follows a digit
        assert_eq!(
            pairs(&indicator.precompute("2bc")),
            vec![(1, "⠠".to_string())]
        );
        // multi-letter word: no letsign
        assert_eq!(pairs(&indicator.precompute("abc")), vec![]);
    }

    #[test]
    fn letters_alone_after_the_first_word_and_after_digits_get_one() {
        use crate::{Direction, Translator};
        let table = "space \\s 0\ndigit 1 1\nlowercase b 12\nlowercase s 234\n\
                     lowercase t 2345\nlowercase x 1346\nnumsign 3456\nletsign 56\n\
                     punctuation ' 3\nendnum st 34\nnoletsignafter '\n";
        let translate = |input| {
            Translator::from_table_source(table, Direction::Forward)
                .unwrap()
                .translate(input)
        };
        assert_eq!(translate("x b"), "⠰⠭⠀⠰⠃");
        assert_eq!(translate("1b"), "⠼⠁⠰⠃");
        assert_eq!(translate("1bt"), "⠼⠁⠰⠃⠞");
        // but not in front of an `endnum`, nor before a `noletsignafter` character
        assert_eq!(translate("1st"), "⠼⠁⠌");
        assert_eq!(translate("x b't"), "⠰⠭⠀⠃⠄⠰⠞");
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
