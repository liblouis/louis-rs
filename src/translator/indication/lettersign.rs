//! Lettersign braille indication
//!
//! Sometimes a sequence of letters happens to be the same as a braille contraction. In that case an
//! indication is needed to clarify the meaning. The lettersign indicator signals that the following
//! braille cells are not a contraction.
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins. At each
//! position it checks whether the remaining input matches a known contraction and, if so, emits the
//! lettersign translation at that position.
//!
//! In addition, even without explicit contraction entries, the indicator fires for any isolated
//! single letter (a letter not adjacent to other letters) or for any letter immediately following a
//! digit.  These are the contexts where a standalone letter cell could be misread as a braille
//! symbol.  The `noletsignafter` opcode lists characters after which the indicator must not fire.

use std::collections::HashSet;

use crate::parser::CharacterClass;
use crate::translator::TranslationStage;
use crate::translator::table::TableContext;
use crate::translator::trie::{Boundary, Transition};
use crate::{
    parser::{AnchoredRule, Direction, Precedence},
    translator::{ResolvedTranslation, trie::Trie},
};

use log::warn;

use std::collections::HashMap;

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder {
    lettersign: Option<(String, AnchoredRule)>,
    contractions: HashMap<String, AnchoredRule>,
    noletsignafter_chars: HashSet<char>,
    noletsignbefore_chars: HashSet<char>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            lettersign: None,
            contractions: HashMap::new(),
            noletsignafter_chars: HashSet::new(),
            noletsignbefore_chars: HashSet::new(),
        }
    }

    /// Build an [`Indicator`]
    pub fn build(self, ctx: &TableContext) -> Option<Indicator> {
        if self.lettersign.is_none() {
            if !self.contractions.is_empty() {
                warn!("Table contains contractions but no letsign");
            }
            return None;
        }

        let (lettersign, origin) = self.lettersign.unwrap();
        let start_translation = ResolvedTranslation::new(
            "",
            &lettersign,
            1,
            TranslationStage::Main,
            origin.clone(),
        );

        let cc = ctx.character_classes();
        let mut trie = Trie::new().with_context(cc.clone());
        for (contraction, _origin) in self.contractions {
            trie.insert(
                &contraction,
                &lettersign,
                Some(Transition::Start(Boundary::Word)),
                Some(Transition::End(Boundary::Word)),
                Direction::Forward,
                Precedence::Default,
                vec![],
                TranslationStage::Main,
                &origin,
            );
        }

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

        let numeric_chars: HashSet<char> =
            cc.get(&CharacterClass::Digit).into_iter().flatten().collect();

        Some(Indicator {
            contractions: trie,
            start_translation,
            letter_chars,
            numeric_chars,
            noletsignafter_chars: self.noletsignafter_chars,
            noletsignbefore_chars: self.noletsignbefore_chars,
        })
    }

    pub fn letsign(&mut self, s: &str, origin: &AnchoredRule) {
        self.lettersign = Some((s.to_string(), origin.clone()));
    }

    pub fn contraction(&mut self, s: &str, origin: &AnchoredRule) {
        self.contractions.insert(s.to_string(), origin.clone());
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
    contractions: Trie,
    start_translation: ResolvedTranslation,
    /// Characters that are considered letters (Letter + Uppercase + Lowercase classes).
    letter_chars: HashSet<char>,
    /// Characters that are considered digits (Digit class).
    numeric_chars: HashSet<char>,
    /// After these characters the letsign must NOT be inserted.
    noletsignafter_chars: HashSet<char>,
    /// Before these characters the letsign must NOT be inserted.
    noletsignbefore_chars: HashSet<char>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();

        // Collect positions where letsign should fire (deduplicated via HashSet).
        let mut fire: HashSet<usize> = HashSet::new();

        // ── Isolation-based letsign ────────────────────────────────────────────
        // Fires for any letter that:
        //   • is not preceded by another letter, AND
        //   • is not preceded by a noletsignafter character, AND
        //   • is either isolated (not followed by another letter)
        //     OR immediately follows a digit.
        for (i, &c) in chars.iter().enumerate() {
            if !self.letter_chars.contains(&c) {
                continue;
            }
            let prev = if i > 0 { Some(chars[i - 1]) } else { None };
            let next = if i + 1 < n { Some(chars[i + 1]) } else { None };

            // Skip if the preceding character is already a letter (part of a word).
            if prev.is_some_and(|p| self.letter_chars.contains(&p)) {
                continue;
            }
            // Skip if preceded by a noletsignafter character.
            if prev.is_some_and(|p| self.noletsignafter_chars.contains(&p)) {
                continue;
            }
            // Skip if followed by a noletsignbefore character.
            if next.is_some_and(|n| self.noletsignbefore_chars.contains(&n)) {
                continue;
            }

            let isolated = next.map_or(true, |n| !self.letter_chars.contains(&n));
            let after_digit = prev.is_some_and(|p| self.numeric_chars.contains(&p));

            if isolated || after_digit {
                fire.insert(i);
            }
        }

        // ── Contraction-based letsign ──────────────────────────────────────────
        // Fires when a known contraction is found at a word boundary (grade 2 tables).
        {
            let mut prev: Option<char> = None;
            let mut char_pos = 0;
            for (byte_pos, c) in input.char_indices() {
                if self.next(&input[byte_pos..], prev).is_some() {
                    fire.insert(char_pos);
                }
                prev = Some(c);
                char_pos += 1;
            }
        }

        let mut result: Vec<(usize, ResolvedTranslation)> = fire
            .into_iter()
            .map(|pos| (pos, self.start_translation.clone()))
            .collect();
        result.sort_by_key(|(pos, _)| *pos);
        result
    }

    pub fn next(&self, s: &str, prev: Option<char>) -> Option<ResolvedTranslation> {
        let translations = self.contractions.find_translations(s, prev);
        if !translations.is_empty() {
            let translation = translations.first().unwrap();
            Some(translation.clone().with_input("").with_weight(1))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::{CharacterClass, CharacterClasses, RuleParser},
        translator::{TranslationStage, table::TableContext},
    };

    fn rule(rule: &str) -> AnchoredRule {
        let rule = RuleParser::new(rule).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    fn pairs(t: &[(usize, ResolvedTranslation)]) -> Vec<(usize, String)> {
        t.iter()
            .map(|(pos, r)| (*pos, r.output().to_string()))
            .collect()
    }

    fn build_indicator(
        letter_chars: &[char],
        noletsignafter: &[char],
        contraction: Option<&str>,
    ) -> Indicator {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        let noletsignafter_str: String = noletsignafter.iter().collect();
        builder.noletsignafter(&noletsignafter_str);
        if let Some(c) = contraction {
            builder.contraction(c, &rule(&format!("contraction {}", c)));
        }
        let cc = CharacterClasses::new(&[
            (CharacterClass::Letter, letter_chars),
            (CharacterClass::Digit, &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
        ]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        builder.build(&ctx).unwrap()
    }

    #[test]
    fn contraction_indicator() {
        let indicator = build_indicator(&['a', 'b', 'c', 'd'], &[], Some("ab"));
        assert_eq!(indicator.next("aa".into(), None), None);
        assert_eq!(
            indicator.next("ab".into(), None),
            Some(ResolvedTranslation::new(
                "".into(),
                "⠠".into(),
                1,
                TranslationStage::Main,
                rule("letsign 6")
            ))
        );
    }

    #[test]
    fn isolated_letter_fires_letsign() {
        let indicator = build_indicator(&['a', 'b', 'c'], &[], None);
        // single isolated letter at start
        assert_eq!(pairs(&indicator.precompute("b:")), vec![(0, "⠠".to_string())]);
        // single letter at end
        assert_eq!(pairs(&indicator.precompute("x,b")), vec![(2, "⠠".to_string())]);
        // multi-letter word: no letsign
        assert_eq!(pairs(&indicator.precompute("abc")), vec![]);
    }

    #[test]
    fn letter_after_digit_fires_letsign() {
        let indicator = build_indicator(&['a', 'b', 'c'], &[], None);
        assert_eq!(pairs(&indicator.precompute("2b")), vec![(1, "⠠".to_string())]);
        // letter after digit followed by more letters still fires
        assert_eq!(pairs(&indicator.precompute("2bc")), vec![(1, "⠠".to_string())]);
    }

    #[test]
    fn noletsignafter_suppresses_letsign() {
        let indicator = build_indicator(&['a', 'b', 'c'], &['.'], None);
        // After '.', no letsign
        assert_eq!(pairs(&indicator.precompute(".b")), vec![]);
        // After other chars, still fires
        assert_eq!(pairs(&indicator.precompute(",b")), vec![(1, "⠠".to_string())]);
    }

    #[test]
    fn precompute_contraction() {
        let mut builder = IndicatorBuilder::new();
        builder.letsign("⠠", &rule("letsign 6"));
        builder.contraction("ab", &rule("contraction ab"));
        builder.contraction("cd", &rule("contraction cd"));
        let cc = CharacterClasses::new(&[(CharacterClass::Letter, &['a', 'b', 'c', 'd'])]);
        let ctx = TableContext::new(cc, CharacterClasses::default(), Default::default());
        let indicator = builder.build(&ctx).unwrap();

        assert_eq!(pairs(&indicator.precompute("aa")), vec![]);
        assert_eq!(
            pairs(&indicator.precompute("ab")),
            vec![(0, "⠠".to_string())]
        );
    }
}
