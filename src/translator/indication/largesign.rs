//! The spaces between large signs
//!
//! `en-us-g2.ctb` defines "and", "for", "of", "the", "with" and "a" as `largesign`s, and
//! liblouis writes two of them that only spaces separate as one: "of the" is ⠷⠮ and "and
//! for the" ⠯⠿⠮. Whether a space goes depends on the rules translation picked, not on the
//! text, so [`LargeSigns`] is a rewriter (see [`indication`](super)), not an [`Indicator`].
//!
//! [`Indicator`]: super::Indicator

use std::collections::HashSet;

use crate::parser::{CharacterClass, CharacterClasses, Rule};
use crate::translator::ResolvedTranslation;

/// Drops the spaces between two large signs (forward), as liblouis does
#[derive(Debug, Clone)]
pub struct LargeSigns {
    space: HashSet<char>,
    punctuation: HashSet<char>,
    letter: HashSet<char>,
}

impl LargeSigns {
    pub fn new(classes: &CharacterClasses) -> Self {
        let class = |class| classes.get(&class).unwrap_or_default();
        LargeSigns {
            space: class(CharacterClass::Space),
            punctuation: class(CharacterClass::Punctuation),
            letter: class(CharacterClass::Letter),
        }
    }

    /// Empties the space translations right before a large sign that follows another, as
    /// liblouis does when it translates a large sign after one (`prevTransOpcode`).
    ///
    /// liblouis's previous rule skips spaces and indications, and the `context`,
    /// `repeated`, `repword` and `rependword` rules, so these don't separate two large
    /// signs. It drops only spaces at the end of the braille so far, so an indication after
    /// the spaces, such as the capital sign of "of The", keeps them. The spaces stay as
    /// translations, without output, so that position maps still find their input.
    pub fn apply(&self, mut translations: Vec<ResolvedTranslation>) -> Vec<ResolvedTranslation> {
        let input: Vec<char> = translations
            .iter()
            .flat_map(|t| t.input().chars().collect::<Vec<_>>())
            .collect();
        let mut pos = 0;
        let mut after_largesign = false;
        for i in 0..translations.len() {
            let t = &translations[i];
            let start = pos;
            pos += t.input().chars().count();
            if start == pos || self.is_space(t) {
                continue;
            }
            match t.rule() {
                Some(Rule::Largesign { .. })
                    if self.stands_alone(&input, start, pos, after_largesign) =>
                {
                    if after_largesign {
                        for space in translations[..i]
                            .iter_mut()
                            .rev()
                            .take_while(|t| self.is_space(t))
                        {
                            *space = space.clone().with_output("");
                        }
                    }
                    after_largesign = true;
                }
                Some(
                    Rule::Context { .. }
                    | Rule::Repeated { .. }
                    | Rule::Repword { .. }
                    | Rule::Rependword { .. },
                ) => {}
                _ => after_largesign = false,
            }
        }
        translations
    }

    /// Whether `t` translates spaces only. An indication translates nothing, so it isn't.
    fn is_space(&self, t: &ResolvedTranslation) -> bool {
        let input = t.input();
        !input.is_empty() && input.chars().all(|c| self.space.contains(&c))
    }

    /// Whether the large sign at `start..end` of `input` is one, as liblouis decides:
    /// after a space or punctuation, not before a letter, and before a space unless it
    /// follows another large sign. The start and end of the input count as spaces.
    /// Otherwise liblouis takes it for an `always` rule.
    fn stands_alone(
        &self,
        input: &[char],
        start: usize,
        end: usize,
        after_largesign: bool,
    ) -> bool {
        let before = start.checked_sub(1).map(|i| input[i]);
        let after = input.get(end).copied();
        before.is_none_or(|c| self.space.contains(&c) || self.punctuation.contains(&c))
            && (after_largesign || after.is_none_or(|c| self.space.contains(&c)))
            && !after.is_some_and(|c| self.letter.contains(&c))
    }
}

#[cfg(test)]
mod tests {
    use crate::{Direction, Translator};

    const TABLE: &str = "space \\s 0\npunctuation , 2\npunctuation . 256\nlowercase a 1\n\
                         lowercase d 145\nlowercase e 15\nlowercase f 124\nlowercase h 125\n\
                         lowercase n 1345\nlowercase o 135\nlowercase r 1235\nlowercase t 2345\n\
                         lowercase x 1346\nuppercase T 2345\ncapsletter 6\nbase uppercase T t\n\
                         largesign and 12346\nlargesign for 123456\nlargesign of 12356\n\
                         largesign the 2346\nlargesign a 1\n";

    fn translate(input: &str) -> String {
        Translator::from_table_source(TABLE, Direction::Forward)
            .unwrap()
            .translate(input)
    }

    #[test]
    fn large_signs_that_only_spaces_separate_are_written_together() {
        assert_eq!(translate("of the"), "⠷⠮");
        assert_eq!(translate("and for the"), "⠯⠿⠮");
        assert_eq!(translate("of a"), "⠷⠁");
        assert_eq!(translate("of  the"), "⠷⠮");
        // after a large sign, one before punctuation still is one
        assert_eq!(translate("of the."), "⠷⠮⠲");
    }

    #[test]
    fn anything_else_between_large_signs_keeps_the_space() {
        assert_eq!(translate("of, the"), "⠷⠂⠀⠮");
        assert_eq!(translate("of x the"), "⠷⠀⠭⠀⠮");
        // the capital sign comes after the space
        assert_eq!(translate("of The"), "⠷⠀⠠⠮");
        // a large sign in a word isn't one
        assert_eq!(translate("the then"), "⠮⠀⠮⠝");
        assert_eq!(translate("the"), "⠮");
    }

    #[test]
    fn a_dropped_space_maps_to_the_large_sign_before_it() {
        let result = Translator::from_table_source(TABLE, Direction::Forward)
            .unwrap()
            .translate_with_options("of the", &Default::default());
        assert_eq!(result.output(), "⠷⠮");
        // as in liblouis: the space goes with the large sign before it
        assert_eq!(result.output_positions(), [0, 0, 0, 1, 1, 1]);
        assert_eq!(result.input_positions(), [0, 3]);
    }
}
