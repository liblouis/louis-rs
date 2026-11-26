//! The spaces between large signs
//!
//! `en-us-g2.ctb` defines "and", "for", "of", "the", "with" and "a" as `largesign`s, and
//! liblouis writes two of them that only spaces separate as one: "of the" is ⠷⠮ and "and
//! for the" ⠯⠿⠮. Back-translation puts the spaces back. Whether a space goes or comes
//! depends on the rules translation picked, not on the text, so [`LargeSigns`] is a
//! rewriter (see [`indication`](super)), not an [`Indicator`].
//!
//! [`Indicator`]: super::Indicator

use std::collections::HashSet;

use crate::parser::{CharacterClass, CharacterClasses, Rule};
use crate::translator::ResolvedTranslation;

/// Drops the spaces between two large signs (forward), as liblouis does, and puts them back
/// (backward)
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
    /// `replace`, `repeated`, `repword` and `rependword` rules, so these don't separate
    /// two large signs. It drops only spaces at the end of the braille so far, so an indication after
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
                        // the spaces at the end of the braille so far, past what wrote none
                        for t in translations[..i]
                            .iter_mut()
                            .rev()
                            .take_while(|t| self.is_space(t) || t.output().is_empty())
                        {
                            if self.is_space(t) {
                                *t = t.clone().with_output("");
                            }
                        }
                    }
                    after_largesign = true;
                }
                Some(
                    Rule::Context { .. }
                    | Rule::Replace { .. }
                    | Rule::Repeated { .. }
                    | Rule::Repword { .. }
                    | Rule::Rependword { .. },
                ) => {}
                _ => after_largesign = false,
            }
        }
        translations
    }

    /// Puts a space back between the large signs of a run written together, when the run
    /// stands as words of its own: after a space or punctuation and not before a letter,
    /// the mirror of [`stands_alone`](Self::stands_alone). So ⠷⠮ is "of the", but the
    /// large signs of ⠮⠁⠞⠻ stay "theater".
    ///
    /// An indication such as a capital sign writes nothing, so it doesn't separate two large
    /// signs: ⠷⠠⠮ is "of The". This runs after the capitals are read, as the braille was
    /// written. A space goes with the large sign before it, as forward.
    pub fn apply_backward(
        &self,
        mut translations: Vec<ResolvedTranslation>,
    ) -> Vec<ResolvedTranslation> {
        let written: Vec<usize> = (0..translations.len())
            .filter(|&i| !translations[i].output().is_empty())
            .collect();
        let is_largesign =
            |i: usize| matches!(translations[i].rule(), Some(Rule::Largesign { .. }));
        let mut runs = Vec::new();
        let mut start = 0;
        for run in written.chunk_by(|&a, &b| is_largesign(a) && is_largesign(b)) {
            let end = start + run.len();
            let before = start.checked_sub(1).map(|i| written[i]);
            let after = written.get(end).copied();
            let stands_alone = before
                .and_then(|i| translations[i].output().chars().last())
                .is_none_or(|c| self.space.contains(&c) || self.punctuation.contains(&c))
                && !after
                    .and_then(|i| translations[i].output().chars().next())
                    .is_some_and(|c| self.letter.contains(&c));
            if run.len() > 1 && stands_alone {
                runs.push(&run[..run.len() - 1]);
            }
            start = end;
        }
        for &i in runs.into_iter().flatten() {
            let spaced = format!("{} ", translations[i].output());
            translations[i] = translations[i].clone().with_output(&spaced);
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
                         lowercase d 145\nlowercase e 15\nlowercase f 124\nlowercase g 1245\nlowercase h 125\n\
                         lowercase n 1345\nlowercase o 135\nlowercase r 1235\nlowercase t 2345\n\
                         lowercase x 1346\nuppercase T 2345\ncapsletter 6\nbase uppercase T t\nbase uppercase O o\n\
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
    fn a_replace_with_nothing_between_large_signs_drops_both_spaces() {
        let table = "space \\s 0\nlowercase f 124\nlowercase o 135\nlowercase x 1346\n\
                     lowercase e 15\nlowercase h 125\nlowercase t 2345\nreplace x\n\
                     largesign of 12356\nlargesign the 2346\n";
        let translator = Translator::from_table_source(table, Direction::Forward).unwrap();
        assert_eq!(translator.translate("of x the"), "⠷⠮");
    }

    fn back(input: &str) -> String {
        Translator::from_table_source(TABLE, Direction::Backward)
            .unwrap()
            .translate(input)
    }

    #[test]
    fn large_signs_written_together_get_their_space_back() {
        // the cases of liblouis/liblouis#2168
        assert_eq!(back("⠷⠮"), "of the");
        assert_eq!(back("⠷⠁"), "of a");
        assert_eq!(back("⠷⠯⠮"), "of and the");
        assert_eq!(back("⠷⠮⠀⠙⠕⠛"), "of the dog");
        assert_eq!(back("⠙⠕⠛⠀⠷⠮⠲"), "dog of the.");
        // a capital sign writes nothing, so it doesn't stand between them
        assert_eq!(back("⠠⠷⠮"), "Of the");
        assert_eq!(back("⠷⠠⠮"), "of The");
        // a space in the braille isn't doubled
        assert_eq!(back("⠷⠀⠮"), "of the");
    }

    #[test]
    fn large_signs_in_a_word_keep_no_space() {
        // only large signs written together get a space between them
        assert_eq!(back("⠷⠙⠕⠛"), "ofdog");
        assert_eq!(back("⠙⠕⠛⠮"), "dogthe");
        // a run of large signs in a word isn't words of its own
        assert_eq!(back("⠮⠁⠞"), "theat");
        assert_eq!(back("⠞⠮⠁"), "tthea");
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
