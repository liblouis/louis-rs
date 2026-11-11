//! Uppercase braille indication
//!
//! Many braille codes have fewer available cell patterns than there are letters, so
//! uppercase and lowercase often share the same cell and capitalization is marked
//! separately instead of getting its own dots.
//!
//! [`Indicator`] analyses the full input text in a single pass before translation
//! begins. At each position it checks whether the character starts, continues, or ends
//! a run of uppercase letters and, if so, emits the matching indicator: a single letter
//! gets `capsletter`, a whole word gets `begcapsword` (closed with `endcapsword` only if
//! it ends mid-word), and two or more consecutive uppercase words become one
//! `begcaps`/`begcapsphrase` span once `lencapsphrase` words are reached instead of one
//! marker per word.
//!
//! `capsmodechars` lists characters (e.g. a hyphen) that stay inside a word without
//! ending it. When a table doesn't define the tier a run would prefer, it falls back to
//! the next tier down, ultimately to marking each letter individually with `capsletter`.

use crate::{
    parser::{AnchoredRule, CharacterClass, Position, Rule},
    translator::{ResolvedTranslation, table::TableContext},
};

use std::collections::HashSet;

use super::class_indicator::ClassIndicator;

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder(ClassIndicator);

impl IndicatorBuilder {
    pub fn new() -> Self {
        IndicatorBuilder(ClassIndicator::new())
    }

    pub fn build(mut self, ctx: &TableContext) -> Option<Indicator> {
        self.0
            .set_character_classes(ctx.character_classes().clone());
        if self.0.is_indicating() {
            Some(Indicator(self.0))
        } else {
            None
        }
    }

    pub fn capsletter(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.modeletter(s, origin);
    }

    pub fn begcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.begmodeword(s, origin);
    }

    pub fn endcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.endmodeword(s, origin);
    }

    pub fn begcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.begmode(s, origin);
    }

    pub fn endcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.endmode(s, origin);
    }

    pub fn begcapsphrase(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.begmodephrase(s, origin);
    }

    pub fn endcapsphrase(&mut self, s: &str, position: &Position, origin: &AnchoredRule) {
        self.0.endmodephrase(s, position, origin);
    }

    pub fn lencapsphrase(&mut self, len: usize) {
        self.0.lenmodephrase(len);
    }

    pub fn capsmodechars(&mut self, s: &str) {
        self.0.mode_characters(s.chars().collect());
    }

    pub fn uppercase_characters(&mut self, chars: HashSet<char>) {
        self.0.active_characters(chars);
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.0.letter_characters(chars);
    }
}

#[derive(Debug, Clone)]
pub struct Indicator(ClassIndicator);

impl Indicator {
    /// Returns sparse `(position, translation)` pairs.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        self.0.precompute(&chars)
    }
}

/// Backward, the capitalized word a `begcapsword` starts, decided from the text written.
///
/// As in liblouis, the characters written after the indicator are uppercased, whatever
/// rule wrote them, until one that is neither a letter nor a `capsmodechars` character,
/// which ends the word, possibly partway through a rule's output (`'s`). `endcapsword`,
/// `capsletter` and a space end it too. Until the first uppercase letter is written, a
/// character that isn't a letter, as the hyphen of UEB's ⠠⠠⠤⠏⠁⠝⠽ "-PANY", doesn't end it.
///
/// Capitalizing changes no rule's choice, so it runs over the translations once they are
/// chosen, rather than guessing the word's extent from the cells beforehand.
#[derive(Debug, Clone)]
pub struct BackwardCapsWord {
    /// The letters and `capsmodechars` characters, which go on a capitalized word.
    word_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

#[derive(Clone, Copy, PartialEq)]
enum CapsWord {
    Off,
    /// After the indicator, before any uppercase letter is written.
    Started,
    Running,
}

/// A builder for [`BackwardCapsWord`]
#[derive(Debug, Default)]
pub struct BackwardCapsWordBuilder {
    capsmodechars: HashSet<char>,
}

impl BackwardCapsWordBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn capsmodechars(&mut self, s: &str) {
        self.capsmodechars.extend(s.chars());
    }

    pub fn build(self, ctx: &TableContext) -> BackwardCapsWord {
        let classes = ctx.character_classes();
        let letters = classes.get(&CharacterClass::Letter).unwrap_or_default();
        BackwardCapsWord {
            word_chars: letters.into_iter().chain(self.capsmodechars).collect(),
            space_chars: classes.get(&CharacterClass::Space).unwrap_or_default(),
        }
    }
}

impl BackwardCapsWord {
    /// Uppercases the output of `translations` inside capitalized words.
    pub fn apply(&self, translations: Vec<ResolvedTranslation>) -> Vec<ResolvedTranslation> {
        let mut state = CapsWord::Off;
        let mut last: Option<char> = None;
        let mut result = Vec::with_capacity(translations.len());
        for translation in translations {
            // liblouis checks the last character written before each rule
            state = match (state, last) {
                (CapsWord::Started, Some(c)) if c.is_uppercase() => CapsWord::Running,
                (CapsWord::Running, Some(c))
                    if !c.is_uppercase() && !self.word_chars.contains(&c) =>
                {
                    CapsWord::Off
                }
                (state, _) => state,
            };
            state = match translation.rule() {
                Some(Rule::Begcapsword { .. }) => CapsWord::Started,
                Some(Rule::Endcapsword { .. } | Rule::Capsletter { .. }) => CapsWord::Off,
                Some(Rule::Multind { names, .. }) if names.iter().any(|n| n == "begcapsword") => {
                    CapsWord::Started
                }
                Some(Rule::Multind { names, .. })
                    if names
                        .iter()
                        .any(|n| n == "endcapsword" || n == "capsletter") =>
                {
                    CapsWord::Off
                }
                _ => state,
            };
            let translation = if state == CapsWord::Off {
                translation
            } else {
                let mut output = String::new();
                for c in translation.output().chars() {
                    if self.space_chars.contains(&c)
                        || state == CapsWord::Running && !self.word_chars.contains(&c)
                    {
                        state = CapsWord::Off;
                    }
                    if state == CapsWord::Off {
                        output.push(c);
                    } else {
                        output.extend(c.to_uppercase());
                    }
                }
                translation.with_output(&output)
            };
            last = translation.output().chars().last().or(last);
            result.push(translation);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{CharacterClass, CharacterClasses, RuleParser};

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn pairs(t: &[(usize, ResolvedTranslation)]) -> Vec<(usize, String)> {
        t.iter()
            .map(|(pos, r)| (*pos, r.output().to_string()))
            .collect()
    }

    fn no_space_ctx() -> TableContext {
        TableContext::default()
    }

    fn space_ctx() -> TableContext {
        let cc = CharacterClasses::new(&[(CharacterClass::Space, &[' '])]);
        TableContext::new(cc, CharacterClasses::default(), Default::default())
    }

    fn base_builder() -> IndicatorBuilder {
        let mut builder = IndicatorBuilder::new();
        builder.uppercase_characters(HashSet::from(['A', 'B', 'C', 'D', 'E', 'F']));
        builder.letter_characters(HashSet::from([
            'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f',
        ]));
        builder
    }

    #[test]
    fn precompute_indicator() {
        let mut builder = base_builder();
        builder.capsletter("⠇", &rule("capsletter 123"));
        let indicator = builder.build(&no_space_ctx()).unwrap();

        assert_eq!(
            pairs(&indicator.precompute("Abc ")),
            vec![(0, "⠇".to_string())]
        );
    }

    #[test]
    fn precompute_end_indication() {
        let mut builder = base_builder();
        builder.begcapsword("⠇", &rule("begcapsword 123"));
        builder.endcapsword("⠠", &rule("endcapsword 6"));
        let indicator = builder.build(&no_space_ctx()).unwrap();

        assert_eq!(
            pairs(&indicator.precompute("ABCa")),
            vec![(0, "⠇".to_string()), (3, "⠠".to_string()),]
        );
    }

    #[test]
    fn begcaps_fallback_when_no_word_tier() {
        // Mirrors liblouis emphasis.yaml's begcaps/endcaps/capsletter test.
        let mut builder = base_builder();
        builder.capsletter("⠇", &rule("capsletter 123"));
        builder.begcaps("⠠⠠", &rule("begcaps 6-6"));
        builder.endcaps("⠄", &rule("endcaps 3"));
        let indicator = builder.build(&no_space_ctx()).unwrap();

        // "ABc": partial word, run "AB" ends mid-word before lowercase 'c' -> begcaps/endcaps,
        // closing unconditionally right before 'c'.
        assert_eq!(
            pairs(&indicator.precompute("ABc")),
            vec![(0, "⠠⠠".to_string()), (2, "⠄".to_string())]
        );

        // "aBC ": partial word, run "BC" ends at the space -> begcaps/endcaps still closes
        // explicitly (unlike endcapsword, which would stay silent here).
        assert_eq!(
            pairs(&indicator.precompute("aBC ")),
            vec![(1, "⠠⠠".to_string()), (3, "⠄".to_string())]
        );

        // "ABC" alone: a whole word, but begcapsword isn't defined, so it falls back to
        // the mode tier too.
        assert_eq!(
            pairs(&indicator.precompute("ABC")),
            vec![(0, "⠠⠠".to_string()), (3, "⠄".to_string())]
        );
    }

    #[test]
    fn capsletter_repeats_when_no_word_or_mode_tier() {
        let mut builder = base_builder();
        builder.capsletter("⠇", &rule("capsletter 123"));
        let indicator = builder.build(&no_space_ctx()).unwrap();

        assert_eq!(
            pairs(&indicator.precompute("ABC")),
            vec![
                (0, "⠇".to_string()),
                (1, "⠇".to_string()),
                (2, "⠇".to_string())
            ]
        );
    }

    #[test]
    fn whole_word_passage_uses_begcaps_over_begcapsword() {
        let mut builder = base_builder();
        builder.begcapsword("⠠", &rule("begcapsword 6"));
        builder.begcaps("⠠⠠", &rule("begcaps 6-6"));
        builder.endcaps("⠄", &rule("endcaps 3"));
        let indicator = builder.build(&space_ctx()).unwrap();

        // Two consecutive whole-uppercase words -> one begcaps/endcaps span, not two
        // begcapsword markers.
        assert_eq!(
            pairs(&indicator.precompute("ABC DEF")),
            vec![(0, "⠠⠠".to_string()), (7, "⠄".to_string())]
        );

        // A single whole-uppercase word still prefers the word tier.
        assert_eq!(
            pairs(&indicator.precompute("ABC")),
            vec![(0, "⠠".to_string())]
        );
    }

    #[test]
    fn hyphenated_segments_dont_count_as_separate_words() {
        // Mirrors liblouis capitalization.yaml: "-" is a plain sign (not a capsmodechar),
        // so "ABC-DEF" is two word-tier segments but only one *linguistic* word — it must
        // not reach the (default) 2-word passage threshold on its own.
        let mut builder = base_builder();
        builder.begcapsword("⠠", &rule("begcapsword 6"));
        builder.begcaps("⠠⠠", &rule("begcaps 6-6"));
        builder.endcaps("⠄", &rule("endcaps 3"));
        let indicator = builder.build(&space_ctx()).unwrap();

        assert_eq!(
            pairs(&indicator.precompute("ABC-DEF")),
            vec![(0, "⠠".to_string()), (4, "⠠".to_string())]
        );
    }

    #[test]
    fn phrase_tier_used_once_length_threshold_reached() {
        let mut builder = base_builder();
        builder.begcapsword("⠠", &rule("begcapsword 6"));
        builder.begcaps("⠠⠠", &rule("begcaps 6-6"));
        builder.endcaps("⠄", &rule("endcaps 3"));
        builder.begcapsphrase("⠠⠠⠠", &rule("begcapsphrase 6-6-6"));
        builder.lencapsphrase(3);
        let indicator = builder.build(&space_ctx()).unwrap();

        // Two words: below lencapsphrase, which also gates begcaps here (mirrors
        // liblouis capitalization.yaml, where lencapsphrase gates begcaps even though
        // begcapsphrase is what actually reaches the threshold below) -> per-word.
        assert_eq!(
            pairs(&indicator.precompute("AB CD")),
            vec![(0, "⠠".to_string()), (3, "⠠".to_string())]
        );

        // Three words: reaches lencapsphrase, uses begcapsphrase; with no endcapsphrase
        // defined, falls back to the shared endcaps closer.
        assert_eq!(
            pairs(&indicator.precompute("AB CD EF")),
            vec![(0, "⠠⠠⠠".to_string()), (8, "⠄".to_string())]
        );
    }
}
