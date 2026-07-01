//! Uppercase Braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins
//! and emits the appropriate capitalisation indicator at each position. liblouis defines
//! four escalating tiers for this (`capsletter`/`begcapsword`/`begcaps`/`begcapsphrase` are
//! aliases of the generic `modeletter`/`begmodeword`/`begmode`/`begmodephrase` family,
//! specialised for the "uppercase" mode):
//!
//! - [`capsletter`](IndicatorBuilder::capsletter) — a single isolated uppercase character.
//! - [`begcapsword`]/[`endcapsword`](IndicatorBuilder::begcapsword) — one word that is
//!   entirely uppercase. The mode is automatically terminated by the first non-letter
//!   character, unless the word ends mid-word (immediately followed by a lowercase
//!   letter), in which case `endcapsword` is needed. Characters listed via
//!   [`capsmodechars`](IndicatorBuilder::capsmodechars) (e.g. a hyphen) are transparent:
//!   they neither break the word nor count as letters themselves.
//! - [`begcaps`]/[`endcaps`](IndicatorBuilder::begcaps) and
//!   [`begcapsphrase`]/[`endcapsphrase`](IndicatorBuilder::begcapsphrase) — general,
//!   passage-level forms used to wrap two or more consecutive whole-uppercase *words*
//!   (real, whitespace-separated words — hyphenated segments within one such word don't
//!   count) in a single span instead of marking each word individually. `begcapsphrase` is
//!   preferred when defined; `begcaps` is the fallback. The threshold is
//!   [`lencapsphrase`](IndicatorBuilder::lencapsphrase) consecutive words (default 2, i.e.
//!   any passage of more than one word). Below the threshold, or when neither opcode is
//!   defined, each word is marked independently at the word tier instead. Unlike
//!   `endcapsword`, the passage closer always closes explicitly, regardless of what follows.
//!
//! When a table doesn't define the opcode a given run would prefer, the run falls back to
//! the next tier down; the ultimate fallback (also used mid-word when `begcapsword` is
//! defined but `endcapsword` is not) is to mark every uppercase letter individually with
//! `capsletter`.

use crate::{
    parser::{AnchoredRule, CharacterClasses, Position},
    translator::{ResolvedTranslation, TranslationStage, table::TableContext},
};

use std::collections::HashSet;

fn make_translation(dots: &str, origin: &AnchoredRule) -> ResolvedTranslation {
    ResolvedTranslation::new("", dots, 1, TranslationStage::Main, origin.clone())
}

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder(Indicator);

impl IndicatorBuilder {
    pub fn new() -> Self {
        IndicatorBuilder(Indicator {
            uppercase_chars: HashSet::default(),
            mode_chars: HashSet::default(),
            letter_chars: HashSet::default(),
            character_classes: CharacterClasses::default(),
            capsletter: None,
            begcapsword: None,
            endcapsword: None,
            begcaps: None,
            endcaps: None,
            begcapsphrase: None,
            endcapsphrase: None,
            endcapsphrase_before: true,
            len_phrase: 2,
        })
    }

    pub fn build(mut self, ctx: &TableContext) -> Option<Indicator> {
        self.0.character_classes = ctx.character_classes().clone();
        if self.0.is_indicating() {
            Some(self.0)
        } else {
            None
        }
    }

    pub fn capsletter(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.capsletter = Some(make_translation(s, origin));
    }

    pub fn begcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.begcapsword = Some(make_translation(s, origin));
    }

    pub fn endcapsword(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.endcapsword = Some(make_translation(s, origin));
    }

    pub fn begcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.begcaps = Some(make_translation(s, origin));
    }

    pub fn endcaps(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.endcaps = Some(make_translation(s, origin));
    }

    pub fn begcapsphrase(&mut self, s: &str, origin: &AnchoredRule) {
        self.0.begcapsphrase = Some(make_translation(s, origin));
    }

    pub fn endcapsphrase(&mut self, s: &str, position: &Position, origin: &AnchoredRule) {
        self.0.endcapsphrase = Some(make_translation(s, origin));
        self.0.endcapsphrase_before = matches!(position, Position::Before);
    }

    pub fn lencapsphrase(&mut self, len: usize) {
        self.0.len_phrase = len;
    }

    pub fn capsmodechars(&mut self, s: &str) {
        self.0.mode_chars = s.chars().collect();
    }

    pub fn uppercase_characters(&mut self, chars: HashSet<char>) {
        self.0.uppercase_chars = chars;
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.0.letter_chars = chars;
    }
}

/// A maximal run of uppercase letters within a word, ignoring [`capsmodechars`] gaps.
///
/// `open`/`close` are absolute character positions (`close` is one past the last letter
/// of the run). `needs_close` is set when the run doesn't reach the end of its containing
/// word — i.e. another letter (necessarily lowercase) follows within the same word — and
/// `close_trigger` is the position of that following letter, which is where a word-tier
/// `endcapsword` must be inserted (`close` itself sits before any trailing mode chars).
struct Run {
    open: usize,
    close: usize,
    letter_count: usize,
    needs_close: bool,
    close_trigger: usize,
}

/// One letter-word (a maximal span of letters and capsmodechars).
enum WordKind {
    /// Every letter in the word is uppercase. `open`/`close` bracket the first/last
    /// letter (skipping any leading/trailing mode chars), `letter_count` is the number
    /// of letters in the word.
    WholeUpper {
        open: usize,
        close: usize,
        letter_count: usize,
    },
    /// The word mixes upper- and lowercase letters (or has no letters at all).
    Partial,
}

struct Word {
    start: usize,
    end: usize,
    kind: WordKind,
}

#[derive(Debug, Clone)]
pub struct Indicator {
    uppercase_chars: HashSet<char>,
    mode_chars: HashSet<char>,
    letter_chars: HashSet<char>,
    character_classes: CharacterClasses,
    capsletter: Option<ResolvedTranslation>,
    begcapsword: Option<ResolvedTranslation>,
    endcapsword: Option<ResolvedTranslation>,
    begcaps: Option<ResolvedTranslation>,
    endcaps: Option<ResolvedTranslation>,
    begcapsphrase: Option<ResolvedTranslation>,
    endcapsphrase: Option<ResolvedTranslation>,
    endcapsphrase_before: bool,
    len_phrase: usize,
}

impl Indicator {
    fn is_indicating(&self) -> bool {
        self.capsletter.is_some()
            || self.begcapsword.is_some()
            || self.begcaps.is_some()
            || self.begcapsphrase.is_some()
    }

    fn is_letter(&self, c: char) -> bool {
        self.letter_chars.contains(&c)
    }

    fn is_upper(&self, c: char) -> bool {
        self.uppercase_chars.contains(&c)
    }

    fn is_word_char(&self, c: char) -> bool {
        self.is_letter(c) || self.mode_chars.contains(&c)
    }

    fn is_space(&self, c: char) -> bool {
        self.character_classes.is_whitespace(c)
    }

    /// Splits the input into maximal spans of letters/capsmodechars ("words").
    fn find_words(&self, chars: &[char]) -> Vec<(usize, usize)> {
        super::find_spans(chars, 0, chars.len(), |c| self.is_word_char(c))
    }

    fn classify_word(&self, chars: &[char], start: usize, end: usize) -> Word {
        let mut has_upper = false;
        let mut has_lower = false;
        let mut first_letter = None;
        let mut last_letter = None;
        let mut letter_count = 0;
        for (i, &c) in chars.iter().enumerate().take(end).skip(start) {
            if self.is_letter(c) {
                letter_count += 1;
                first_letter.get_or_insert(i);
                last_letter = Some(i);
                if self.is_upper(c) {
                    has_upper = true;
                } else {
                    has_lower = true;
                }
            }
        }
        let kind = if has_upper && !has_lower {
            WordKind::WholeUpper {
                open: first_letter.unwrap(),
                close: last_letter.unwrap() + 1,
                letter_count,
            }
        } else {
            WordKind::Partial
        };
        Word { start, end, kind }
    }

    /// Finds maximal uppercase-letter runs within `[start, end)`, treating capsmodechars
    /// as transparent (they neither extend nor break a run).
    fn upper_runs(&self, chars: &[char], start: usize, end: usize) -> Vec<Run> {
        let mut runs = Vec::new();
        let mut i = start;
        while i < end {
            if self.is_letter(chars[i]) && self.is_upper(chars[i]) {
                let open = i;
                let mut close = i + 1;
                let mut letter_count = 1;
                let mut j = i + 1;
                loop {
                    if j >= end {
                        break;
                    }
                    if self.is_letter(chars[j]) {
                        if self.is_upper(chars[j]) {
                            close = j + 1;
                            letter_count += 1;
                            j += 1;
                        } else {
                            break;
                        }
                    } else {
                        j += 1; // capsmodechar: transparent
                    }
                }
                // Does another letter (necessarily lowercase) follow within the word?
                let mut needs_close = false;
                let mut close_trigger = close;
                let mut k = close;
                while k < end {
                    if self.is_letter(chars[k]) {
                        needs_close = true;
                        close_trigger = k;
                        break;
                    }
                    k += 1;
                }
                runs.push(Run {
                    open,
                    close,
                    letter_count,
                    needs_close,
                    close_trigger,
                });
                i = j;
            } else {
                i += 1;
            }
        }
        runs
    }

    fn emit_letterwise(
        &self,
        chars: &[char],
        start: usize,
        end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if let Some(t) = &self.capsletter {
            for (i, &c) in chars.iter().enumerate().take(end).skip(start) {
                if self.is_letter(c) && self.is_upper(c) {
                    result.push((i, t.clone()));
                }
            }
        }
    }

    /// Emits indicators for one run of uppercase letters inside a partial (mixed-case)
    /// word — i.e. a run that cannot participate in a whole-word caps passage.
    fn emit_run(&self, chars: &[char], run: &Run, result: &mut Vec<(usize, ResolvedTranslation)>) {
        if run.letter_count == 1
            && let Some(t) = &self.capsletter
        {
            result.push((run.open, t.clone()));
            return;
        }
        if let Some(begt) = &self.begcapsword {
            if !run.needs_close {
                result.push((run.open, begt.clone()));
                return;
            } else if let Some(endt) = &self.endcapsword {
                result.push((run.open, begt.clone()));
                result.push((run.close_trigger, endt.clone()));
                return;
            }
            // begcapsword defined but can't be closed (no endcapsword, ends mid-word):
            // fall through to the mode tier, then to per-letter capsletter.
        }
        if let Some(begt) = &self.begcaps {
            result.push((run.open, begt.clone()));
            if let Some(endt) = &self.endcaps {
                result.push((run.close, endt.clone()));
            }
            return;
        }
        self.emit_letterwise(chars, run.open, run.close, result);
    }

    fn emit_partial_word(
        &self,
        chars: &[char],
        start: usize,
        end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        for run in self.upper_runs(chars, start, end) {
            self.emit_run(chars, &run, result);
        }
    }

    /// Emits indicators for a single whole-uppercase word (used both for a standalone
    /// word and, when a passage-level tier isn't available or reached, for each word of
    /// a group).
    fn emit_whole_word(
        &self,
        chars: &[char],
        open: usize,
        close: usize,
        letter_count: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if letter_count == 1
            && let Some(t) = &self.capsletter
        {
            result.push((open, t.clone()));
            return;
        }
        if let Some(t) = &self.begcapsword {
            // A whole word always ends at a proper word boundary, so no endcapsword
            // is ever needed here (unlike a run inside a mixed-case word).
            result.push((open, t.clone()));
            return;
        }
        if let Some(begt) = &self.begcaps {
            result.push((open, begt.clone()));
            if let Some(endt) = &self.endcaps {
                result.push((close, endt.clone()));
            }
            return;
        }
        self.emit_letterwise(chars, open, close, result);
    }

    /// Emits indicators for a maximal group of consecutive whole-uppercase words.
    /// `linguistic_words` is the number of real (whitespace-separated) words the group
    /// spans — hyphenated segments of the same word don't add to this count.
    fn emit_group(
        &self,
        chars: &[char],
        group: &[(usize, usize, usize)], // (open, close, letter_count) per word
        linguistic_words: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if linguistic_words >= self.len_phrase {
            if let Some(begphrase) = &self.begcapsphrase {
                let start_pos = group[0].0;
                let &(last_open, last_close, last_count) = group.last().unwrap();
                result.push((start_pos, begphrase.clone()));
                match &self.endcapsphrase {
                    Some(_) if self.endcapsphrase_before => {
                        // The last word's own indicator serves as the implicit terminator.
                        self.emit_whole_word(chars, last_open, last_close, last_count, result);
                    }
                    Some(endt) => {
                        result.push((last_close, endt.clone()));
                    }
                    None => {
                        // No dedicated endcapsphrase: fall back to the shared mode
                        // closer (real tables, e.g. my-g1.utb, rely on this).
                        if let Some(endt) = &self.endcaps {
                            result.push((last_close, endt.clone()));
                        }
                    }
                }
                return;
            }
            if let Some(begt) = &self.begcaps {
                let start_pos = group[0].0;
                let close_pos = group.last().unwrap().1;
                result.push((start_pos, begt.clone()));
                if let Some(endt) = &self.endcaps {
                    result.push((close_pos, endt.clone()));
                }
                return;
            }
        }
        // Below the passage threshold (or no passage-level tier available): mark each
        // word independently at the word tier.
        for &(open, close, letter_count) in group {
            self.emit_whole_word(chars, open, close, letter_count, result);
        }
    }

    /// Returns sparse `(position, translation)` pairs.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut result = Vec::new();

        if !self.is_indicating() {
            return result;
        }

        let words: Vec<Word> = self
            .find_words(&chars)
            .into_iter()
            .map(|(s, e)| self.classify_word(&chars, s, e))
            .collect();

        let mut group: Vec<(usize, usize, usize)> = Vec::new();
        let mut linguistic_words = 0usize;
        let mut prev_close: Option<usize> = None;

        for word in &words {
            match word.kind {
                WordKind::WholeUpper {
                    open,
                    close,
                    letter_count,
                } => {
                    let starts_new_linguistic_word = match prev_close {
                        Some(prev) => chars[prev..open].iter().any(|&c| self.is_space(c)),
                        None => true,
                    };
                    if starts_new_linguistic_word {
                        linguistic_words += 1;
                    }
                    group.push((open, close, letter_count));
                    prev_close = Some(close);
                }
                WordKind::Partial => {
                    if !group.is_empty() {
                        self.emit_group(&chars, &group, linguistic_words, &mut result);
                        group.clear();
                        linguistic_words = 0;
                    }
                    prev_close = None;
                    self.emit_partial_word(&chars, word.start, word.end, &mut result);
                }
            }
        }
        if !group.is_empty() {
            self.emit_group(&chars, &group, linguistic_words, &mut result);
        }

        result.sort_by_key(|(pos, _)| *pos);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{CharacterClass, RuleParser};

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
