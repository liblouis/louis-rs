//! Shared tier-selection engine behind [`mode`](super::mode) and
//! [`uppercase`](super::uppercase): one algorithm, [`ClassIndicator`], letter / word /
//! passage / threshold-passage, parametrized by two character sets per mode:
//!
//! - [`active_characters`](ClassIndicator::active_characters) — which characters the mode
//!   is "on" for (uppercase letters, digits, or a custom class).
//! - [`letter_characters`](ClassIndicator::letter_characters) — what bounds a mode word,
//!   i.e. what a run can extend across without an explicit `endmode`-family close.
//!   Uppercase is bounded by `letter` (plus [`mode_characters`](ClassIndicator::mode_characters),
//!   e.g. a hyphen, as a transparent gap); `digit` is bounded by itself, so its
//!   `endmode`/`endmodeword` structurally can never fire; any other custom class is
//!   bounded by `letter`, like uppercase without the transparency carve-out.
//!
//! [`mode::IndicatorBuilder::build`](super::mode::IndicatorBuilder::build) picks the
//! boundary by attribute name; `uppercase` sets its classes directly.

use std::collections::HashSet;

use crate::{
    parser::{AnchoredRule, CharacterClasses, Position},
    translator::{ResolvedTranslation, TranslationStage},
};

fn make_translation(dots: &str, origin: &AnchoredRule) -> ResolvedTranslation {
    ResolvedTranslation::new("", dots, 1, TranslationStage::Main, origin.clone())
}

/// A maximal run of active characters within a word, ignoring [`mode_characters`](ClassIndicator::mode_characters)
/// gaps.
///
/// `open`/`close` are absolute character positions (`close` is one past the last active
/// character of the run). `needs_close` is set when the run doesn't reach the end of its
/// containing word — i.e. another word character (necessarily inactive) follows within the
/// same word — and `close_trigger` is the position of that following character, which is
/// where a word-tier `endmode`-family close must be inserted (`close` itself sits before any
/// trailing mode chars).
struct Run {
    open: usize,
    close: usize,
    active_count: usize,
    needs_close: bool,
    close_trigger: usize,
}

/// One word (a maximal span of letters — or, for a self-bounded class, active characters —
/// and mode chars).
enum WordKind {
    /// Every word character is active. `open`/`close` bracket the first/last active
    /// character (skipping any leading/trailing mode chars), `active_count` is the number
    /// of active characters in the word.
    WholeActive {
        open: usize,
        close: usize,
        active_count: usize,
    },
    /// The word mixes active and inactive characters (or has no active characters at all).
    Partial,
}

struct Word {
    start: usize,
    end: usize,
    kind: WordKind,
}

/// Compiled indicator data for one mode class (e.g. "uppercase", "digit", or a custom
/// attribute). See the module doc for what `active_chars`/`letter_chars`/`mode_chars` mean.
#[derive(Debug, Clone)]
pub(super) struct ClassIndicator {
    active_chars: HashSet<char>,
    letter_chars: HashSet<char>,
    mode_chars: HashSet<char>,
    character_classes: CharacterClasses,
    modeletter: Option<ResolvedTranslation>,
    begmodeword: Option<ResolvedTranslation>,
    endmodeword: Option<ResolvedTranslation>,
    begmode: Option<ResolvedTranslation>,
    endmode: Option<ResolvedTranslation>,
    begmodephrase: Option<ResolvedTranslation>,
    endmodephrase: Option<ResolvedTranslation>,
    endmodephrase_before: bool,
    len_phrase: usize,
}

impl ClassIndicator {
    pub(super) fn new() -> Self {
        ClassIndicator {
            active_chars: HashSet::default(),
            letter_chars: HashSet::default(),
            mode_chars: HashSet::default(),
            character_classes: CharacterClasses::default(),
            modeletter: None,
            begmodeword: None,
            endmodeword: None,
            begmode: None,
            endmode: None,
            begmodephrase: None,
            endmodephrase: None,
            endmodephrase_before: true,
            len_phrase: 2,
        }
    }

    pub(super) fn is_indicating(&self) -> bool {
        self.modeletter.is_some()
            || self.begmodeword.is_some()
            || self.begmode.is_some()
            || self.begmodephrase.is_some()
    }

    pub(super) fn modeletter(&mut self, dots: &str, origin: &AnchoredRule) {
        self.modeletter = Some(make_translation(dots, origin));
    }

    pub(super) fn begmodeword(&mut self, dots: &str, origin: &AnchoredRule) {
        self.begmodeword = Some(make_translation(dots, origin));
    }

    pub(super) fn endmodeword(&mut self, dots: &str, origin: &AnchoredRule) {
        self.endmodeword = Some(make_translation(dots, origin));
    }

    pub(super) fn begmode(&mut self, dots: &str, origin: &AnchoredRule) {
        self.begmode = Some(make_translation(dots, origin));
    }

    pub(super) fn endmode(&mut self, dots: &str, origin: &AnchoredRule) {
        self.endmode = Some(make_translation(dots, origin));
    }

    pub(super) fn begmodephrase(&mut self, dots: &str, origin: &AnchoredRule) {
        self.begmodephrase = Some(make_translation(dots, origin));
    }

    pub(super) fn endmodephrase(&mut self, dots: &str, position: &Position, origin: &AnchoredRule) {
        self.endmodephrase = Some(make_translation(dots, origin));
        self.endmodephrase_before = matches!(position, Position::Before);
    }

    pub(super) fn lenmodephrase(&mut self, len: usize) {
        self.len_phrase = len;
    }

    /// The mode's own active-character class (`uppercase_chars` for the built-in uppercase
    /// mode, or the named attribute's class for a generic mode).
    pub(super) fn active_characters(&mut self, chars: HashSet<char>) {
        self.active_chars = chars;
    }

    /// The class that bounds a mode word — see the module doc's "Word boundary" axis.
    pub(super) fn letter_characters(&mut self, chars: HashSet<char>) {
        self.letter_chars = chars;
    }

    /// Characters transparent to word-boundary detection (`capsmodechars` for uppercase; no
    /// generic opcode sets this for any other mode).
    pub(super) fn mode_characters(&mut self, chars: HashSet<char>) {
        self.mode_chars = chars;
    }

    pub(super) fn set_character_classes(&mut self, classes: CharacterClasses) {
        self.character_classes = classes;
    }

    fn is_letter(&self, c: char) -> bool {
        self.letter_chars.contains(&c)
    }

    fn is_active(&self, c: char) -> bool {
        self.active_chars.contains(&c)
    }

    fn is_word_char(&self, c: char) -> bool {
        self.is_letter(c) || self.mode_chars.contains(&c)
    }

    fn is_space(&self, c: char) -> bool {
        self.character_classes.is_whitespace(c)
    }

    /// Splits the input into maximal spans of letters/mode chars ("words").
    fn find_words(&self, chars: &[char]) -> Vec<(usize, usize)> {
        super::find_spans(chars, 0, chars.len(), |c| self.is_word_char(c))
    }

    fn classify_word(&self, chars: &[char], start: usize, end: usize) -> Word {
        let mut has_active = false;
        let mut has_inactive = false;
        let mut first_letter = None;
        let mut last_letter = None;
        let mut active_count = 0;
        for (i, &c) in chars.iter().enumerate().take(end).skip(start) {
            if self.is_letter(c) {
                active_count += usize::from(self.is_active(c));
                first_letter.get_or_insert(i);
                last_letter = Some(i);
                if self.is_active(c) {
                    has_active = true;
                } else {
                    has_inactive = true;
                }
            }
        }
        let kind = if has_active && !has_inactive {
            WordKind::WholeActive {
                open: first_letter.unwrap(),
                close: last_letter.unwrap() + 1,
                active_count,
            }
        } else {
            WordKind::Partial
        };
        Word { start, end, kind }
    }

    /// Finds maximal active-character runs within `[start, end)`, treating mode chars as
    /// transparent (they neither extend nor break a run).
    fn active_runs(&self, chars: &[char], start: usize, end: usize) -> Vec<Run> {
        let mut runs = Vec::new();
        let mut i = start;
        while i < end {
            if self.is_letter(chars[i]) && self.is_active(chars[i]) {
                let open = i;
                let mut close = i + 1;
                let mut active_count = 1;
                let mut j = i + 1;
                loop {
                    if j >= end {
                        break;
                    }
                    if self.is_letter(chars[j]) {
                        if self.is_active(chars[j]) {
                            close = j + 1;
                            active_count += 1;
                            j += 1;
                        } else {
                            break;
                        }
                    } else {
                        j += 1; // mode char: transparent
                    }
                }
                // Does another letter (necessarily inactive) follow within the word?
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
                    active_count,
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

    fn emit_charwise(
        &self,
        chars: &[char],
        start: usize,
        end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if let Some(t) = &self.modeletter {
            for (i, &c) in chars.iter().enumerate().take(end).skip(start) {
                if self.is_letter(c) && self.is_active(c) {
                    result.push((i, t.clone()));
                }
            }
        }
    }

    /// Emits indicators for one run of active characters inside a partial (mixed) word —
    /// i.e. a run that cannot participate in a whole-word passage.
    fn emit_run(&self, chars: &[char], run: &Run, result: &mut Vec<(usize, ResolvedTranslation)>) {
        if run.active_count == 1
            && let Some(t) = &self.modeletter
        {
            result.push((run.open, t.clone()));
            return;
        }
        if let Some(begt) = &self.begmodeword {
            if !run.needs_close {
                result.push((run.open, begt.clone()));
                return;
            } else if let Some(endt) = &self.endmodeword {
                result.push((run.open, begt.clone()));
                result.push((run.close_trigger, endt.clone()));
                return;
            }
            // begmodeword defined but can't be closed (no endmodeword, ends mid-word):
            // fall through to the general tier, then to per-letter modeletter.
        }
        if let Some(begt) = &self.begmode {
            result.push((run.open, begt.clone()));
            if let Some(endt) = &self.endmode {
                result.push((run.close, endt.clone()));
            }
            return;
        }
        self.emit_charwise(chars, run.open, run.close, result);
    }

    fn emit_partial_word(
        &self,
        chars: &[char],
        start: usize,
        end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        for run in self.active_runs(chars, start, end) {
            self.emit_run(chars, &run, result);
        }
    }

    /// Emits indicators for a single whole-active word (used both for a standalone word
    /// and, when a passage-level tier isn't available or reached, for each word of a group).
    fn emit_whole_word(
        &self,
        chars: &[char],
        open: usize,
        close: usize,
        active_count: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if active_count == 1
            && let Some(t) = &self.modeletter
        {
            result.push((open, t.clone()));
            return;
        }
        if let Some(t) = &self.begmodeword {
            // A whole word always ends at a proper word boundary, so no endmodeword is
            // ever needed here (unlike a run inside a mixed word).
            result.push((open, t.clone()));
            return;
        }
        if let Some(begt) = &self.begmode {
            result.push((open, begt.clone()));
            if let Some(endt) = &self.endmode {
                result.push((close, endt.clone()));
            }
            return;
        }
        self.emit_charwise(chars, open, close, result);
    }

    /// Emits indicators for a maximal group of consecutive whole-active words.
    /// `linguistic_words` is the number of real (whitespace-separated) words the group
    /// spans — mode-char-joined segments of the same word don't add to this count.
    fn emit_group(
        &self,
        chars: &[char],
        group: &[(usize, usize, usize)], // (open, close, active_count) per word
        linguistic_words: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if linguistic_words >= self.len_phrase {
            if let Some(begphrase) = &self.begmodephrase {
                let start_pos = group[0].0;
                let &(last_open, last_close, last_count) = group.last().unwrap();
                result.push((start_pos, begphrase.clone()));
                match &self.endmodephrase {
                    Some(_) if self.endmodephrase_before => {
                        // The last word's own indicator serves as the implicit terminator.
                        self.emit_whole_word(chars, last_open, last_close, last_count, result);
                    }
                    Some(endt) => {
                        result.push((last_close, endt.clone()));
                    }
                    None => {
                        // No dedicated endmodephrase: fall back to the shared mode closer.
                        if let Some(endt) = &self.endmode {
                            result.push((last_close, endt.clone()));
                        }
                    }
                }
                return;
            }
            if let Some(begt) = &self.begmode {
                let start_pos = group[0].0;
                let close_pos = group.last().unwrap().1;
                result.push((start_pos, begt.clone()));
                if let Some(endt) = &self.endmode {
                    result.push((close_pos, endt.clone()));
                }
                return;
            }
        }
        // Below the passage threshold (or no passage-level tier available): mark each
        // word independently at the word tier.
        for &(open, close, active_count) in group {
            self.emit_whole_word(chars, open, close, active_count, result);
        }
    }

    /// Returns sparse `(position, translation)` pairs.
    pub(super) fn precompute(&self, chars: &[char]) -> Vec<(usize, ResolvedTranslation)> {
        let mut result = Vec::new();

        if !self.is_indicating() {
            return result;
        }

        let words: Vec<Word> = self
            .find_words(chars)
            .into_iter()
            .map(|(s, e)| self.classify_word(chars, s, e))
            .collect();

        let mut group: Vec<(usize, usize, usize)> = Vec::new();
        let mut linguistic_words = 0usize;
        let mut prev_close: Option<usize> = None;

        for word in &words {
            match word.kind {
                WordKind::WholeActive {
                    open,
                    close,
                    active_count,
                } => {
                    let starts_new_linguistic_word = match prev_close {
                        Some(prev) => chars[prev..open].iter().any(|&c| self.is_space(c)),
                        None => true,
                    };
                    if starts_new_linguistic_word {
                        linguistic_words += 1;
                    }
                    group.push((open, close, active_count));
                    prev_close = Some(close);
                }
                WordKind::Partial => {
                    if !group.is_empty() {
                        self.emit_group(chars, &group, linguistic_words, &mut result);
                        group.clear();
                        linguistic_words = 0;
                    }
                    prev_close = None;
                    self.emit_partial_word(chars, word.start, word.end, &mut result);
                }
            }
        }
        if !group.is_empty() {
            self.emit_group(chars, &group, linguistic_words, &mut result);
        }

        result.sort_by_key(|(pos, _)| *pos);
        result
    }
}
