//! Emphasis (italic, bold, underline, …) braille indication.
//!
//! Analyses the [`EmphasisSpan`] list supplied by the caller and emits the
//! appropriate indicator cells (emphletter, begemphword, …) before or after
//! the relevant character positions.
//!
//! The choice of indicator (letter / word / phrase) depends on the full
//! run context, so the entire decision is made during precompute.

use std::collections::HashMap;

use crate::{
    parser::{AnchoredRule, Position},
    emphasis::EmphasisSpan,
    translator::{ResolvedTranslation, TranslationStage},
};

fn make_translation(dots: &str, origin: &AnchoredRule) -> ResolvedTranslation {
    ResolvedTranslation::new("", dots, 1, TranslationStage::Main, origin.clone())
}

/// Build a dense per-position active flag for a single emphasis class name.
fn active_positions(class_name: &str, n: usize, spans: &[EmphasisSpan]) -> Vec<bool> {
    let mut active = vec![false; n];
    for span in spans {
        if span.class == class_name {
            let end = span.range.end.min(n);
            for pos in span.range.start..end {
                active[pos] = true;
            }
        }
    }
    active
}

/// Compiled indicator data for one emphasis class (e.g. "italic").
#[derive(Debug, Clone)]
struct ClassIndicator {
    class_name: String,
    /// Single-letter emphasis indicator (emphletter).
    emphletter: Option<ResolvedTranslation>,
    /// General start indicator (begemph), used for any emphasis boundary.
    begemph: Option<ResolvedTranslation>,
    /// General end indicator (endemph).
    endemph: Option<ResolvedTranslation>,
    /// Start of a word-level emphasis run (begemphword).
    begemphword: Option<ResolvedTranslation>,
    /// End of a word-level emphasis run (endemphword), only emitted mid-word.
    endemphword: Option<ResolvedTranslation>,
    /// Start of a phrase-level emphasis run (begemphphrase).
    begemphphrase: Option<ResolvedTranslation>,
    /// End of a phrase-level emphasis run (endemphphrase).
    endemphphrase: Option<ResolvedTranslation>,
    /// Whether endemphphrase is inserted before (true) or after (false) the trigger.
    endemphphrase_before: bool,
    /// Minimum words in a run to qualify as a phrase.
    len_phrase: usize,
}

impl ClassIndicator {
    fn new(class_name: String) -> Self {
        ClassIndicator {
            class_name,
            emphletter: None,
            begemph: None,
            endemph: None,
            begemphword: None,
            endemphword: None,
            begemphphrase: None,
            endemphphrase: None,
            endemphphrase_before: true,
            len_phrase: 4,
        }
    }

    fn is_indicating(&self) -> bool {
        self.emphletter.is_some()
            || self.begemphword.is_some()
            || self.begemph.is_some()
            || self.begemphphrase.is_some()
    }

    fn emit(
        &self,
        chars: &[char],
        active: &[bool],
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        let n = chars.len();
        let mut pos = 0;
        while pos < n {
            if !active.get(pos).copied().unwrap_or(false) {
                pos += 1;
                continue;
            }
            let run_start = pos;
            while pos < n && active.get(pos).copied().unwrap_or(false) {
                pos += 1;
            }
            let run_end = pos;
            self.emit_run(chars, run_start, run_end, result);
        }
    }

    fn emit_run(
        &self,
        chars: &[char],
        run_start: usize,
        run_end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        // begemph/endemph: general indicators that can start/end anywhere.
        if let Some(begemph) = &self.begemph {
            let effective_start = (run_start..run_end)
                .find(|&i| !chars[i].is_whitespace())
                .unwrap_or(run_start);
            result.push((effective_start, begemph.clone()));

            if let Some(endemph) = &self.endemph {
                // Find the last non-space position in the run. endemph goes
                // at the next slot (= first trailing-space, or n if none).
                let effective_end = (run_start..run_end)
                    .rev()
                    .find(|&i| !chars[i].is_whitespace())
                    .map(|i| i + 1)
                    .unwrap_or(run_end);
                result.push((effective_end, endemph.clone()));
            }
            return;
        }

        // begemphword / emphletter / begemphphrase path.
        let words = find_word_segments(chars, run_start, run_end);
        if words.is_empty() {
            return;
        }

        // Strip leading/trailing whitespace inside the run before checking
        // whether it aligns with word boundaries.  Runs that start or end with
        // spaces still begin/finish at a word boundary even if the adjacent
        // character outside the run happens to be non-space.
        let effective_run_start = (run_start..run_end)
            .find(|&i| !chars[i].is_whitespace())
            .unwrap_or(run_start);
        let starts_clean =
            effective_run_start == 0 || chars[effective_run_start.saturating_sub(1)].is_whitespace();
        let effective_run_end = (run_start..run_end)
            .rev()
            .find(|&i| !chars[i].is_whitespace())
            .map(|i| i + 1)
            .unwrap_or(run_start);
        let ends_clean = effective_run_end >= chars.len() || chars[effective_run_end].is_whitespace();

        if starts_clean && ends_clean {
            self.emit_clean_words(&words, result);
        } else {
            self.emit_partial(chars, run_start, run_end, &words, result);
        }
    }

    /// Emit indicators for emphasis that aligns cleanly with word boundaries.
    fn emit_clean_words(
        &self,
        words: &[(usize, usize)],
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if let Some(begemphphrase) = &self.begemphphrase {
            if words.len() >= self.len_phrase {
                result.push((words[0].0, begemphphrase.clone()));

                if self.endemphphrase_before {
                    // BANA-style: phrase covers all but the last word; the last
                    // word's begemphword indicator serves as the implicit phrase
                    // terminator ("close with the word indicator before the last
                    // italicized word").
                    let last = words.last().unwrap();
                    self.emit_single_word(last.0, last.1, result);
                } else {
                    // Phrase brackets all words; explicit endemph after the last.
                    if let Some(end_t) = &self.endemphphrase {
                        result.push((words.last().unwrap().1, end_t.clone()));
                    }
                }
                return;
            }
        }
        for &(ws, we) in words {
            self.emit_single_word(ws, we, result);
        }
    }

    /// Emit a begemphword (or emphletter for single chars) for one word.
    /// No endemphword is needed when the word ends at a clean boundary.
    fn emit_single_word(
        &self,
        word_start: usize,
        word_end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if word_end - word_start == 1 {
            if let Some(t) = &self.emphletter {
                result.push((word_start, t.clone()));
                return;
            }
        }
        if let Some(t) = &self.begemphword {
            result.push((word_start, t.clone()));
        }
        // No endemphword: word ends at a space/boundary, so terminator is implicit.
    }

    /// Emit indicators for emphasis that starts or ends mid-word.
    fn emit_partial(
        &self,
        chars: &[char],
        run_start: usize,
        run_end: usize,
        words: &[(usize, usize)],
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        let total_chars: usize = words.iter().map(|(s, e)| e - s).sum();

        if total_chars == 1 {
            if let Some(t) = &self.emphletter {
                result.push((words[0].0, t.clone()));
                return;
            }
        }

        if let Some(t) = &self.begemphword {
            result.push((run_start, t.clone()));
            // endemphword is only needed when emphasis ends mid-word (the next
            // character is not a word boundary and not end-of-string).
            if let Some(end_t) = &self.endemphword {
                if run_end < chars.len() && !chars[run_end].is_whitespace() {
                    result.push((run_end, end_t.clone()));
                }
            }
            return;
        }

        // Phrase-only class (e.g. underline): no word indicator, use phrase indicators.
        if let Some(t) = &self.begemphphrase {
            result.push((run_start, t.clone()));
            // Phrase indicators always need explicit closing; emit at run_end
            // unless we're at end-of-string.
            if let Some(end_t) = &self.endemphphrase {
                if run_end < chars.len() {
                    result.push((run_end, end_t.clone()));
                }
            }
        }
    }
}

fn find_word_segments(chars: &[char], run_start: usize, run_end: usize) -> Vec<(usize, usize)> {
    let mut words = Vec::new();
    let mut pos = run_start;
    while pos < run_end {
        while pos < run_end && chars[pos].is_whitespace() {
            pos += 1;
        }
        if pos >= run_end {
            break;
        }
        let word_start = pos;
        while pos < run_end && !chars[pos].is_whitespace() {
            pos += 1;
        }
        words.push((word_start, pos));
    }
    words
}

// ── Builder ─────────────────────────────────────────────────────────────────

/// Builder for [`Indicator`].
#[derive(Debug)]
pub struct IndicatorBuilder {
    classes: HashMap<String, ClassIndicator>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self { classes: HashMap::new() }
    }

    fn class_mut(&mut self, name: &str) -> &mut ClassIndicator {
        self.classes
            .entry(name.to_string())
            .or_insert_with(|| ClassIndicator::new(name.to_string()))
    }

    pub fn emphclass(&mut self, name: &str) {
        self.class_mut(name);
    }

    pub fn emphletter(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).emphletter = Some(make_translation(dots, origin));
    }

    pub fn begemphword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begemphword = Some(make_translation(dots, origin));
    }

    pub fn endemphword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endemphword = Some(make_translation(dots, origin));
    }

    pub fn begemphphrase(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begemphphrase = Some(make_translation(dots, origin));
    }

    pub fn endemphphrase(
        &mut self,
        name: &str,
        dots: &str,
        position: &Position,
        origin: &AnchoredRule,
    ) {
        let class = self.class_mut(name);
        class.endemphphrase = Some(make_translation(dots, origin));
        class.endemphphrase_before = matches!(position, Position::Before);
    }

    pub fn lenemphphrase(&mut self, name: &str, len: usize) {
        self.class_mut(name).len_phrase = len;
    }

    pub fn begemph(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begemph = Some(make_translation(dots, origin));
    }

    pub fn endemph(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endemph = Some(make_translation(dots, origin));
    }

    pub fn build(self) -> Option<Indicator> {
        let classes: Vec<ClassIndicator> = self
            .classes
            .into_values()
            .filter(|c| c.is_indicating())
            .collect();
        if classes.is_empty() { None } else { Some(Indicator { classes }) }
    }
}

// ── Indicator ────────────────────────────────────────────────────────────────

/// Compiled emphasis indicator for one or more emphasis classes.
#[derive(Debug, Clone)]
pub struct Indicator {
    classes: Vec<ClassIndicator>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs for the given input and emphasis spans.
    ///
    /// Position `n` (where `n = input.chars().count()`) is valid and used for
    /// translations that must appear after all input characters (e.g. `endemph`
    /// at end of string).
    pub fn precompute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
    ) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut result: Vec<(usize, ResolvedTranslation)> = Vec::new();
        for class in &self.classes {
            let active = active_positions(&class.class_name, chars.len(), spans);
            class.emit(&chars, &active, &mut result);
        }
        result
    }
}
