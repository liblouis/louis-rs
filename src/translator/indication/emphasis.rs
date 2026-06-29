//! Emphasis (italic, bold, underline, …) braille indication.
//!
//! Analyses the [`EmphasisSpan`] list supplied by the caller and emits the
//! appropriate indicator cells (emphletter, begemphword, …) before or after
//! the relevant character positions.
//!
//! The choice of indicator (letter / word / phrase) depends on the full
//! run context, so the entire decision is made during precompute.

use std::collections::{HashMap, HashSet};

use crate::{
    parser::{AnchoredRule, Position},
    emphasis::EmphasisSpan,
    translator::{ResolvedTranslation, TranslationStage},
};

fn make_translation(dots: &str, origin: &AnchoredRule) -> ResolvedTranslation {
    ResolvedTranslation::new("", dots, 1, TranslationStage::Main, origin.clone())
}

/// Ordering tag attached to each emitted event.
///
/// `Open`/`Close` events are for `begemph`/`endemph` indicators and carry the
/// span's `run_end` so that same-position events can be sorted into the correct
/// LIFO (stack) order across multiple emphasis classes.  All other indicator
/// types (word, phrase, letter) use `Unordered` and retain their original
/// class-definition order.
#[derive(Debug)]
enum EmitKind {
    /// `begemph` event; `run_end` is the position where the corresponding
    /// `endemph` will be emitted.
    Open { run_end: usize },
    /// `endemph` event; `emit_start` is the position where the matching `Open`
    /// (begemph) was emitted.  Used to sort same-position closes in inverse
    /// open order (later open = closes first).
    Close { emit_start: usize },
    /// All other indicators — ordering within a position is preserved as-is.
    Unordered,
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
    /// Characters where the emphasis *mode is maintained* across them (emphmodechars).
    ///
    /// These chars are transparent to emphasis in two ways: the overall emphasis
    /// run stays active across them (so no indicator is closed and reopened), but
    /// they still act as word separators — each word on either side gets its own
    /// `begemphword`. Think of a hyphen in "well-known": one emphasis run, two
    /// word indicators. Default (empty set) falls back to Unicode whitespace.
    emphmodechars: HashSet<char>,
    /// Characters that cannot carry emphasis (noemphchars).
    ///
    /// Their positions are forced inactive even when inside a span, splitting the
    /// span into separate runs on either side.
    noemphchars: HashSet<char>,
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
            emphmodechars: HashSet::new(),
            noemphchars: HashSet::new(),
        }
    }

    fn is_emphasis_word_separator(&self, ch: char) -> bool {
        if self.emphmodechars.is_empty() {
            ch.is_whitespace()
        } else {
            self.emphmodechars.contains(&ch)
        }
    }

    /// Build a dense per-position active flag from the caller's emphasis spans.
    ///
    /// Positions occupied by `noemphchars` are forced to `false`, splitting the
    /// span into separate runs around those characters.
    fn active_positions(&self, chars: &[char], spans: &[EmphasisSpan]) -> Vec<bool> {
        let n = chars.len();
        let mut active = vec![false; n];
        for span in spans {
            if span.class == self.class_name {
                let end = span.range.end.min(n);
                for pos in span.range.start..end {
                    if !self.noemphchars.contains(&chars[pos]) {
                        active[pos] = true;
                    }
                }
            }
        }
        active
    }

    fn find_word_segments(&self, chars: &[char], run_start: usize, run_end: usize) -> Vec<(usize, usize)> {
        let mut words = Vec::new();
        let mut pos = run_start;
        while pos < run_end {
            while pos < run_end && self.is_emphasis_word_separator(chars[pos]) {
                pos += 1;
            }
            if pos >= run_end {
                break;
            }
            let word_start = pos;
            while pos < run_end && !self.is_emphasis_word_separator(chars[pos]) {
                pos += 1;
            }
            words.push((word_start, pos));
        }
        words
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
        result: &mut Vec<(usize, ResolvedTranslation, EmitKind)>,
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
        result: &mut Vec<(usize, ResolvedTranslation, EmitKind)>,
    ) {
        // begemph/endemph: general indicators that can start/end anywhere.
        if let Some(begemph) = &self.begemph {
            let effective_start = (run_start..run_end)
                .find(|&i| !self.is_emphasis_word_separator(chars[i]))
                .unwrap_or(run_start);
            result.push((effective_start, begemph.clone(), EmitKind::Open { run_end }));

            if let Some(endemph) = &self.endemph {
                // Find the last non-mode-char position in the run. endemph goes
                // at the next slot (= first trailing mode char, or n if none).
                let effective_end = (run_start..run_end)
                    .rev()
                    .find(|&i| !self.is_emphasis_word_separator(chars[i]))
                    .map(|i| i + 1)
                    .unwrap_or(run_end);
                result.push((effective_end, endemph.clone(), EmitKind::Close { emit_start: effective_start }));
            }
            return;
        }

        // begemphword / emphletter / begemphphrase path.
        let words = self.find_word_segments(chars, run_start, run_end);
        if words.is_empty() {
            return;
        }

        // Strip leading/trailing mode chars inside the run before checking
        // whether it aligns with word boundaries.  Runs that start or end with
        // mode chars still begin/finish at a word boundary even if the adjacent
        // character outside the run happens to be non-mode.
        let effective_run_start = (run_start..run_end)
            .find(|&i| !self.is_emphasis_word_separator(chars[i]))
            .unwrap_or(run_start);
        let starts_clean =
            effective_run_start == 0 || self.is_emphasis_word_separator(chars[effective_run_start.saturating_sub(1)]);
        let effective_run_end = (run_start..run_end)
            .rev()
            .find(|&i| !self.is_emphasis_word_separator(chars[i]))
            .map(|i| i + 1)
            .unwrap_or(run_start);
        let ends_clean = effective_run_end >= chars.len() || self.is_emphasis_word_separator(chars[effective_run_end]);

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
        result: &mut Vec<(usize, ResolvedTranslation, EmitKind)>,
    ) {
        if let Some(begemphphrase) = &self.begemphphrase {
            if words.len() >= self.len_phrase {
                result.push((words[0].0, begemphphrase.clone(), EmitKind::Unordered));

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
                        result.push((words.last().unwrap().1, end_t.clone(), EmitKind::Unordered));
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
        result: &mut Vec<(usize, ResolvedTranslation, EmitKind)>,
    ) {
        if word_end - word_start == 1 {
            if let Some(t) = &self.emphletter {
                result.push((word_start, t.clone(), EmitKind::Unordered));
                return;
            }
        }
        if let Some(t) = &self.begemphword {
            result.push((word_start, t.clone(), EmitKind::Unordered));
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
        result: &mut Vec<(usize, ResolvedTranslation, EmitKind)>,
    ) {
        let total_chars: usize = words.iter().map(|(s, e)| e - s).sum();

        if total_chars == 1 {
            if let Some(t) = &self.emphletter {
                result.push((words[0].0, t.clone(), EmitKind::Unordered));
                return;
            }
        }

        if let Some(t) = &self.begemphword {
            result.push((run_start, t.clone(), EmitKind::Unordered));
            // endemphword is only needed when emphasis ends mid-word (the next
            // character is not a word boundary and not end-of-string).
            if let Some(end_t) = &self.endemphword {
                if run_end < chars.len() && !self.is_emphasis_word_separator(chars[run_end]) {
                    result.push((run_end, end_t.clone(), EmitKind::Unordered));
                }
            }
            return;
        }

        // Phrase-only class (e.g. underline): no word indicator, use phrase indicators.
        if let Some(t) = &self.begemphphrase {
            result.push((run_start, t.clone(), EmitKind::Unordered));
            // Phrase indicators always need explicit closing; emit at run_end
            // unless we're at end-of-string.
            if let Some(end_t) = &self.endemphphrase {
                if run_end < chars.len() {
                    result.push((run_end, end_t.clone(), EmitKind::Unordered));
                }
            }
        }
    }
}


// ── Builder ─────────────────────────────────────────────────────────────────

/// Builder for [`Indicator`].
#[derive(Debug)]
pub struct IndicatorBuilder {
    classes: HashMap<String, ClassIndicator>,
    /// Tracks emphclass declaration order so the built `Indicator` preserves it.
    class_order: Vec<String>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self { classes: HashMap::new(), class_order: Vec::new() }
    }

    fn class_mut(&mut self, name: &str) -> &mut ClassIndicator {
        if !self.classes.contains_key(name) {
            self.class_order.push(name.to_string());
            self.classes.insert(name.to_string(), ClassIndicator::new(name.to_string()));
        }
        self.classes.get_mut(name).unwrap()
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

    pub fn emphmodechars(&mut self, name: &str, chars: &str) {
        self.class_mut(name).emphmodechars = chars.chars().collect();
    }

    pub fn noemphchars(&mut self, name: &str, chars: &str) {
        self.class_mut(name).noemphchars = chars.chars().collect();
    }

    pub fn build(mut self) -> Option<Indicator> {
        let classes: Vec<ClassIndicator> = self.class_order
            .into_iter()
            .filter_map(|name| self.classes.remove(&name))
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
    ///
    /// When multiple emphasis classes emit indicators at the same position the
    /// output is sorted to respect the liblouis stack rule:
    /// * Opens at the same position: the class whose run ends **latest** opens
    ///   first (it is the "outermost" bracket).  Tiebreaker: last-defined class
    ///   opens first.
    /// * Closes at the same position: sorted in the inverse of the open order,
    ///   i.e. the class whose run ends **earliest** closes first.  Tiebreaker:
    ///   first-defined class closes first.
    pub fn precompute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
    ) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();

        // Collect tagged events: (position, translation, kind, class_idx).
        // class_idx reflects emphclass declaration order (guaranteed by IndicatorBuilder).
        let mut events: Vec<(usize, ResolvedTranslation, EmitKind, usize)> = Vec::new();
        for (class_idx, class) in self.classes.iter().enumerate() {
            let active = class.active_positions(&chars, spans);
            let mut raw: Vec<(usize, ResolvedTranslation, EmitKind)> = Vec::new();
            class.emit(&chars, &active, &mut raw);
            for (pos, trans, kind) in raw {
                events.push((pos, trans, kind, class_idx));
            }
        }

        // Stable-sort so that same-position, same-kind events keep their relative
        // order unless the rules require a reordering.
        events.sort_by(|a, b| {
            use std::cmp::Ordering;
            let pos_ord = a.0.cmp(&b.0);
            if pos_ord != Ordering::Equal {
                return pos_ord;
            }
            match (&a.2, &b.2) {
                // Opens at same position: later run_end first, then higher class_idx first.
                (EmitKind::Open { run_end: re_a }, EmitKind::Open { run_end: re_b }) => {
                    re_b.cmp(re_a).then(b.3.cmp(&a.3))
                }
                // Closes at same position: later open (higher emit_start) closes first
                // (= inverse of open order).  Tiebreaker: lower class_idx first
                // (first-defined closes first, matching emphclass definition order).
                (EmitKind::Close { emit_start: es_a }, EmitKind::Close { emit_start: es_b }) => {
                    es_b.cmp(es_a).then(a.3.cmp(&b.3))
                }
                // All other combinations: preserve original order.
                _ => Ordering::Equal,
            }
        });

        events.into_iter().map(|(pos, trans, _, _)| (pos, trans)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emphasis::EmphasisSpan;
    use crate::parser::{AnchoredRule, RuleParser};

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn outputs_at(result: &[(usize, ResolvedTranslation)], pos: usize) -> Vec<String> {
        result
            .iter()
            .filter(|(p, _)| *p == pos)
            .map(|(_, t)| t.output().to_string())
            .collect()
    }

    #[test]
    fn emphletter_for_single_char() {
        let mut b = IndicatorBuilder::new();
        b.emphletter("italic", "⠨", &rule("always a 1"));
        let indicator = b.build().unwrap();

        let result = indicator.precompute("hello", &[EmphasisSpan::new("italic", 2..3)]);
        assert_eq!(outputs_at(&result, 2), vec!["⠨"]);
        assert!(outputs_at(&result, 0).is_empty());
    }

    #[test]
    fn begemphword_for_whole_word() {
        let mut b = IndicatorBuilder::new();
        b.begemphword("italic", "⠨", &rule("always a 1"));
        let indicator = b.build().unwrap();

        let result = indicator.precompute("hello", &[EmphasisSpan::new("italic", 0..5)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]);
    }

    #[test]
    fn noemphchars_splits_span_into_two_runs() {
        let mut b = IndicatorBuilder::new();
        b.emphletter("italic", "⠨", &rule("always a 1"));
        b.noemphchars("italic", ",");
        let indicator = b.build().unwrap();

        // The comma cannot be emphasized, so "a,b" with a span over all three chars
        // becomes two single-char runs — each gets emphletter.
        let result = indicator.precompute("a,b", &[EmphasisSpan::new("italic", 0..3)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]); // 'a'
        assert!(outputs_at(&result, 1).is_empty());     // ',' — not indicated
        assert_eq!(outputs_at(&result, 2), vec!["⠨"]); // 'b'
    }

    #[test]
    fn emphmodechars_hyphen_splits_word_within_run() {
        let mut b = IndicatorBuilder::new();
        b.begemphword("italic", "⠨", &rule("always a 1"));
        b.emphmodechars("italic", " -");
        let indicator = b.build().unwrap();

        // Without emphmodechars the hyphen would be part of the word, giving one
        // begemphword.  With '-' as a separator, "well" and "known" are two words
        // — each gets its own begemphword, but the run is not closed between them.
        let result = indicator.precompute("well-known", &[EmphasisSpan::new("italic", 0..10)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]); // "well"
        assert_eq!(outputs_at(&result, 5), vec!["⠨"]); // "known"
        assert!(outputs_at(&result, 4).is_empty());     // '-' — no indicator
    }
}
