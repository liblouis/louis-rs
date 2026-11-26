//! Emphasis (italic, bold, underline, …) braille indication.
//!
//! Analyses the [`EmphasisSpan`] list supplied by the caller and emits the
//! appropriate indicator cells (emphletter, begemphword, …) before or after
//! the relevant character positions.
//!
//! The choice of indicator (letter / word / phrase) depends on the full
//! run context, so the entire decision is made during precompute.

use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};

use super::IndicationOrder;
use crate::{
    emphasis::{Emphasis, EmphasisSpan},
    parser::{AnchoredRule, CharacterClasses, Position, Rule},
    translator::{ResolvedTranslation, table::TableContext},
};

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
    /// word indicators. Default (empty set) falls back to the table's space class.
    emphmodechars: HashSet<char>,
    /// Characters that cannot carry emphasis (noemphchars).
    ///
    /// Their positions are forced inactive even when inside a span, splitting the
    /// span into separate runs on either side.
    noemphchars: HashSet<char>,
    /// Table character classes, used to determine word separators.
    character_classes: CharacterClasses,
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
            character_classes: CharacterClasses::default(),
        }
    }

    fn is_emphasis_word_separator(&self, ch: char) -> bool {
        if self.emphmodechars.is_empty() {
            self.character_classes.is_whitespace(ch)
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
            if span.class() == self.class_name {
                let end = span.range().end.min(n);
                for pos in span.range().start..end {
                    if !self.noemphchars.contains(&chars[pos]) {
                        active[pos] = true;
                    }
                }
            }
        }
        // a space neither starts nor ends emphasis: spaces between two emphasized
        // characters are emphasized too, as in liblouis, unless they are `noemphchars`
        let mut pos = 0;
        while pos < n {
            let gap_start = pos;
            while pos < n
                && !active[pos]
                && self.character_classes.is_whitespace(chars[pos])
                && !self.noemphchars.contains(&chars[pos])
            {
                pos += 1;
            }
            if pos > gap_start && gap_start > 0 && active[gap_start - 1] && pos < n && active[pos] {
                active[gap_start..pos].fill(true);
            }
            pos += 1;
        }
        active
    }

    fn find_word_segments(
        &self,
        chars: &[char],
        run_start: usize,
        run_end: usize,
    ) -> Vec<(usize, usize)> {
        super::find_spans(chars, run_start, run_end, |c| {
            !self.is_emphasis_word_separator(c)
        })
    }

    fn is_indicating(&self) -> bool {
        self.emphletter.is_some()
            || self.begemphword.is_some()
            || self.begemph.is_some()
            || self.begemphphrase.is_some()
    }

    /// The indicators for the emphasis of this class, each with its place among the
    /// indicators at the same position: a run's begin indicators open as long as the run,
    /// its end indicators close what it opened (see [`IndicationOrder`]).
    fn emit(
        &self,
        chars: &[char],
        active: &[bool],
        definition_order: usize,
        result: &mut Vec<(usize, IndicationOrder, ResolvedTranslation)>,
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
            // a space doesn't start or end emphasis, so neither does it count for nesting
            let opened = (run_start..run_end)
                .find(|&i| !self.is_emphasis_word_separator(chars[i]))
                .unwrap_or(run_start);
            let until = (run_start..run_end)
                .rev()
                .find(|&i| !self.is_emphasis_word_separator(chars[i]))
                .map_or(run_end, |i| i + 1);
            let mut run = Vec::new();
            self.emit_run(chars, run_start, run_end, &mut run);
            result.extend(run.into_iter().map(|(pos, t)| {
                let order = match t.rule() {
                    Some(
                        Rule::Endemph { .. }
                        | Rule::Endemphword { .. }
                        | Rule::Endemphphrase { .. },
                    ) => IndicationOrder::EmphasisEnd {
                        opened: Reverse(opened),
                        definition_order,
                    },
                    _ => IndicationOrder::EmphasisBegin {
                        until: Reverse(until),
                        definition_order: Reverse(definition_order),
                    },
                };
                (pos, order, t)
            }));
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
                .find(|&i| !self.is_emphasis_word_separator(chars[i]))
                .unwrap_or(run_start);
            result.push((effective_start, begemph.clone()));

            if let Some(endemph) = &self.endemph {
                // Find the last non-mode-char position in the run. endemph goes
                // at the next slot (= first trailing mode char, or n if none).
                let effective_end = (run_start..run_end)
                    .rev()
                    .find(|&i| !self.is_emphasis_word_separator(chars[i]))
                    .map(|i| i + 1)
                    .unwrap_or(run_end);
                result.push((effective_end, endemph.clone()));
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
        let starts_clean = effective_run_start == 0
            || self.is_emphasis_word_separator(chars[effective_run_start.saturating_sub(1)]);
        let effective_run_end = (run_start..run_end)
            .rev()
            .find(|&i| !self.is_emphasis_word_separator(chars[i]))
            .map(|i| i + 1)
            .unwrap_or(run_start);
        let ends_clean = effective_run_end >= chars.len()
            || self.is_emphasis_word_separator(chars[effective_run_end]);

        if starts_clean && ends_clean {
            self.emit_clean_words(&words, result);
        } else if words.len() == 1 || self.begemphword.is_none() {
            self.emit_partial(chars, run_start, run_end, &words, result);
        } else {
            // Several words, the first or the last only partly emphasized: indicated as
            // words or as a phrase all the same, and the last word, if the emphasis ends
            // inside it, closed with endemphword ("Escherichia coli." with the period
            // not emphasized). A last word of one letter has emphletter, which needs none.
            let closed = self.emit_clean_words(&words, result);
            let (last_start, last_end) = *words.last().unwrap();
            let letter = last_end - last_start == 1 && self.emphletter.is_some();
            if let Some(end_t) = &self.endemphword
                && !closed
                && !letter
                && !ends_clean
            {
                result.push((effective_run_end, end_t.clone()));
            }
        }
    }

    /// Emit indicators for emphasis that aligns cleanly with word boundaries. Whether it
    /// closed the emphasis with an `endemphphrase` after the last word.
    fn emit_clean_words(
        &self,
        words: &[(usize, usize)],
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) -> bool {
        if let Some(begemphphrase) = &self.begemphphrase
            && words.len() >= self.len_phrase
        {
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
                    return true;
                }
            }
            return false;
        }
        for &(ws, we) in words {
            self.emit_single_word(ws, we, result);
        }
        false
    }

    /// Emit a begemphword (or emphletter for single chars) for one word.
    /// No endemphword is needed when the word ends at a clean boundary.
    fn emit_single_word(
        &self,
        word_start: usize,
        word_end: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if word_end - word_start == 1
            && let Some(t) = &self.emphletter
        {
            result.push((word_start, t.clone()));
            return;
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

        if total_chars == 1
            && let Some(t) = &self.emphletter
        {
            result.push((words[0].0, t.clone()));
            return;
        }

        if let Some(t) = &self.begemphword {
            result.push((run_start, t.clone()));
            // endemphword is only needed when emphasis ends mid-word (the next
            // character is not a word boundary and not end-of-string).
            if let Some(end_t) = &self.endemphword
                && run_end < chars.len()
                && !self.is_emphasis_word_separator(chars[run_end])
            {
                result.push((run_end, end_t.clone()));
            }
            return;
        }

        // Phrase-only class (e.g. underline): no word indicator, use phrase indicators.
        if let Some(t) = &self.begemphphrase {
            result.push((run_start, t.clone()));
            // Phrase indicators always need explicit closing; emit at run_end
            // unless we're at end-of-string.
            if let Some(end_t) = &self.endemphphrase
                && run_end < chars.len()
            {
                result.push((run_end, end_t.clone()));
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
        Self {
            classes: HashMap::new(),
            class_order: Vec::new(),
        }
    }

    fn class_mut(&mut self, name: &str) -> &mut ClassIndicator {
        if !self.classes.contains_key(name) {
            self.class_order.push(name.to_string());
            self.classes
                .insert(name.to_string(), ClassIndicator::new(name.to_string()));
        }
        self.classes.get_mut(name).unwrap()
    }

    pub fn emphclass(&mut self, name: &str) {
        self.class_mut(name);
    }

    pub fn emphletter(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).emphletter = Some(ResolvedTranslation::indication(dots, origin));
    }

    pub fn begemphword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begemphword = Some(ResolvedTranslation::indication(dots, origin));
    }

    pub fn endemphword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endemphword = Some(ResolvedTranslation::indication(dots, origin));
    }

    pub fn begemphphrase(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begemphphrase = Some(ResolvedTranslation::indication(dots, origin));
    }

    pub fn endemphphrase(
        &mut self,
        name: &str,
        dots: &str,
        position: &Position,
        origin: &AnchoredRule,
    ) {
        let class = self.class_mut(name);
        class.endemphphrase = Some(ResolvedTranslation::indication(dots, origin));
        class.endemphphrase_before = matches!(position, Position::Before);
    }

    pub fn lenemphphrase(&mut self, name: &str, len: usize) {
        self.class_mut(name).len_phrase = len;
    }

    pub fn begemph(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begemph = Some(ResolvedTranslation::indication(dots, origin));
    }

    pub fn endemph(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endemph = Some(ResolvedTranslation::indication(dots, origin));
    }

    pub fn emphmodechars(&mut self, name: &str, chars: &str) {
        self.class_mut(name).emphmodechars = chars.chars().collect();
    }

    pub fn noemphchars(&mut self, name: &str, chars: &str) {
        self.class_mut(name).noemphchars = chars.chars().collect();
    }

    pub fn build(mut self, ctx: &TableContext) -> Option<Indicator> {
        let classes: Vec<ClassIndicator> = self
            .class_order
            .into_iter()
            .filter_map(|name| {
                let mut c = self.classes.remove(&name)?;
                c.character_classes = ctx.character_classes().clone();
                Some(c)
            })
            .filter(|c| c.is_indicating())
            .collect();
        if classes.is_empty() {
            None
        } else {
            Some(Indicator { classes })
        }
    }
}

// ── Indicator ────────────────────────────────────────────────────────────────

/// Compiled emphasis indicator for one or more emphasis classes.
#[derive(Debug, Clone)]
pub struct Indicator {
    classes: Vec<ClassIndicator>,
}

impl Indicator {
    /// The indicators for the given input and emphasis spans, at their positions and with
    /// their place among the indicators at the same position.
    ///
    /// Position `n` (where `n = input.chars().count()`) is valid and used for
    /// translations that must appear after all input characters (e.g. `endemph`
    /// at end of string).
    pub fn precompute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
    ) -> Vec<(usize, IndicationOrder, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut result = Vec::new();
        // the classes are in the order of their `emphclass` rules (`IndicatorBuilder`)
        for (definition_order, class) in self.classes.iter().enumerate() {
            let active = class.active_positions(&chars, spans);
            class.emit(&chars, &active, definition_order, &mut result);
        }
        result
    }
}

/// The emphasis of what the chosen backward `translations` output: an `emphletter`
/// emphasizes what the next rule writes, a `begemphword` or `begemph` everything up to the
/// next `space` rule or the matching end, and a `begemphphrase` everything up to its
/// `endemphphrase`.
pub fn backward_emphasis(translations: &[ResolvedTranslation]) -> Emphasis {
    // A small state machine: the classes active for the phrase, for the word and for the
    // next rule only. The indicators chosen switch them on and off, a space ends the word,
    // and whatever a translation writes gets the classes active at that point.
    let mut phrase: HashSet<&str> = HashSet::new();
    let mut word: HashSet<&str> = HashSet::new();
    let mut next: HashSet<&str> = HashSet::new();
    let mut spans = Vec::new();
    let mut position = 0;
    for translation in translations {
        match translation.rule() {
            Some(Rule::Emphletter { name, .. }) => {
                next.insert(name);
            }
            Some(Rule::Begemphword { name, .. } | Rule::Begemph { name, .. }) => {
                word.insert(name);
            }
            // `endemphword` and `endemphphrase` can share their dots (UEB), so like liblouis
            // an end without a word to end ends the phrase
            Some(Rule::Endemphword { name, .. } | Rule::Endemph { name, .. }) => {
                if !word.remove(name.as_str()) {
                    phrase.remove(name.as_str());
                }
            }
            Some(Rule::Begemphphrase { name, .. }) => {
                phrase.insert(name);
            }
            Some(Rule::Endemphphrase { name, .. }) => {
                phrase.remove(name.as_str());
            }
            _ => (),
        }
        // a space ends a word's emphasis. liblouis only ends it after writing the space,
        // which so gets the emphasis of the word before it; louis-rs doesn't do that
        if matches!(translation.rule(), Some(Rule::Space { .. })) {
            word.clear();
        }
        let written = translation.output().chars().count();
        if written > 0 {
            for class in &(&phrase | &word) | &next {
                spans.push(EmphasisSpan::new(class, position..position + written));
            }
            next.clear();
        }
        position += written;
    }
    spans.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emphasis::EmphasisSpan;
    use crate::parser::{AnchoredRule, RuleParser};
    use crate::translator::table::TableContext;

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn outputs_at(
        result: &[(usize, IndicationOrder, ResolvedTranslation)],
        pos: usize,
    ) -> Vec<String> {
        result
            .iter()
            .filter(|(p, _, _)| *p == pos)
            .map(|(_, _, t)| t.output().to_string())
            .collect()
    }

    #[test]
    fn emphletter_for_single_char() {
        let mut b = IndicatorBuilder::new();
        b.emphletter("italic", "⠨", &rule("always a 1"));
        let indicator = b.build(&TableContext::default()).unwrap();

        let result = indicator.precompute("hello", &[EmphasisSpan::new("italic", 2..3)]);
        assert_eq!(outputs_at(&result, 2), vec!["⠨"]);
        assert!(outputs_at(&result, 0).is_empty());
    }

    #[test]
    fn begemphword_for_whole_word() {
        let mut b = IndicatorBuilder::new();
        b.begemphword("italic", "⠨", &rule("always a 1"));
        let indicator = b.build(&TableContext::default()).unwrap();

        let result = indicator.precompute("hello", &[EmphasisSpan::new("italic", 0..5)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]);
    }

    #[test]
    fn words_before_unemphasized_punctuation_each_get_a_word_indicator() {
        // "ab cd." with "ab cd" emphasized: a word indicator for each word, and the last
        // closed before the period, not one word indicator for the whole run
        let mut b = IndicatorBuilder::new();
        b.begemphword("italic", "⠨", &rule("always a 1"));
        b.endemphword("italic", "⠠", &rule("always a 1"));
        let ctx = TableContext::compile(&[rule("space \\s 0")]).unwrap();
        let indicator = b.build(&ctx).unwrap();

        let result = indicator.precompute("ab cd.", &[EmphasisSpan::new("italic", 0..5)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]);
        assert_eq!(outputs_at(&result, 3), vec!["⠨"]);
        assert_eq!(outputs_at(&result, 5), vec!["⠠"]);
    }

    #[test]
    fn noemphchars_splits_span_into_two_runs() {
        let mut b = IndicatorBuilder::new();
        b.emphletter("italic", "⠨", &rule("always a 1"));
        b.noemphchars("italic", ",");
        let indicator = b.build(&TableContext::default()).unwrap();

        // The comma cannot be emphasized, so "a,b" with a span over all three chars
        // becomes two single-char runs — each gets emphletter.
        let result = indicator.precompute("a,b", &[EmphasisSpan::new("italic", 0..3)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]); // 'a'
        assert!(outputs_at(&result, 1).is_empty()); // ',' — not indicated
        assert_eq!(outputs_at(&result, 2), vec!["⠨"]); // 'b'
    }

    #[test]
    fn emphmodechars_hyphen_splits_word_within_run() {
        let mut b = IndicatorBuilder::new();
        b.begemphword("italic", "⠨", &rule("always a 1"));
        b.emphmodechars("italic", " -");
        let indicator = b.build(&TableContext::default()).unwrap();

        // Without emphmodechars the hyphen would be part of the word, giving one
        // begemphword.  With '-' as a separator, "well" and "known" are two words
        // — each gets its own begemphword, but the run is not closed between them.
        let result = indicator.precompute("well-known", &[EmphasisSpan::new("italic", 0..10)]);
        assert_eq!(outputs_at(&result, 0), vec!["⠨"]); // "well"
        assert_eq!(outputs_at(&result, 5), vec!["⠨"]); // "known"
        assert!(outputs_at(&result, 4).is_empty()); // '-' — no indicator
    }
}
