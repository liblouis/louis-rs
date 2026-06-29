//! Computer braille indication (begcomp/endcomp).
//!
//! Analyses the `computer_braille` emphasis spans and emits `begcomp` before
//! each span and `endcomp` after each span.

use crate::{
    emphasis::EmphasisSpan,
    parser::AnchoredRule,
    translator::{ResolvedTranslation, TranslationStage},
};

fn make_translation(dots: &str, origin: &AnchoredRule) -> ResolvedTranslation {
    ResolvedTranslation::new("", dots, 1, TranslationStage::Main, origin.clone())
}

// ── Builder ──────────────────────────────────────────────────────────────────

/// Builder for [`Indicator`].
#[derive(Debug)]
pub struct IndicatorBuilder {
    begcomp: Option<(String, AnchoredRule)>,
    endcomp: Option<(String, AnchoredRule)>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            begcomp: None,
            endcomp: None,
        }
    }

    pub fn begcomp(&mut self, dots: &str, origin: &AnchoredRule) {
        self.begcomp = Some((dots.to_string(), origin.clone()));
    }

    pub fn endcomp(&mut self, dots: &str, origin: &AnchoredRule) {
        self.endcomp = Some((dots.to_string(), origin.clone()));
    }

    pub fn build(self) -> Option<Indicator> {
        let (begcomp_dots, begcomp_rule) = self.begcomp?;
        let begcomp = make_translation(&begcomp_dots, &begcomp_rule);
        let endcomp = self
            .endcomp
            .map(|(dots, rule)| make_translation(&dots, &rule));
        Some(Indicator { begcomp, endcomp })
    }
}

// ── Indicator ────────────────────────────────────────────────────────────────

/// Compiled computer braille indicator.
#[derive(Debug, Clone)]
pub struct Indicator {
    /// Translation emitted at the start of each computer braille span.
    begcomp: ResolvedTranslation,
    /// Translation emitted at the end of each computer braille span (if defined).
    endcomp: Option<ResolvedTranslation>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs for the given input and emphasis spans.
    ///
    /// Emits `begcomp` at `span.range.start` and `endcomp` at `span.range.end`
    /// (clamped to `n`) for each span with `class == "computer_braille"`.
    /// Only emits for spans where `start < end`. Result is sorted by position.
    pub fn precompute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
    ) -> Vec<(usize, ResolvedTranslation)> {
        let n = input.chars().count();
        let mut result: Vec<(usize, ResolvedTranslation)> = Vec::new();

        for span in spans {
            if span.class != "computer_braille" {
                continue;
            }
            let start = span.range.start;
            let end = span.range.end.min(n);
            if start >= end {
                continue;
            }
            result.push((start, self.begcomp.clone()));
            if let Some(ref endcomp) = self.endcomp {
                result.push((end, endcomp.clone()));
            }
        }

        result.sort_by_key(|(pos, _)| *pos);
        result
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

    #[test]
    fn begcomp_emitted_at_span_start() {
        let mut b = IndicatorBuilder::new();
        b.begcomp("⠿", &rule("begcomp 123456"));
        b.endcomp("⠿", &rule("endcomp 123456"));
        let indicator = b.build().unwrap();

        let result = indicator.precompute("hello", &[EmphasisSpan::new("computer_braille", 1..4)]);
        let positions: Vec<usize> = result.iter().map(|(p, _)| *p).collect();
        assert!(positions.contains(&1));
        assert!(positions.contains(&4));
    }

    #[test]
    fn non_computer_braille_spans_ignored() {
        let mut b = IndicatorBuilder::new();
        b.begcomp("⠿", &rule("begcomp 123456"));
        let indicator = b.build().unwrap();

        let result = indicator.precompute("hello", &[EmphasisSpan::new("italic", 0..5)]);
        assert!(result.is_empty());
    }
}
