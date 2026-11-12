//! Per-character emphasis annotation for braille translation.

use std::ops::Range;

/// An emphasis class active over a contiguous range of characters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmphasisSpan {
    class: String,
    range: Range<usize>,
}

impl EmphasisSpan {
    /// Emphasis of `class` over the characters in `range`. The class must be declared with
    /// the `emphclass` opcode in the table, e.g. `"italic"`, `"bold"` or `"underline"`.
    pub fn new(class: impl Into<String>, range: Range<usize>) -> Self {
        Self {
            class: class.into(),
            range,
        }
    }

    /// The name of the emphasis class.
    pub fn class(&self) -> &str {
        &self.class
    }

    /// The characters emphasized, as character indices `[start, end)`.
    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }
}

/// The emphasis of a text: which [`EmphasisSpan`]s it has, liblouis's typeform.
///
/// The spans are kept in one order, by class and then by start, and spans of the same class
/// that overlap or touch are merged, so two `Emphasis` are equal when they emphasize the
/// same characters the same way, however they were built.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Emphasis(Vec<EmphasisSpan>);

impl Emphasis {
    /// The spans, by class and then by start.
    pub fn spans(&self) -> &[EmphasisSpan] {
        &self.0
    }

    /// Whether nothing is emphasized.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The emphasis carried over to another text, whose character `i` came from the
    /// character `sources[i]` of this one.
    pub(crate) fn remap(&self, sources: &[usize]) -> Emphasis {
        sources
            .iter()
            .enumerate()
            .flat_map(|(i, &from)| {
                self.0
                    .iter()
                    .filter(move |span| span.range.contains(&from))
                    .map(move |span| EmphasisSpan::new(span.class.clone(), i..i + 1))
            })
            .collect()
    }
}

impl FromIterator<EmphasisSpan> for Emphasis {
    fn from_iter<I: IntoIterator<Item = EmphasisSpan>>(spans: I) -> Self {
        let mut spans: Vec<EmphasisSpan> =
            spans.into_iter().filter(|s| !s.range.is_empty()).collect();
        spans.sort_by(|a, b| (&a.class, a.range.start).cmp(&(&b.class, b.range.start)));
        let mut merged: Vec<EmphasisSpan> = Vec::with_capacity(spans.len());
        for span in spans {
            match merged.last_mut() {
                Some(last) if last.class == span.class && span.range.start <= last.range.end => {
                    last.range.end = last.range.end.max(span.range.end);
                }
                _ => merged.push(span),
            }
        }
        Emphasis(merged)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emphasis_is_kept_in_one_order_and_merged() {
        let emphasis: Emphasis = [
            EmphasisSpan::new("italic", 3..4),
            EmphasisSpan::new("bold", 0..2),
            EmphasisSpan::new("italic", 0..1),
            EmphasisSpan::new("italic", 1..3),
            EmphasisSpan::new("bold", 5..5),
        ]
        .into_iter()
        .collect();
        assert_eq!(
            emphasis.spans(),
            [
                EmphasisSpan::new("bold", 0..2),
                EmphasisSpan::new("italic", 0..4)
            ]
        );
    }

    #[test]
    fn remapped_emphasis_follows_the_characters() {
        let emphasis: Emphasis = [EmphasisSpan::new("italic", 1..3)].into_iter().collect();
        // the second character became two, and the last one went away
        let remapped = emphasis.remap(&[0, 1, 1, 2]);
        assert_eq!(remapped.spans(), [EmphasisSpan::new("italic", 1..4)]);
    }
}
