//! Per-character emphasis annotation for braille translation.

use std::ops::Range;

/// An emphasis class active over a contiguous range of input characters.
///
/// The translator inspects these spans before translation begins to determine
/// which emphasis indicator cells to insert.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmphasisSpan {
    /// Name of the emphasis class (e.g. `"italic"`, `"bold"`, `"underline"`).
    /// Must match a class declared with the `emphclass` opcode in the table.
    pub class: String,
    /// Character-index range `[start, end)` over which the class is active.
    pub range: Range<usize>,
}

impl EmphasisSpan {
    pub fn new(class: impl Into<String>, range: Range<usize>) -> Self {
        Self {
            class: class.into(),
            range,
        }
    }
}
