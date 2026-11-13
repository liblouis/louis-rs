//! Translation options and mode flags.
//!
//! [`TranslationOptions`] bundles the per-call settings that control how a
//! translation is performed. It is passed through the full translation pipeline
//! so that any stage can consult the relevant fields.

use enumset::{EnumSet, EnumSetType};

use crate::emphasis::Emphasis;

/// Flags that modify the behaviour of the translator.
///
/// These correspond to the mode bits that liblouis accepts in its `mode`
/// parameter.
// no_ops: no `|` that would return an `EnumSet` and put enumset in the API
#[derive(EnumSetType, Debug, Hash)]
#[enumset(no_ops)]
#[non_exhaustive]
pub enum TranslationMode {
    /// Translate without contractions (liblouis's `noContractions`): no `word`, `always`,
    /// `match` or other contracting rule applies. Backward: ignored.
    NoContractions,
    /// Show the word at the cursor uncontracted, in computer braille without `begcomp` and
    /// `endcomp` (`compbrlAtCursor`). Needs a cursor position; backward: ignored.
    CompbrlAtCursor,
    /// Like [`CompbrlAtCursor`](Self::CompbrlAtCursor), but only the part of the word
    /// before the cursor (`compbrlLeftCursor`).
    CompbrlLeftCursor,
    /// Drop characters the tables don't define rather than writing them out (`noUndefined`)
    NoUndefined,
    /// Back-translate input that may end in the middle of a word, such as braille typed so
    /// far (`partialTrans`): no word is taken to end, so rules for whole words or word
    /// ends don't apply, and rules for a word that goes on do. Forward: ignored.
    PartialTrans,
}

/// A set of [`TranslationMode`]s.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TranslationModes(EnumSet<TranslationMode>);

impl TranslationModes {
    /// The empty set: translate with no modes.
    pub fn empty() -> Self {
        TranslationModes(EnumSet::empty())
    }

    /// Add `value`; returns whether it was not in the set before.
    pub fn insert(&mut self, value: TranslationMode) -> bool {
        self.0.insert(value)
    }

    /// Whether `value` is in the set.
    pub fn contains(&self, value: TranslationMode) -> bool {
        self.0.contains(value)
    }
}

impl FromIterator<TranslationMode> for TranslationModes {
    fn from_iter<I: IntoIterator<Item = TranslationMode>>(modes: I) -> Self {
        TranslationModes(modes.into_iter().collect())
    }
}

/// Per-call options for a translation.
///
/// Positions are indices of characters (Unicode scalar values), not of bytes.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TranslationOptions {
    modes: TranslationModes,
    /// Which emphasis classes are active where in the input
    emphasis: Emphasis,
    cursor_position: Option<usize>,
}

impl TranslationOptions {
    /// Translate with `modes`.
    pub fn with_modes(mut self, modes: TranslationModes) -> Self {
        self.modes = modes;
        self
    }

    /// Translate with the input emphasized as `emphasis` says.
    pub fn with_emphasis(mut self, emphasis: Emphasis) -> Self {
        self.emphasis = emphasis;
        self
    }

    /// Map the cursor at input position `cursor_position` to the output, in
    /// [`TranslationResult::cursor_position`](crate::TranslationResult::cursor_position).
    pub fn with_cursor_position(mut self, cursor_position: usize) -> Self {
        self.cursor_position = Some(cursor_position);
        self
    }

    /// The modes to translate with.
    pub fn modes(&self) -> &TranslationModes {
        &self.modes
    }

    /// The emphasis of the input.
    pub fn emphasis(&self) -> &Emphasis {
        &self.emphasis
    }

    /// The cursor position in the input, if there is one.
    pub fn cursor_position(&self) -> Option<usize> {
        self.cursor_position
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translation_modes() {
        let mut modes = TranslationModes::empty();
        assert!(modes.insert(TranslationMode::NoUndefined));
        assert!(!modes.insert(TranslationMode::NoUndefined));
        assert!(modes.contains(TranslationMode::NoUndefined));
        assert!(!modes.contains(TranslationMode::NoContractions));
        let collected: TranslationModes =
            [TranslationMode::NoUndefined, TranslationMode::PartialTrans]
                .into_iter()
                .collect();
        assert!(collected.contains(TranslationMode::PartialTrans));
        assert_ne!(modes, collected);
    }
}
