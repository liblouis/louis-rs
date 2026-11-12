//! Translation options and mode flags.
//!
//! [`TranslationOptions`] bundles the per-call settings that control how a
//! translation is performed. It is passed through the full translation pipeline
//! so that any stage can consult the relevant fields.

use enumset::{EnumSet, EnumSetType};

use crate::emphasis::EmphasisSpan;

/// Flags that modify the behaviour of the translator.
///
/// These correspond to the mode bits that liblouis accepts in its `mode`
/// parameter.
#[derive(EnumSetType, Debug)]
#[non_exhaustive]
pub enum TranslationMode {
    /// Translate without contractions (liblouis's `noContractions`). Not implemented yet:
    /// ignored.
    NoContractions,
    /// Translate the word at the cursor in computer braille. Not implemented yet: ignored.
    CompbrlAtCursor,
    /// Read and write braille as dot patterns rather than characters (`dotsIO`). Not
    /// implemented yet: ignored.
    DotsIo,
    /// Translate from the cursor to the end of the word in computer braille. Not
    /// implemented yet: ignored.
    CompbrlLeftCursor,
    /// Write computer braille with uppercase letters (`ucBrl`). Not implemented yet: ignored.
    UcBrl,
    /// Drop characters the tables don't define rather than writing them out (`noUndefined`)
    NoUndefined,
    /// Translate a partial input (`partialTrans`). Not implemented yet: ignored.
    PartialTrans,
}

/// A set of [`TranslationMode`]s.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, Default)]
pub struct TranslationOptions {
    mode: TranslationModes,
    /// Per-character emphasis annotations (which emphasis classes are active where).
    emphasis: Vec<EmphasisSpan>,
    cursor_pos: Option<usize>,
}

impl TranslationOptions {
    /// Translate with `mode`.
    pub fn with_mode(mut self, mode: TranslationModes) -> Self {
        self.mode = mode;
        self
    }

    /// Translate with the input emphasized as `emphasis` says.
    pub fn with_emphasis(mut self, emphasis: Vec<EmphasisSpan>) -> Self {
        self.emphasis = emphasis;
        self
    }

    /// Map the cursor at input position `cursor_pos` to the output, in
    /// [`TranslationResult::cursor_pos`](crate::TranslationResult::cursor_pos).
    pub fn with_cursor_pos(mut self, cursor_pos: usize) -> Self {
        self.cursor_pos = Some(cursor_pos);
        self
    }

    /// The modes to translate with.
    pub fn mode(&self) -> &TranslationModes {
        &self.mode
    }

    /// The emphasis of the input.
    pub fn emphasis(&self) -> &[EmphasisSpan] {
        &self.emphasis
    }

    /// The cursor position in the input, if there is one.
    pub fn cursor_pos(&self) -> Option<usize> {
        self.cursor_pos
    }
}

impl Default for TranslationModes {
    fn default() -> Self {
        Self::empty()
    }
}
