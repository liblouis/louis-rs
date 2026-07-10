//! Translation options and mode flags.
//!
//! [`TranslationOptions`] bundles the per-call settings that control how a
//! translation is performed. It is passed through the full translation pipeline
//! so that any stage can consult the relevant fields.

use enumset::{EnumSet, EnumSetType};

use crate::emphasis::EmphasisSpan;

/// Flags that modify the behaviour of the translator.
///
/// These correspond to the mode bits that liblouis accepts in its `typemode`
/// parameter.
#[derive(EnumSetType, Debug)]
pub enum TranslationMode {
    NoContractions,
    CompbrlAtCursor,
    DotsIo,
    CompbrlLeftCursor,
    UcBrl,
    NoUndefined,
    PartialTrans,
}

#[derive(Debug, Clone)]
pub struct TranslationModes(EnumSet<TranslationMode>);

impl TranslationModes {
    pub fn empty() -> Self {
        TranslationModes(EnumSet::empty())
    }

    pub fn insert(&mut self, value: TranslationMode) -> bool {
        self.0.insert(value)
    }

    /// Build from a raw bit mask, where bit `i` selects the i-th
    /// [`TranslationMode`] variant. Returns `None` if any bit does not
    /// correspond to a defined mode.
    pub fn from_bits(bits: u32) -> Option<Self> {
        EnumSet::try_from_u32(bits).map(Self)
    }
}

/// Per-call options for a translation.
#[derive(Debug, Clone, Default)]
pub struct TranslationOptions {
    mode: TranslationModes,
    /// Per-character emphasis annotations (which emphasis classes are active where).
    emphasis: Vec<EmphasisSpan>,
    cursor_pos: Option<usize>,
}

impl TranslationOptions {
    pub fn with_mode(mut self, mode: TranslationModes) -> Self {
        self.mode = mode;
        self
    }

    pub fn with_emphasis(mut self, emphasis: Vec<EmphasisSpan>) -> Self {
        self.emphasis = emphasis;
        self
    }

    pub fn with_cursor_pos(mut self, cursor_pos: usize) -> Self {
        self.cursor_pos = Some(cursor_pos);
        self
    }

    pub fn mode(&self) -> &TranslationModes {
        &self.mode
    }

    pub fn emphasis(&self) -> &[EmphasisSpan] {
        &self.emphasis
    }

    pub fn cursor_pos(&self) -> Option<usize> {
        self.cursor_pos
    }
}

impl Default for TranslationModes {
    fn default() -> Self {
        Self::empty()
    }
}
