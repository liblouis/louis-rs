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
}

/// Per-call options for a translation.
#[derive(Debug, Clone, Default)]
pub struct TranslationOptions {
    pub mode: TranslationModes,
    /// Per-character emphasis annotations (which emphasis classes are active where).
    pub emphasis: Vec<EmphasisSpan>,
    pub cursor_pos: Option<usize>,
}

impl TranslationOptions {
    pub fn emphasis(&self) -> &[EmphasisSpan] {
        &self.emphasis
    }
}

impl Default for TranslationModes {
    fn default() -> Self {
        Self::empty()
    }
}
