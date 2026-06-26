//! Translation options and mode flags.
//!
//! [`TranslationOptions`] bundles the per-call settings that control how a
//! translation is performed. It is passed through the full translation pipeline
//! so that any stage can consult the relevant fields.

use enumset::{EnumSet, EnumSetType};

use crate::text_attribute::TextAttributes;

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
#[derive(Debug, Clone)]
pub struct TranslationOptions {
    pub mode: TranslationModes,
    /// Per-character text attribute annotations (what liblouis calls typeforms).
    pub typeforms: Option<Vec<TextAttributes>>,
    pub cursor_pos: Option<usize>,
}

impl TranslationOptions {
    pub fn typeforms(&self) -> &[TextAttributes] {
        self.typeforms.as_deref().unwrap_or(&[])
    }
}

impl Default for TranslationOptions {
    fn default() -> Self {
        Self {
            mode: TranslationModes::empty(),
            typeforms: None,
            cursor_pos: None,
        }
    }
}
