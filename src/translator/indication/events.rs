//! Per-position behaviour flags for the indication pre-computation pass.
//!
//! Each [`BehaviourFlag`] modifies how the translation loop handles the
//! character at that position.  Unlike output events (which produce braille
//! cells and are stored directly in the per-position translation vecs),
//! behaviour flags carry no braille output — they only gate rule selection.
//!
//! The flags are stored in a dense `Vec<BehaviourFlags>` with one entry per
//! input character.  Most entries will be [`BehaviourFlags::empty()`].

use enumset::{EnumSet, EnumSetType};

/// A rule-selection flag that the indication pass can set at a character
/// position.
#[derive(EnumSetType, Debug)]
pub enum BehaviourFlag {
    /// Suppress multi-character (contraction) rules at this position.
    ///
    /// Set by the numeric indicator for every character inside a numeric run,
    /// so that contractions are not applied to digit sequences.
    DontContract,
}

pub type BehaviourFlags = EnumSet<BehaviourFlag>;
