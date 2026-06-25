//! Pre-computed indication events.
//!
//! Rather than running indicator state machines in parallel with the translation
//! loop, the input text is analysed once before translation begins. For each
//! character position the events that must be emitted at that position, and any
//! rule-selection flags that apply at that position, are recorded in a flat
//! array.
//!
//! # Data structure choice: dense Vec over sparse HashMap
//!
//! Indication events (numsign, capsletter, …) occur only at transition points
//! and would naturally suggest a sparse representation. However, the
//! `dontContract` flag — which suppresses contraction rules for every position
//! inside a numeric run — is inherently dense: it is active for each character
//! within the run, not just at its boundaries. Storing it sparsely would
//! require re-deriving "am I inside a numeric run?" during translation, which
//! reintroduces the state machine we are trying to eliminate.
//!
//! A dense `Vec<EnumSet<IndicationEvent>>` with one entry per input character
//! avoids this. Most entries will be empty (`EnumSet::empty()`). For typical
//! braille inputs (sentences to short paragraphs) the allocation is on the
//! order of a few hundred bytes and lookup is a plain array index.
//!
//! See the Architecture Decision Record "Pre-compute indication events before
//! translation" in `doc/Architecture_Decision_Records.org`.

use enumset::{EnumSet, EnumSetType};
use std::ops::BitOr;

/// An indication event at a single character position.
///
/// `*Start` / `*End` variants bracket a run; the plain variants (e.g.
/// [`Number`](IndicationEvent::Number)) mark that we are still inside a run,
/// which is used to suppress contraction rules (liblouis `dontContract`).
#[derive(EnumSetType, Debug)]
pub enum IndicationEvent {
    AllCaps,
    AllCapsStart,
    AllCapsEnd,
    Uppercase,
    UppercaseStart,
    UppercaseEnd,
    Number,
    NumberStart,
    NumberEnd,
    Italic,
    Bold,
    LetterSign,
    NoContractSign,
    /// Suppress contraction rules at this position (liblouis `dontContract`).
    /// Set for every character inside a numeric run and for characters covered
    /// by a `no_contract` typeform bit.
    DontContract,
}

/// Pre-computed indication events for an entire input string.
///
/// One [`EnumSet<IndicationEvent>`] per input character, indexed by character
/// position (not byte offset). Most entries will be empty.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct IndicationEvents(Vec<EnumSet<IndicationEvent>>);

impl IndicationEvents {
    pub fn new(len: usize) -> Self {
        IndicationEvents(vec![EnumSet::empty(); len])
    }

    pub fn get(&self, pos: usize) -> EnumSet<IndicationEvent> {
        self.0.get(pos).copied().unwrap_or(EnumSet::empty())
    }

    pub fn insert(&mut self, pos: usize, event: IndicationEvent) {
        if let Some(entry) = self.0.get_mut(pos) {
            entry.insert(event);
        }
    }
}

impl From<Vec<EnumSet<IndicationEvent>>> for IndicationEvents {
    fn from(v: Vec<EnumSet<IndicationEvent>>) -> Self {
        IndicationEvents(v)
    }
}

impl BitOr for IndicationEvents {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        let len = self.0.len().max(rhs.0.len());
        let mut result = vec![EnumSet::empty(); len];
        for (i, entry) in result.iter_mut().enumerate() {
            *entry = self.0.get(i).copied().unwrap_or(EnumSet::empty())
                | rhs.0.get(i).copied().unwrap_or(EnumSet::empty());
        }
        IndicationEvents(result)
    }
}
