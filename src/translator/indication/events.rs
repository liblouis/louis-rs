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
//! A dense `Vec<IndicationFlags>` with one entry per input character avoids
//! this. Most entries are zero (no events, no suppression). For typical braille
//! inputs (sentences to short paragraphs) the allocation is on the order of
//! 1–4 KB and lookup is a plain array index with no hashing overhead.
//!
//! # TODO
//!
//! Replace [`IndicationEvent`] and the inner `Vec<Vec<IndicationEvent>>` with a
//! bitflag word (e.g. `bitflags!` over a `u32`) so that the per-position
//! representation is a plain integer with no heap allocation. The `Vec<u32>`
//! over all character positions is the target data structure.
//!
//! See the Architecture Decision Record "Pre-compute indication events before
//! translation" in `doc/Architecture_Decision_Records.org`.

/// An indication event at a single character position.
///
/// `*Start` / `*End` variants bracket a run; the plain variants (e.g.
/// [`Number`](IndicationEvent::Number)) mark that we are still inside a run,
/// which is used to suppress contraction rules (liblouis `dontContract`).
#[derive(Debug, Clone, PartialEq)]
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
    /// Suppress contraction rules at this position (liblouis `dontContract`).
    /// Set for every character inside a numeric run and for characters covered
    /// by a `no_contract` typeform bit.
    DontContract,
}

/// Pre-computed indication events for an entire input string.
///
/// One entry per input character, indexed by character position (not byte
/// offset). Most entries will be empty.
#[derive(Debug, Clone, Default)]
pub struct IndicationEvents(Vec<Vec<IndicationEvent>>);

impl IndicationEvents {
    pub fn new(len: usize) -> Self {
        IndicationEvents(vec![Vec::new(); len])
    }

    pub fn get(&self, pos: usize) -> &[IndicationEvent] {
        self.0.get(pos).map(Vec::as_slice).unwrap_or(&[])
    }

    pub fn push(&mut self, pos: usize, event: IndicationEvent) {
        if let Some(entry) = self.0.get_mut(pos) {
            entry.push(event);
        }
    }
}
