//! The `nocontractsign` and letter sign in front of a `contraction`
//!
//! Unlike the other indications they depend on the rule translation picks, not on the text:
//! liblouis inserts them only once it has chosen a `contraction` rule (`insertLetterSign`).
//! So [`ContractionSigns`] is an amender (see [`indication`](super)), not an [`Indicator`].
//!
//! [`Indicator`]: super::Indicator

use crate::parser::{AnchoredRule, Rule};
use crate::translator::ResolvedTranslation;

/// A builder for [`ContractionSigns`]
#[derive(Debug, Default)]
pub struct ContractionSignsBuilder {
    nocontractsign: Option<(String, AnchoredRule)>,
    letsign: Option<(String, AnchoredRule)>,
}

impl ContractionSignsBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn nocontractsign(&mut self, dots: &str, origin: &AnchoredRule) {
        self.nocontractsign = Some((dots.to_string(), origin.clone()));
    }

    pub fn letsign(&mut self, dots: &str, origin: &AnchoredRule) {
        self.letsign = Some((dots.to_string(), origin.clone()));
    }

    /// The signs that go in front of a `contraction`, in order, or none if the table has
    /// neither.
    pub fn build(self) -> Option<ContractionSigns> {
        let signs: Vec<_> = self
            .nocontractsign
            .into_iter()
            .chain(self.letsign)
            .map(|(dots, origin)| ResolvedTranslation::indication(&dots, &origin))
            .collect();
        (!signs.is_empty()).then_some(ContractionSigns(signs))
    }
}

#[derive(Debug, Clone)]
pub struct ContractionSigns(Vec<ResolvedTranslation>);

impl ContractionSigns {
    /// Puts the signs in front of each `contraction` rule applied: after the indications
    /// already there, such as emphasis, but before the capitals, as in liblouis. A letter
    /// sign already there for a letter on its own isn't doubled.
    pub fn apply(&self, translations: Vec<ResolvedTranslation>) -> Vec<ResolvedTranslation> {
        let mut result = Vec::with_capacity(translations.len());
        for t in translations {
            if matches!(t.rule(), Some(Rule::Contraction { .. })) {
                // the indications in front of it, at the same position
                let run_start = result.len()
                    - result
                        .iter()
                        .rev()
                        .take_while(|p: &&ResolvedTranslation| p.input().is_empty())
                        .count();
                let at = result[run_start..]
                    .iter()
                    .position(|p| {
                        matches!(
                            p.rule(),
                            Some(
                                Rule::Capsletter { .. }
                                    | Rule::Begcapsword { .. }
                                    | Rule::Begcaps { .. }
                                    | Rule::Begcapsphrase { .. }
                            )
                        )
                    })
                    .map_or(result.len(), |i| run_start + i);
                let signs: Vec<ResolvedTranslation> = self
                    .0
                    .iter()
                    .filter(|sign| !result[run_start..].iter().any(|p| p.rule() == sign.rule()))
                    .cloned()
                    .collect();
                result.splice(at..at, signs);
            }
            result.push(t);
        }
        result
    }
}
