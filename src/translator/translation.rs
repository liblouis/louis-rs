use crate::{
    parser::{AnchoredRule, Precedence},
    translator::{effect::Effect, swap::Swapper},
};

/// A translation can have multiple stages.
///
/// The `Main` translation is always done. The others are only done if
/// specific rules are present in a translation table
#[derive(Debug, PartialEq, Clone, Default, Copy)]
pub enum TranslationStage {
    /// Pre-translation stage where the `correct` rules are applied
    Pre,
    /// Main translation stage where all the translation and the
    /// context rules are applied
    #[default]
    Main,
    /// The second post-translation stage where the `pass2` rules are applied
    Post1,
    /// The third post-translation stage where the `pass3` rules are applied
    Post2,
    /// The fourth post-translation stage where the `pass4` rules are applied
    Post3,
}

impl std::fmt::Display for TranslationStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranslationStage::Pre => write!(f, "Pre"),
            TranslationStage::Main => write!(f, "Main"),
            TranslationStage::Post1 => write!(f, "Pass2"),
            TranslationStage::Post2 => write!(f, "Pass3"),
            TranslationStage::Post3 => write!(f, "Pass4"),
        }
    }
}

/// Wrapper type to handle different kinds of translation targets.
///
/// In most case a translation just translates to a literal string. However in some cases the output
/// depends on a capture from the input or even some modified (_swapped_) version of the capture
#[derive(Debug, PartialEq, Clone)]
pub enum TranslationTarget {
    /// Translate to a literal string
    Literal(String),
    /// Translate to the result of a capture
    Capture,
    /// Translate to the result of a capture but also apply the mapping defined in the [`Swapper`]
    Swap(Swapper),
}

impl std::fmt::Display for TranslationTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranslationTarget::Literal(s) => write!(f, "{}", s),
            t => unreachable!(
                "to_string() should never be invoked on an unresolved TranslationTarget {:?}",
                t
            ),
        }
    }
}

impl TranslationTarget {
    pub fn resolve(self, capture: &str) -> Self {
        match self {
            TranslationTarget::Literal(_) => self,
            TranslationTarget::Capture => TranslationTarget::Literal(capture.to_string()),
            TranslationTarget::Swap(swapper) => TranslationTarget::Literal(swapper.swap(capture)),
        }
    }
}

/// A sequence of [`TranslationTarget`]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct TranslationTargets(Vec<TranslationTarget>);

impl std::fmt::Display for TranslationTargets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self.0.iter().map(|t| t.to_string()).collect();
        write!(f, "{}", s)
    }
}

pub trait Resolve {
    fn resolve(self, capture: &str, weight: usize, offset: usize) -> ResolvedTranslation;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnresolvedTranslation {
    output: Vec<TranslationTarget>,
    precedence: Precedence,
    stage: TranslationStage,
    effects: Vec<Effect>,
    origin: Option<AnchoredRule>,
}

impl UnresolvedTranslation {
    pub fn new(
        output: &[TranslationTarget],
        precedence: Precedence,
        stage: TranslationStage,
        effects: &[Effect],
        origin: impl Into<Option<AnchoredRule>>,
    ) -> Self {
        Self {
            output: output.to_vec(),
            precedence,
            stage,
            effects: effects.to_vec(),
            origin: origin.into(),
        }
    }
}

impl Resolve for UnresolvedTranslation {
    fn resolve(self, capture: &str, weight: usize, offset: usize) -> ResolvedTranslation {
        let resolved: String = self
            .output
            .iter()
            .cloned()
            .map(|t| t.resolve(capture))
            .map(|t| t.to_string())
            .collect();
        let length = capture.chars().count();
        ResolvedTranslation {
            input: capture.to_string(),
            output: resolved,
            length,
            weight,
            offset,
            precedence: self.precedence,
            stage: self.stage,
            effects: self.effects,
            origin: self.origin,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Translation {
    Resolved(ResolvedTranslation),
    Unresolved(UnresolvedTranslation),
}

impl Resolve for Translation {
    fn resolve(self, capture: &str, weight: usize, offset: usize) -> ResolvedTranslation {
        match self {
            Translation::Resolved(translation) => {
                translation.with_weight(weight).with_offset(offset)
            }
            Translation::Unresolved(unresolved) => unresolved.resolve(capture, weight, offset),
        }
    }
}

/// The basic unit of a translation.
///
/// Maps an `input` string to an `output` which is typically also a string.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ResolvedTranslation {
    /// Input string to be translated
    input: String,
    /// The translation of `input`, typically Unicode braille. In the case of back-translation the
    /// `input` contains Unicode braille and the output plain text.
    output: String,
    /// Number of chars in `input`
    length: usize,
    /// Weight of a translation. Typically this is the length of the input, but often it includes
    /// word boundaries as well. In some cases the weight has to be calculated dynamically, for
    /// instance for `match` opcodes where the length of the matched input often depends on regular
    /// expressions.
    weight: usize,
    /// The `match` opcode contains a pre-pattern which is essentially a look-behind regexp. The
    /// `offset` is the length of this pre-pattern (calculated at run-time) so that the translation
    /// can be applied later in the input string, when the pre-pattern has been consumed.
    offset: usize,
    /// The precedence of a translation based on the precedence of the originating translation rule
    precedence: Precedence,
    /// The stage in which this translation is applied
    stage: TranslationStage,
    /// A possibly empty list of [`Effect`]s. These will be applied to the
    /// [`Environment`](crate::translator::effect::Environment) if this Translation is used
    effects: Vec<Effect>,
    /// Which translation rule was the cause for this translation
    origin: Option<AnchoredRule>,
}

/// A subset of translation for which [`Hash`] and [`Eq`] is implemented
#[derive(Debug, PartialEq, Eq, Clone, Default, Hash)]
pub struct TranslationSubset {
    input: String,
    output: String,
    length: usize,
    weight: usize,
}

impl From<&ResolvedTranslation> for TranslationSubset {
    fn from(translation: &ResolvedTranslation) -> Self {
        Self {
            input: translation.input(),
            output: translation.output(),
            length: translation.length(),
            weight: translation.weight(),
        }
    }
}

impl ResolvedTranslation {
    pub fn new(
        input: &str,
        output: &str,
        weight: usize,
        stage: TranslationStage,
        // FIXME: this is some weird thing recommended by Claude: apparently the `impl
        // Into<Option<T>>` trait bound automatically converts `T` to `Some(T)` and `None` to
        // `None`, giving you the overloaded behavior you want with a single function. This is more
        // idiomatic than having separate `new` and `new_with_origin` methods.
        origin: impl Into<Option<AnchoredRule>>,
    ) -> Self {
        Self {
            input: input.to_string(),
            output: output.to_string(),
            length: input.chars().count(),
            weight,
            offset: 0,
            precedence: Precedence::Default,
            stage,
            effects: Vec::default(),
            origin: origin.into(),
        }
    }

    pub fn input(&self) -> String {
        self.input.clone()
    }

    pub fn output(&self) -> String {
        self.output.to_string()
    }

    pub fn length(&self) -> usize {
        self.length
    }

    pub fn weight(&self) -> usize {
        self.weight
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn precedence(&self) -> Precedence {
        self.precedence.clone()
    }

    pub fn origin(&self) -> Option<AnchoredRule> {
        self.origin.clone()
    }

    pub fn stage(&self) -> TranslationStage {
        self.stage
    }

    pub fn effects(&self) -> &Vec<Effect> {
        &self.effects
    }

    /// Set the `input` of a translation.
    pub fn with_input(self, input: &str) -> Self {
        Self {
            input: input.to_string(),
            length: input.chars().count(),
            ..self
        }
    }

    /// Set the `weight` of a translation.
    pub fn with_weight(self, weight: usize) -> Self {
        Self { weight, ..self }
    }

    /// Set the `offset` of a translation.
    pub fn with_offset(self, offset: usize) -> Self {
        Self { offset, ..self }
    }

    /// Decrement the `offset` of a translation.
    pub fn decrement_offset(self, decrement: usize) -> Self {
        Self {
            offset: self.offset - decrement,
            ..self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::translator::swap::Swapper;

    #[test]
    fn resolve_literal() {
        assert_eq!(
            TranslationTarget::Literal("foo".to_string()).resolve("bar"),
            TranslationTarget::Literal("foo".to_string())
        );
    }

    #[test]
    fn resolve_capture() {
        let resolved = TranslationTarget::Capture.resolve("CAPTURED");
        assert_eq!(resolved, TranslationTarget::Literal("CAPTURED".to_string()));
    }

    #[test]
    fn resolve_swap() {
        let swapper = Swapper::new(&[('a', "A"), ('b', "B"), ('c', "C")]);

        let resolved = TranslationTarget::Swap(swapper).resolve("abc");
        assert_eq!(resolved, TranslationTarget::Literal("ABC".to_string()));
    }

    #[test]
    fn translation_capture_empty() {
        let translation = UnresolvedTranslation::new(
            &[TranslationTarget::Capture],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("", 5, 0);

        assert_eq!(result.input(), "");
        assert_eq!(result.output(), "");
    }

    #[test]
    fn translation_capture_literal() {
        let translation = UnresolvedTranslation::new(
            &[TranslationTarget::Capture],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("captured", 8, 0);

        assert_eq!(result.input(), "captured");
        assert_eq!(result.output(), "captured");
    }

    #[test]
    fn translation_capture_capture() {
        let translation = UnresolvedTranslation::new(
            &[
                TranslationTarget::Literal("<".to_string()),
                TranslationTarget::Capture,
                TranslationTarget::Literal(">".to_string()),
            ],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("MIDDLE", 8, 0);

        assert_eq!(result.input(), "MIDDLE");
        assert_eq!(result.output(), "<MIDDLE>");
    }

    #[test]
    fn translation_capture_swap() {
        let swapper = Swapper::new(&[('x', "X"), ('y', "Y")]);

        let translation = UnresolvedTranslation::new(
            &[
                TranslationTarget::Literal("<".to_string()),
                TranslationTarget::Swap(swapper),
                TranslationTarget::Literal(">".to_string()),
            ],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("xyz", 8, 0);

        assert_eq!(result.input(), "xyz");
        assert_eq!(result.output(), "<XYz>");
    }

    #[test]
    fn translation_capture_unicode() {
        let translation = UnresolvedTranslation::new(
            &[
                TranslationTarget::Literal("<".to_string()),
                TranslationTarget::Capture,
                TranslationTarget::Literal(">".to_string()),
            ],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("café🚀🚀", 6, 0);

        assert_eq!(result.input(), "café🚀🚀");
        assert_eq!(result.length(), 6);
    }
}
