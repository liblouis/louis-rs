use crate::{
    parser::{AnchoredRule, Precedence},
    translator::swap::Swapper,
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
pub enum TranslateTo {
    /// Translate to a literal string
    Literal(String),
    /// Translate to the result of a capture
    Capture { before: String, after: String },
    /// Translate to the result of a capture but also apply the mapping defined in the Swappery
    Swap {
        before: String,
        after: String,
        swapper: Swapper,
    },
}

impl Default for TranslateTo {
    fn default() -> Self {
        TranslateTo::Literal("".to_string())
    }
}

impl std::fmt::Display for TranslateTo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranslateTo::Literal(s) => write!(f, "{}", s),
            _ => todo!(),
        }
    }
}

impl TranslateTo {
    pub fn resolve(self, capture: &str) -> Self {
        match self {
            TranslateTo::Literal(_) => self,
            TranslateTo::Capture { before, after } => {
                TranslateTo::Literal(format!("{}{}{}", before, capture, after))
            }
            TranslateTo::Swap {
                before,
                after,
                swapper,
            } => TranslateTo::Literal(format!("{}{}{}", before, swapper.swap(capture), after)),
        }
    }
}

/// The basic unit of a translation. Maps an `input` string to an `output` which is typically also a
/// string.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Translation {
    /// Input string to be translated
    input: String,
    /// The translation of `input`, typically Unicode braille. In the case of back-translation the
    /// `input` contains Unicode braille and the output plain text.
    output: TranslateTo,
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
    /// Which translation rule was the cause for this translation
    origin: Option<AnchoredRule>,
}

impl Translation {
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
            output: TranslateTo::Literal(output.to_string()),
            weight,
            length: input.chars().count(),
            offset: 0,
            precedence: Precedence::Default,
            stage,
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

    /// Set the `weight` of a translation if `offset` is greater than 0, otherwise return the
    /// translation unchanged.
    pub fn with_weight_if_offset(self, weight: usize, offset: usize) -> Self {
        if offset > 0 {
            Self { weight, ..self }
        } else {
            self
        }
    }

    /// Adapt a translation based on a captured string.
    ///
    /// Update the `input` with the given `capture` if it is non-empty. The `output` can also be set
    /// if it has references to the capture.
    ///
    /// This is used in rules such as `context` and the multipass rules.
    pub fn with_capture(self, capture: String) -> Self {
        if !capture.is_empty() {
            let output = self.output.resolve(&capture);
            let length = capture.chars().count();
            Self {
                input: capture,
                length,
                output,
                ..self
            }
        } else {
            self
        }
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
            TranslateTo::Literal("foo".to_string()).resolve("bar"),
            TranslateTo::Literal("foo".to_string())
        );
    }

    #[test]
    fn resolve_capture() {
        let resolved = TranslateTo::Capture {
            before: "<".to_string(),
            after: ">".to_string(),
        }
        .resolve("CAPTURED");
        assert_eq!(resolved, TranslateTo::Literal("<CAPTURED>".to_string()));
    }

    #[test]
    fn resolve_capture_empty_before_after() {
        let resolved = TranslateTo::Capture {
            before: "".to_string(),
            after: "".to_string(),
        }
        .resolve("test");
        assert_eq!(resolved, TranslateTo::Literal("test".to_string()));
    }

    #[test]
    fn resolve_swap() {
        let swapper = Swapper::new(&[('a', "A"), ('b', "B"), ('c', "C")]);

        let resolved = TranslateTo::Swap {
            before: "<<".to_string(),
            after: ">>".to_string(),
            swapper,
        }
        .resolve("abc");
        assert_eq!(resolved, TranslateTo::Literal("<<ABC>>".to_string()));
    }

    #[test]
    fn translation_capture_empty() {
        let translation = Translation::new("input", "output", 5, TranslationStage::Main, None);
        let result = translation.with_capture("".to_string());

        // Should be unchanged when capture is empty
        assert_eq!(result.input(), "input");
        assert_eq!(result.output(), "output");
    }

    #[test]
    fn translation_capture_literal() {
        let translation = Translation::new("input", "output", 5, TranslationStage::Main, None)
            .with_capture("captured".to_string());

        assert_eq!(translation.input(), "captured");
        assert_eq!(translation.output(), "output"); // Literal output unchanged
    }

    #[test]
    fn translation_capture_capture() {
        let mut translation = Translation::new("input", "output", 5, TranslationStage::Main, None);
        translation.output = TranslateTo::Capture {
            before: "<".to_string(),
            after: ">".to_string(),
        };

        let translation = translation.with_capture("MIDDLE".to_string());

        assert_eq!(translation.input(), "MIDDLE");
        assert_eq!(translation.output(), "<MIDDLE>");
    }

    #[test]
    fn translation_capture_swap() {
        let swapper = Swapper::new(&[('x', "X"), ('y', "Y")]);

        let mut translation = Translation::new("input", "output", 5, TranslationStage::Main, None);
        translation.output = TranslateTo::Swap {
            before: "[".to_string(),
            after: "]".to_string(),
            swapper,
        };

        let result = translation.with_capture("xyz".to_string());

        assert_eq!(result.input(), "xyz");
        assert_eq!(result.output(), "[XYz]");
    }

    #[test]
    fn translation_capture_unicode() {
        let translation = Translation::new("input", "output", 5, TranslationStage::Main, None);
        let result = translation.with_capture("café🚀🚀".to_string());

        assert_eq!(result.input(), "café🚀🚀");
        assert_eq!(result.length(), 6);
    }
}
