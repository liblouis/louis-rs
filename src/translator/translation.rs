use crate::parser::{AnchoredRule, Precedence};

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

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Translation {
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
    precedence: Precedence,
    stage: TranslationStage,
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
            output: output.to_string(),
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
        self.output.clone()
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
            Self {
                input: capture,
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
