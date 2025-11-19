//! Numeric Braille indication
//!
//! [`Indicator`] is a simple state machine to keep track of the state of a translation. As soon
//! as a character is encountered that is in the set of [`Indicator::numeric_chars`] the state is
//! changed to [`State::Numeric`]. When a character is encountered that is neither in
//! [`Indicator::numeric_chars`] nor in [`Indicator::extra_numeric_chars`] the state is changed back
//! to [`State::Default`]
//!
//! An indication for a start is only emitted if there is a [`Indicator::start_translation`] and the
//! state is changed to `State::Numeric`.
//!
//! Indication for the end is only emitted if there is a [`Indicator::end_translation`], the state is
//! changed to `State::Default` and the character encountered is in the set of
//! [`Indicator::terminating_chars`].

use crate::{parser::AnchoredRule, translator::Translation};

use std::collections::HashSet;

/// Possible states for the [`Indicator`] state machine
#[derive(Debug, Clone)]
enum State {
    Default,
    Numeric,
}

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder(Indicator);

impl IndicatorBuilder {
    pub fn new() -> Self {
        IndicatorBuilder(Indicator {
            state: State::Default,
            numeric_chars: HashSet::default(),
            extra_numeric_chars: HashSet::default(),
            start_translation: None,
            end_translation: None,
            terminating_chars: HashSet::default(),
        })
    }

    pub fn build(self) -> Indicator {
        self.0
    }

    pub fn numsign(mut self, s: &str, origin: &AnchoredRule) -> Self {
        self.0.start_translation = Some(Translation::new(
            "".to_string(),
            s.to_string(),
            1,
            origin.clone(),
        ));
        self
    }

    pub fn nonumsign(mut self, s: &str, origin: &AnchoredRule) -> Self {
        self.0.end_translation = Some(Translation::new(
            "".to_string(),
            s.to_string(),
            1,
            origin.clone(),
        ));
        self
    }

    pub fn numericnocontchars(mut self, s: &str) -> Self {
        self.0.terminating_chars = HashSet::from_iter(s.chars());
        self
    }

    pub fn numericmodechars(mut self, s: &str) -> Self {
        self.0.extra_numeric_chars = HashSet::from_iter(s.chars());
        self
    }

    pub fn numeric_characters(mut self, chars: HashSet<char>) -> Self {
        self.0.numeric_chars = chars;
        self
    }
}

/// A very simple state machine to keep track when an numeric indication is
/// required
///
/// The state is changed to `State::Numeric` as soon as a character is
/// encountered that is a member of the set of `numeric_chars`. And if a
/// character is encountered that is neither in the set of `numeric_chars` nor
/// in the set of `extra_numeric_chars` the state is changed to
/// `State::Default`.
///
/// An indication for a start is emitted if the `start_indicator` is not None.
/// Indication for the end is emitted if `end_indicator` is not None and the
/// character encountered is in the set of terminating_chars.
#[derive(Debug, Clone)]
pub struct Indicator {
    state: State,
    /// The set of characters that will trigger a state change to the
    /// [State::Numeric] mode
    numeric_chars: HashSet<char>,
    /// The set of characters that will prevent a state change to the
    /// [State::Default] mode
    extra_numeric_chars: HashSet<char>,
    /// The translation to indicate the start of a sequence of numerical characters
    start_translation: Option<Translation>,
    /// The characters to indicate the end of a sequence of numerical characters
    end_translation: Option<Translation>,
    /// The characters that will trigger the indication of the end of a sequence of numerical characters
    terminating_chars: HashSet<char>,
}

impl Indicator {
    /// The transition method of the numeric indication state machine.
    ///
    /// Takes a string slice to examine the next character(s). Typically the
    /// indicator only looks at the next character, but there are cases where
    /// the indicator wants a bigger look-ahead, so we take a `&str` as input
    /// instead of just a `char`. Returns a [`Translation`] when transitioning
    /// between numeric and non-numeric states or `None` when no state change
    /// occurs (or the table contains no `numsign` opcode).
    ///
    /// # Arguments
    /// * `s` - A string slice containing the character(s) to process
    pub fn next(&mut self, s: &str) -> Option<Translation> {
        let c = s.chars().next();
        if self.start_translation.is_none() || c.is_none() {
            return None;
        }
        match (&self.state, self.numeric_chars.contains(&c.unwrap())) {
            (State::Default, true) => {
                self.state = State::Numeric;
                self.start_translation.clone()
            }
            (State::Numeric, false) => {
                self.state = State::Default;
                // only indicate the end of a number if the character is contained in
                // terminating_chars
                if self.terminating_chars.contains(&c.unwrap()) {
                    // FIXME: end indication should only occur within a word
                    self.end_translation.clone()
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::RuleParser;

    use super::*;

    #[test]
    fn indicator() {
        let rule = RuleParser::new("numsign 3456").rule().unwrap();
        let rule = AnchoredRule::new(rule, None, 0);
        let numeric_chars: HashSet<char> = HashSet::from(['1', '2', '3']);
        let builder = IndicatorBuilder::new()
            .numeric_characters(numeric_chars)
            .numsign("⠼", &rule)
            .nonumsign("⠰", &rule)
            .numericnocontchars("abc");
        let mut indicator = builder.build();
        assert_eq!(indicator.next("ab12 a".into()), None);
        assert_eq!(indicator.next("b12 a".into()), None);
        assert_eq!(
            indicator.next("12 a".into()),
            Some(Translation::new("".to_string(), "⠼".to_string(), 1, rule))
        );
        assert_eq!(indicator.next("2 a".into()), None);
        assert_eq!(indicator.next(" a".into()), None);
        assert_eq!(indicator.next("a".into()), None);
        assert_eq!(indicator.next("".into()), None);
    }

    #[test]
    fn end_indication() {
        let rule = RuleParser::new("numsign 3456").rule().unwrap();
        let numsign_rule = AnchoredRule::new(rule, None, 0);
        let rule = RuleParser::new("nonumsign 56").rule().unwrap();
        let nonumsign_rule = AnchoredRule::new(rule, None, 0);
        let numeric_chars: HashSet<char> = HashSet::from(['1', '2', '3']);
        let builder = IndicatorBuilder::new()
            .numeric_characters(numeric_chars)
            .numsign("⠼", &numsign_rule)
            .nonumsign("⠰", &nonumsign_rule)
            .numericnocontchars("abc");
        let mut indicator = builder.build();
        assert_eq!(indicator.next("ab12a".into()), None);
        assert_eq!(indicator.next("b12a".into()), None);
        assert_eq!(
            indicator.next("12a".into()),
            Some(Translation::new(
                "".to_string(),
                "⠼".to_string(),
                1,
                numsign_rule
            ))
        );
        assert_eq!(indicator.next("2a".into()), None);
        assert_eq!(
            indicator.next("a".into()),
            Some(Translation::new(
                "".to_string(),
                "⠰".to_string(),
                1,
                nonumsign_rule
            ))
        );
        assert_eq!(indicator.next("".into()), None);
    }
}
