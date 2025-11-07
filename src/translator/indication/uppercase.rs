//! Uppercase Braille indication
//!
//! [`Indicator`] is a simple state machine to keep track of the state of a translation. As soon as
//! a character is encountered that is in the set of [`Indicator::uppercase_chars`] the state is
//! changed to [`State::Uppercase`]. When a character is encountered that is not in
//! [`Indicator::uppercase_chars`] the state is changed back to [`State::Default`].
//!
//! An indication for a start is only emitted if there is a [`Indicator::start_indicator`] and the
//! state is changed to `State::Uppercase`.
//!
//! Indication for the end is only emitted if there is a [`Indicator::end_indicator`] and the state
//! is changed to `State::Default`.

use crate::translator::indication::Indication;

use std::collections::HashSet;

/// Possible states for the [`Indicator`] state machine
#[derive(Debug, Clone)]
enum State {
    Default,
    Uppercase,
}

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder(Indicator);

impl IndicatorBuilder {
    pub fn new() -> Self {
        IndicatorBuilder(Indicator {
            state: State::Default,
            uppercase_chars: HashSet::default(),
            extra_uppercase_chars: HashSet::default(),
            start_indicator: None,
            end_indicator: None,
            terminating_chars: HashSet::default(),
        })
    }

    pub fn build(self) -> Indicator {
        self.0
    }

    pub fn start_indicator(mut self, s: &str) -> Self {
        self.0.start_indicator = Some(s.to_string());
        self
    }

    pub fn begcaps(mut self, s: &str) -> Self {
        self.0.start_indicator = Some(s.to_string());
        self
    }

    pub fn endcaps(mut self, s: &str) -> Self {
        self.0.end_indicator = Some(s.to_string());
        self
    }

    pub fn capsmodechars(mut self, s: &str) -> Self {
        self.0.extra_uppercase_chars = HashSet::from_iter(s.chars());
        self
    }

    pub fn uppercase_characters(mut self, chars: HashSet<char>) -> Self {
        self.0.uppercase_chars = chars;
        self
    }

    pub fn letter_characters(mut self, chars: HashSet<char>) -> Self {
        self.0.terminating_chars = chars;
        self
    }
}

/// A very simple state machine to keep track when an uppercase indication is
/// required
///
/// The state is changed to `State::Uppercase` as soon as a character is
/// encountered that is a member of the set of `uppercase_chars`. And if a
/// character is encountered that is neither in the set of `uppercase_chars` nor
/// in the set of `extra_uppercase_chars` the state is changed to
/// `State::Default`.
///
/// An indication for a start is emitted if the `start_indicator` is not None.
/// Indication for the end is emitted if `end_indicator` is not None and the
/// character encountered is in the set of terminating_chars.
#[derive(Debug, Clone)]
pub struct Indicator {
    state: State,
    /// The set of characters that will trigger a state change to the
    /// [State::Uppercase] mode
    uppercase_chars: HashSet<char>,
    /// The set of characters that will prevent a state change to the
    /// [State::Default] mode
    extra_uppercase_chars: HashSet<char>,
    /// The characters to indicate the start of a sequence of uppercase characters
    start_indicator: Option<String>,
    /// The characters to indicate the end of a sequence of uppercase characters
    end_indicator: Option<String>,
    /// The characters that will trigger an [`Indication::UppercaseEnd`] indication
    terminating_chars: HashSet<char>,
}

impl Indicator {
    /// The transition method of the uppercase indication state machine.
    ///
    /// Takes a string slice to examine the next character(s). Typically the
    /// indicator only looks at the next character, but there are cases where
    /// the indicator wants a bigger look-ahead, so we take a `&str` as input
    /// instead of just a `char`.
    ///
    /// # Arguments
    /// * `s` - A string slice containing the character(s) to process
    pub fn next(&mut self, s: &str) -> Option<Indication> {
        let c = s.chars().next();
        if self.start_indicator.is_none() || c.is_none() {
            return None;
        }
        match (&self.state, self.uppercase_chars.contains(&c.unwrap())) {
            (State::Default, true) => {
                self.state = State::Uppercase;
                Some(Indication::UppercaseStart)
            }
            (State::Uppercase, false) => {
                self.state = State::Default;
                // only indicate the end of uppercase if there is an end_indicator and the next char
                // is a letter
                if self.end_indicator.is_some() && self.terminating_chars.contains(&c.unwrap()) {
                    // FIXME: end indication should only occur within a word
                    Some(Indication::UppercaseEnd)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn start_indicator(&self) -> Option<String> {
        self.start_indicator.clone()
    }
    pub fn end_indicator(&self) -> Option<String> {
        self.end_indicator.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indicator_test() {
        let builder = IndicatorBuilder::new()
            .begcaps("⠸")
            .uppercase_characters(HashSet::from(['A', 'B', 'C']))
            .letter_characters(HashSet::from(['a', 'b', 'c']));
        let mut indicator = builder.build();
        assert_eq!(
            indicator.next("ABC ".into()),
            Some(Indication::UppercaseStart)
        );
        assert_eq!(indicator.next("BC ".into()), None);
        assert_eq!(indicator.next("C ".into()), None);
        assert_eq!(indicator.next(" ".into()), None);
        assert_eq!(indicator.next("".into()), None);
    }

    #[test]
    fn end_indication_test() {
        let builder = IndicatorBuilder::new()
            .begcaps("⠸")
            .endcaps("⠠")
            .uppercase_characters(HashSet::from(['A', 'B', 'C']))
            .letter_characters(HashSet::from(['a', 'b', 'c']));
        let mut indicator = builder.build();
        dbg!(&indicator);
        assert_eq!(
            indicator.next("ABCa".into()),
            Some(Indication::UppercaseStart)
        );
        assert_eq!(indicator.next("BCa".into()), None);
        assert_eq!(indicator.next("Ca".into()), None);
        assert_eq!(indicator.next("a".into()), Some(Indication::UppercaseEnd));
        assert_eq!(indicator.next("".into()), None);
    }
}
