//! Braille indication
//!
//! Braille indicators are dot patterns which are inserted into the braille text
//! to indicate such things as capitalization, italic type, computer braille,
//! etc.
//!
//! Braille indication is handled with the help of a number of simple state
//! machines that keep track in which state a translation currently is. When
//! given the next pending character(s) to translate, they will notify the
//! caller of a state change by returning an [Indication].

//! There are multiple state machines to keep track of different indication
//! requirements:
//! * [NumericIndicator]: knowns whether the translation is in numeric mode
use std::collections::HashSet;

/// Possible indication events that the indicator state machine(s) support
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Indication {
    NumericStart,
    NumericEnd,
    CapitalizationStart,
    CapitalizationEnd,
    EmphasisStart,
    EmphasisEnd,
}

/// Possible states for the [NumericIndicator] state machine
#[derive(Debug, Clone)]
enum State {
    Default,
    Numeric,
}

#[derive(Debug)]
pub struct NumericIndicatorBuilder(NumericIndicator);

impl NumericIndicatorBuilder {
    pub fn new() -> Self {
        NumericIndicatorBuilder(NumericIndicator {
            state: State::Default,
            numeric_chars: HashSet::default(),
            extra_numeric_chars: HashSet::default(),
            start_indicator: None,
            end_indicator: None,
            terminating_chars: HashSet::default(),
        })
    }

    pub fn build(self) -> NumericIndicator {
        self.0
    }

    pub fn start_indicator(mut self, s: &str) -> Self {
        self.0.start_indicator = Some(s.to_string());
        self
    }

    pub fn numsign(mut self, s: &str) -> Self {
        self.0.start_indicator = Some(s.to_string());
        self
    }

    pub fn nonumsign(mut self, s: &str) -> Self {
        self.0.end_indicator = Some(s.to_string());
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
pub struct NumericIndicator {
    state: State,
    /// Characters that will trigger a state change to the [State::Numeric] mode
    numeric_chars: HashSet<char>,
    /// Characters that will prevent a state change to the [State::Default] mode
    extra_numeric_chars: HashSet<char>,
    /// The characters to indicate the start of a sequence of numerical characters
    start_indicator: Option<String>,
    /// The characters to indicate the end of a sequence of numerical characters
    end_indicator: Option<String>,
    /// Characters that will trigger an [`Indication::NumericEndend`] indication
    terminating_chars: HashSet<char>,
}

impl NumericIndicator {
    /// The transition method of the numeric indication state machine.
    ///
    /// Takes a string slice to examine the next character(s). Typically the
    /// indicator only looks at the next character, but there are cases where
    /// the indicator wants a bigger look-ahead, so we take a `&str` as input
    /// instead of just a `char`. Returns an [Indication] when transitioning
    /// between numeric and non-numeric states or `None` when no state change
    /// occurs (or the table contains no `numsign` opcode).
    ///
    /// # Arguments
    /// * `s` - A string slice containing the character(s) to process
    pub fn next(&mut self, s: &str) -> Option<Indication> {
        let c = s.chars().next();
        if self.start_indicator.is_none() || c.is_none() {
            return None;
        }
        match (&self.state, self.numeric_chars.contains(&c.unwrap())) {
            (State::Default, true) => {
                self.state = State::Numeric;
                Some(Indication::NumericStart)
            }
            (State::Numeric, false) => {
                self.state = State::Default;
                // only indicate the end of a number if there is an end_indicator
                if self.end_indicator.is_some() {
                    // FIXME: end indication should only occur within a word
                    Some(Indication::NumericEnd)
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
        let numeric_chars: HashSet<char> = HashSet::from(['1', '2', '3']);
        let builder = NumericIndicatorBuilder::new()
            .numeric_characters(numeric_chars)
            .numsign("⠼")
            .nonumsign("⠰");
        let mut indicator = builder.build();
        assert_eq!(indicator.next("ab12 a".into()), None);
        assert_eq!(indicator.next("b12 a".into()), None);
        assert_eq!(
            indicator.next("12 a".into()),
            Some(Indication::NumericStart)
        );
        assert_eq!(indicator.next("2 a".into()), None);
        assert_eq!(indicator.next(" a".into()), Some(Indication::NumericEnd));
        assert_eq!(indicator.next("a".into()), None);
        assert_eq!(indicator.next("".into()), None);
    }
}
