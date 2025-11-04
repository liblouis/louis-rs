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
#[derive(Debug)]
enum State {
    Default,
    Numeric,
}

/// A very simple state machine to keep track when an numeric indication is
/// required
#[derive(Debug)]
pub struct NumericIndicator {
    state: State,
    /// Characters that will trigger a state change to the [State::Numeric] mode
    numeric_chars: HashSet<char>,
    /// The characters to indicate the start of a sequence of numerical characters
    start_indicator: Option<String>,
    /// The characters to indicate the end of a sequence of numerical characters
    end_indicator: Option<String>,
}

impl NumericIndicator {
    pub fn new(
        numeric_chars: HashSet<char>,
        start_indicator: Option<String>,
        end_indicator: Option<String>,
    ) -> Self {
        Self {
            state: State::Default,
            numeric_chars,
            start_indicator,
            end_indicator,
        }
    }

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indicator_test() {
        let numeric_chars: HashSet<char> = HashSet::from(['1', '2', '3']);
        let mut indicator =
            NumericIndicator::new(numeric_chars, Some('⠼'.to_string()), Some("⠰".to_string()));
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
