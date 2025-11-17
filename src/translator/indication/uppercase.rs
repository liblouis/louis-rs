//! Uppercase Braille indication
//!
//! [`Indicator`] is a simple state machine to keep track of the state of a translation. As soon as
//! a character is encountered that is in the set of [`Indicator::uppercase_chars`] the state is
//! changed to [`State::UppercaseSingle`] or [`State::UppercaseMulti`]. When a character is
//! encountered that is not in [`Indicator::uppercase_chars`] the state is changed back to
//! [`State::Default`].
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
    UppercaseSingle,
    UppercaseMulti,
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
            start_letter_indicator: None,
            start_word_indicator: None,
            end_word_indicator: None,
        })
    }

    pub fn build(self) -> Indicator {
        self.0
    }

    pub fn capsletter(mut self, s: &str) -> Self {
        self.0.start_letter_indicator = Some(s.to_string());
        self
    }

    pub fn begcapsword(mut self, s: &str) -> Self {
        self.0.start_word_indicator = Some(s.to_string());
        self
    }

    pub fn endcapsword(mut self, s: &str) -> Self {
        self.0.end_word_indicator = Some(s.to_string());
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

#[derive(Debug, Clone)]
pub struct Indicator {
    state: State,
    uppercase_chars: HashSet<char>,
    extra_uppercase_chars: HashSet<char>,
    start_letter_indicator: Option<String>,
    start_word_indicator: Option<String>,
    end_word_indicator: Option<String>,
    start_indicator: Option<String>,
    end_indicator: Option<String>,
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
        let mut chars = s.chars();
        let c = chars.next();
        if !self.is_indicating() || c.is_none() {
            return None;
        }
        match (&self.state, self.uppercase_chars.contains(&c.unwrap())) {
            (State::Default, true) => {
                // OK, so we hit an uppercase char. Are we looking at just a
                // single char or are we looking at a whole capitalized word?
                if let Some(next_char) = chars.next()
                    && self.uppercase_chars.contains(&next_char)
                {
                    // well, at least two uppercase chars
                    // FIXME: Not sure if we can deduce that the whole word is
                    // uppercase by just looking at the first two characters
                    self.state = State::UppercaseMulti;
                    if self.start_word_indicator.is_some() {
                        Some(Indication::UppercaseStartWord)
                    } else if self.start_letter_indicator.is_some() {
                        Some(Indication::UppercaseStartLetter)
                    } else {
                        None
                    }
                } else {
                    // looks like it was just a single uppercase letter
                    self.state = State::UppercaseSingle;
                    if self.start_letter_indicator.is_some() {
                        Some(Indication::UppercaseStartLetter)
                    } else {
                        None
                    }
                }
            }
            (State::UppercaseSingle, false) => {
                self.state = State::Default;
                // no need to indicate anything
                None
            }
            (State::UppercaseMulti, false) => {
                self.state = State::Default;
                // only indicate the end of uppercase if there is an end_indicator and the next char
                // is a letter
                if self.end_word_indicator.is_some() && self.terminating_chars.contains(&c.unwrap())
                {
                    Some(Indication::UppercaseEndWord)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn is_indicating(&self) -> bool {
        self.start_letter_indicator.is_some()
            || self.start_word_indicator.is_some()
            || self.start_indicator.is_some()
    }

    pub fn start_indicator(&self) -> Option<String> {
        self.start_indicator.clone()
    }
    pub fn end_indicator(&self) -> Option<String> {
        self.end_indicator.clone()
    }
    pub fn start_letter_indicator(&self) -> Option<String> {
        self.start_letter_indicator.clone()
    }
    pub fn start_word_indicator(&self) -> Option<String> {
        self.start_word_indicator.clone()
    }
    pub fn end_word_indicator(&self) -> Option<String> {
        self.end_word_indicator.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indicator() {
        let builder = IndicatorBuilder::new()
            .capsletter("⠸")
            .uppercase_characters(HashSet::from(['A', 'B', 'C']))
            .letter_characters(HashSet::from(['a', 'b', 'c']));
        let mut indicator = builder.build();
        assert_eq!(
            indicator.next("Abc ".into()),
            Some(Indication::UppercaseStartLetter)
        );
        assert_eq!(indicator.next("bc ".into()), None);
        assert_eq!(indicator.next("c ".into()), None);
        assert_eq!(indicator.next(" ".into()), None);
        assert_eq!(indicator.next("".into()), None);
    }

    #[test]
    fn end_indication() {
        let builder = IndicatorBuilder::new()
            .begcapsword("⠸")
            .endcapsword("⠠")
            .uppercase_characters(HashSet::from(['A', 'B', 'C']))
            .letter_characters(HashSet::from(['a', 'b', 'c']));
        let mut indicator = builder.build();
        assert_eq!(
            indicator.next("ABCa".into()),
            Some(Indication::UppercaseStartWord)
        );
        assert_eq!(indicator.next("BCa".into()), None);
        assert_eq!(indicator.next("Ca".into()), None);
        assert_eq!(
            indicator.next("a".into()),
            Some(Indication::UppercaseEndWord)
        );
        assert_eq!(indicator.next("".into()), None);
    }
}
