//! Helper functions to define boundaries
//!
//! For simplicity sake for now this module defines boundaries based
//! on Unicode properties. The original liblouis uses the character
//! definitions instead.

/// Return true if a character is part of a word
fn is_word(c: char) -> bool {
    // TODO: the definition of what really constitutes a word is a bit
    // hand-wavy here. Maybe it should be something along the lines of
    //    !c.is_numeric() && !c.is_whitespace() && !c.is_control()
    c.is_alphabetic()
}

/// Return true if a character is at the beginning of a word
pub fn word_start(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (None, Some(c)) => is_word(c),
        (Some(p), Some(c)) if is_word(c) => !is_word(p),
        (_, _) => false,
    }
}

/// Return true if a character is at the end of a word
pub fn word_end(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c), None) => is_word(c),
        (Some(p), Some(c)) if is_word(p) => !is_word(c),
        (_, _) => false,
    }
}

fn number_start(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (None, Some(c)) => c.is_numeric(),
        (Some(p), Some(c)) if c.is_numeric() => !p.is_numeric(),
        (_, _) => false,
    }
}

fn number_end(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c), None) => c.is_numeric(),
        (Some(c1), Some(c2)) => c1.is_numeric() && !c2.is_numeric(),
        (_, _) => false,
    }
}

/// Return true if a character is at the boundary between a word and a
/// number
pub fn word_number(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c1), Some(c2)) => is_word(c1) && c2.is_numeric(),
        (_, _) => false,
    }
}

/// Return true if a character is at the boundary between a number and a
/// word
pub fn number_word(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c1), Some(c2)) => c1.is_numeric() && is_word(c2),
        (_, _) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn word_start_test() {
        assert!(word_start(Some(' '), Some('c')));
        assert!(word_start(None, Some('c')));
        assert!(!word_start(Some('x'), Some('c')));
        assert!(!word_start(Some('c'), None));
        assert!(!word_start(None, None));
        assert!(!word_start(Some(' '), Some(' ')));
        assert!(!word_start(Some(';'), Some('.')));
        assert!(!word_start(Some('c'), Some(' ')));
    }

    #[test]
    fn word_end_test() {
        assert!(word_end(Some('c'), Some(' ')));
        assert!(word_end(Some('c'), None));
        assert!(word_end(Some('c'), Some('.')));
        assert!(!word_end(Some('x'), Some('c')));
        assert!(!word_end(None, Some('c')));
        assert!(!word_end(None, None));
        assert!(!word_end(Some(' '), Some(' ')));
        assert!(!word_end(Some(';'), Some('.')));
        assert!(!word_end(Some(' '), Some('c')));
    }

    #[test]
    fn number_start_test() {
        assert!(number_start(Some(' '), Some('1')));
        assert!(number_start(None, Some('1')));
        assert!(number_start(Some('x'), Some('1')));
        assert!(number_start(Some(','), Some('1')));
        assert!(!number_start(Some('c'), None));
        assert!(!number_start(None, None));
        assert!(!number_start(Some(' '), Some(' ')));
        assert!(!number_start(Some(';'), Some('.')));
        assert!(!number_start(Some('1'), Some(' ')));
    }

    #[test]
    fn number_end_test() {
        assert!(number_end(Some('1'), Some(' ')));
        assert!(number_end(Some('1'), None));
        assert!(number_end(Some('1'), Some('c')));
        assert!(!number_end(None, Some('c')));
        assert!(!number_end(None, None));
        assert!(!number_end(Some(' '), Some(' ')));
        assert!(!number_end(Some(';'), Some('.')));
        assert!(!number_end(Some(' '), Some('c')));
    }

    #[test]
    fn word_number_test() {
        assert!(word_number(Some('a'), Some('1')));
        // TODO: '#' is currently not recognized as a word
        //        assert!(word_number(Some('#'), Some('2')));
        assert!(!word_number(Some('1'), Some('1')));
        assert!(!word_number(Some('1'), Some(' ')));
        assert!(!word_number(Some('1'), Some('a')));
        assert!(!word_number(Some(' '), Some('1')));
        assert!(!word_number(Some('a'), Some('a')));
        assert!(!word_number(Some(' '), Some(' ')));
    }

    #[test]
    fn number_word_test() {
        assert!(number_word(Some('1'), Some('a')));
        // TODO: '#' and '$' are currently not recognized as a word
        //        assert!(number_word(Some('2'), Some('#')));
        //        assert!(number_word(Some('2'), Some('$')));
        assert!(!number_word(Some('1'), Some('1')));
        assert!(!number_word(Some('1'), Some(' ')));
        assert!(!number_word(Some('a'), Some('1')));
        assert!(!number_word(Some(' '), Some('1')));
        assert!(!number_word(Some('a'), Some('a')));
        assert!(!number_word(Some(' '), Some(' ')));
        assert!(!number_word(None, Some('a')));
    }
}
