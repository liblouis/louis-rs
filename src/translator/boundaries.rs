pub fn word_start(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (None, Some(c)) if c.is_alphabetic() => true,
        (Some(p), Some(c)) if c.is_alphabetic() => !p.is_alphabetic(),
        (_, _) => false,
    }
}

pub fn word_end(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c), None) => c.is_alphabetic(),
        (Some(p), Some(c)) if p.is_alphabetic() => !c.is_alphabetic(),
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
}
