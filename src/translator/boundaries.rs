fn word_start(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
	(None, Some(c)) if c.is_alphabetic() => true,
	(Some(p), Some(c)) if c.is_alphabetic() => !p.is_alphabetic(),
	(_, _) => false
    }
}

fn word_end(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
	(Some(c), None) => c.is_alphabetic(),
	(Some(p), Some(c)) if p.is_alphabetic() => !c.is_alphabetic(),
	(_, _) => false
    }
}

fn number_start(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
	(None, Some(c)) => c.is_numeric(),
	(Some(p), Some(c)) if c.is_numeric() => !p.is_numeric(),
	(_, _) => false
    }
}

fn number_end(prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
	(Some(c), None) => c.is_numeric(),
	(Some(c1), Some(c2)) => c1.is_numeric() && !c2.is_numeric(),
	(_, _) => false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn word_start_test() {
        assert_eq!(true, word_start(Some(' '), Some('c')));
        assert_eq!(true, word_start(None, Some('c')));
        assert_eq!(false, word_start(Some('x'), Some('c')));
        assert_eq!(false, word_start(Some('c'), None));
        assert_eq!(false, word_start(None, None));
        assert_eq!(false, word_start(Some(' '), Some(' ')));
        assert_eq!(false, word_start(Some(';'), Some('.')));
        assert_eq!(false, word_start(Some('c'), Some(' ')));
    }

    #[test]
    fn word_end_test() {
        assert_eq!(true, word_end(Some('c'), Some(' ')));
        assert_eq!(true, word_end(Some('c'), None));
        assert_eq!(false, word_end(Some('x'), Some('c')));
        assert_eq!(false, word_end(None, Some('c')));
        assert_eq!(false, word_end(None, None));
        assert_eq!(false, word_end(Some(' '), Some(' ')));
        assert_eq!(false, word_end(Some(';'), Some('.')));
        assert_eq!(false, word_end(Some(' '), Some('c')));
    }

    #[test]
    fn number_start_test() {
        assert_eq!(true, number_start(Some(' '), Some('1')));
        assert_eq!(true, number_start(None, Some('1')));
        assert_eq!(true, number_start(Some('x'), Some('1')));
        assert_eq!(true, number_start(Some(','), Some('1')));
        assert_eq!(false, number_start(Some('c'), None));
        assert_eq!(false, number_start(None, None));
        assert_eq!(false, number_start(Some(' '), Some(' ')));
        assert_eq!(false, number_start(Some(';'), Some('.')));
        assert_eq!(false, number_start(Some('1'), Some(' ')));
    }

    #[test]
    fn number_end_test() {
        assert_eq!(true, number_end(Some('1'), Some(' ')));
        assert_eq!(true, number_end(Some('1'), None));
        assert_eq!(true, number_end(Some('1'), Some('c')));
        assert_eq!(false, number_end(None, Some('c')));
        assert_eq!(false, number_end(None, None));
        assert_eq!(false, number_end(Some(' '), Some(' ')));
        assert_eq!(false, number_end(Some(';'), Some('.')));
        assert_eq!(false, number_end(Some(' '), Some('c')));
    }

}
