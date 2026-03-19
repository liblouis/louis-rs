//! Helper functions to define boundaries

use crate::parser::CharacterClasses;

/// Return true if a character is at the beginning of a word
pub fn word_start(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (None, Some(c)) => ctx.is_word(c),
        (Some(p), Some(c)) if ctx.is_word(c) => !ctx.is_word(p),
        (_, _) => false,
    }
}

/// Return true if a character is at the end of a word
pub fn word_end(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c), None) => ctx.is_word(c),
        (Some(p), Some(c)) if ctx.is_word(p) => !ctx.is_word(c),
        (_, _) => false,
    }
}

/// Return true if a character is at the beginning of punctuation
pub fn punctuation_start(
    ctx: &CharacterClasses,
    prev: Option<char>,
    current: Option<char>,
) -> bool {
    match (prev, current) {
        (None, Some(c)) => ctx.is_punctuation(c),
        (Some(p), Some(c)) if ctx.is_punctuation(c) => ctx.is_whitespace(p),
        (_, _) => false,
    }
}

/// Return true if a character is at the end of a punctuation
pub fn punctuation_end(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c), None) => ctx.is_punctuation(c),
        (Some(p), Some(c)) if ctx.is_punctuation(p) => ctx.is_whitespace(c),
        (_, _) => false,
    }
}

/// Return true if a character is at the boundary between a word and punctuation
pub fn word_punctuation(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c1), Some(c2)) => ctx.is_word(c1) && ctx.is_punctuation(c2),
        (_, _) => false,
    }
}

/// Return true if a character is at the boundary between punctuation and a word
pub fn punctuation_word(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c1), Some(c2)) => ctx.is_punctuation(c1) && ctx.is_word(c2),
        (_, _) => false,
    }
}

fn number_start(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (None, Some(c)) => ctx.is_numeric(c),
        (Some(p), Some(c)) if ctx.is_numeric(c) => !ctx.is_numeric(p),
        (_, _) => false,
    }
}

fn number_end(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c), None) => ctx.is_numeric(c),
        (Some(c1), Some(c2)) => ctx.is_numeric(c1) && !ctx.is_numeric(c2),
        (_, _) => false,
    }
}

/// Return true if a character is at the boundary between a word and a
/// number
pub fn word_number(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c1), Some(c2)) => ctx.is_word(c1) && ctx.is_numeric(c2),
        (_, _) => false,
    }
}

/// Return true if a character is at the boundary between a number and a
/// word
pub fn number_word(ctx: &CharacterClasses, prev: Option<char>, current: Option<char>) -> bool {
    match (prev, current) {
        (Some(c1), Some(c2)) => ctx.is_numeric(c1) && ctx.is_word(c2),
        (_, _) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::CharacterClass;

    use super::*;

    #[test]
    fn test_word_start() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['c', 'x']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(word_start(&ctx, Some(' '), Some('c')));
        assert!(word_start(&ctx, None, Some('c')));
        assert!(!word_start(&ctx, Some('x'), Some('c')));
        assert!(!word_start(&ctx, Some('c'), None));
        assert!(!word_start(&ctx, None, None));
        assert!(!word_start(&ctx, Some(' '), Some(' ')));
        assert!(!word_start(&ctx, Some(';'), Some('.')));
        assert!(!word_start(&ctx, Some('c'), Some(' ')));
    }

    #[test]
    fn test_word_end() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &[';']),
            (CharacterClass::Letter, &['c', 'x']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(word_end(&ctx, Some('c'), Some(' ')));
        assert!(word_end(&ctx, Some('c'), None));
        assert!(word_end(&ctx, Some('c'), Some('.')));
        assert!(!word_end(&ctx, Some('x'), Some('c')));
        assert!(!word_end(&ctx, None, Some('c')));
        assert!(!word_end(&ctx, None, None));
        assert!(!word_end(&ctx, Some(' '), Some(' ')));
        assert!(!word_end(&ctx, Some(';'), Some('.')));
        assert!(!word_end(&ctx, Some(' '), Some('c')));
    }
    #[test]
    fn test_number_start() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &[',', '.', ';']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['x']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(number_start(&ctx, Some(' '), Some('1')));
        assert!(number_start(&ctx, None, Some('1')));
        assert!(number_start(&ctx, Some('x'), Some('1')));
        assert!(number_start(&ctx, Some(','), Some('1')));
        assert!(!number_start(&ctx, Some('c'), None));
        assert!(!number_start(&ctx, None, None));
        assert!(!number_start(&ctx, Some(' '), Some(' ')));
        assert!(!number_start(&ctx, Some(';'), Some('.')));
        assert!(!number_start(&ctx, Some('1'), Some(' ')));
    }

    #[test]
    fn test_number_end() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &[',', '.', ';']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['c']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(number_end(&ctx, Some('1'), Some(' ')));
        assert!(number_end(&ctx, Some('1'), None));
        assert!(number_end(&ctx, Some('1'), Some('c')));
        assert!(!number_end(&ctx, None, Some('c')));
        assert!(!number_end(&ctx, None, None));
        assert!(!number_end(&ctx, Some(' '), Some(' ')));
        assert!(!number_end(&ctx, Some(';'), Some('.')));
    }

    #[test]
    fn test_word_number() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(word_number(&ctx, Some('a'), Some('1')));
        // TODO: '#' is currently not recognized as a word
        //        assert!(word_number(Some('#'), Some('2')));
        assert!(!word_number(&ctx, Some('1'), Some('1')));
        assert!(!word_number(&ctx, Some('1'), Some(' ')));
        assert!(!word_number(&ctx, Some('1'), Some('a')));
        assert!(!word_number(&ctx, Some(' '), Some('1')));
        assert!(!word_number(&ctx, Some('a'), Some('a')));
        assert!(!word_number(&ctx, Some(' '), Some(' ')));
    }

    #[test]
    fn test_number_word() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(number_word(&ctx, Some('1'), Some('a')));
        // TODO: '#' and '$' are currently not recognized as a word
        //        assert!(number_word(Some('2'), Some('#')));
        //        assert!(number_word(Some('2'), Some('$')));
        assert!(!number_word(&ctx, Some('1'), Some('1')));
        assert!(!number_word(&ctx, Some('1'), Some(' ')));
        assert!(!number_word(&ctx, Some('a'), Some('1')));
        assert!(!number_word(&ctx, Some(' '), Some('1')));
        assert!(!number_word(&ctx, Some('a'), Some('a')));
        assert!(!number_word(&ctx, Some(' '), Some(' ')));
        assert!(!number_word(&ctx, None, Some('a')));
    }

    #[test]
    fn test_punctuation_start() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(punctuation_start(&ctx, Some(' '), Some('(')));
        assert!(punctuation_start(&ctx, Some(' '), Some(')')));
        assert!(punctuation_start(&ctx, None, Some('(')));
        assert!(punctuation_start(&ctx, None, Some(')')));
        assert!(!punctuation_start(&ctx, Some('('), Some('(')));
        assert!(!punctuation_start(&ctx, Some(')'), Some('(')));
        assert!(!punctuation_start(&ctx, Some('a'), Some('(')));
        assert!(!punctuation_start(&ctx, Some('1'), Some('(')));
        assert!(!punctuation_start(&ctx, Some('('), Some(' ')));
        assert!(!punctuation_start(&ctx, Some('('), Some('a')));
        assert!(!punctuation_start(&ctx, Some('('), Some('1')));
        assert!(!punctuation_start(&ctx, Some('('), Some(')')));
    }

    #[test]
    fn test_punctuation_end() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(punctuation_end(&ctx, Some(')'), Some(' ')));
        assert!(punctuation_end(&ctx, Some('('), Some(' ')));
        assert!(punctuation_end(&ctx, Some('('), None));
        assert!(punctuation_end(&ctx, Some(')'), None));
        assert!(!punctuation_end(&ctx, Some('('), Some('(')));
        assert!(!punctuation_end(&ctx, Some(')'), Some('(')));
        assert!(!punctuation_end(&ctx, Some('a'), Some('(')));
        assert!(!punctuation_end(&ctx, Some('1'), Some('(')));
        assert!(!punctuation_end(&ctx, Some(' '), Some(')')));
        assert!(!punctuation_end(&ctx, Some('('), Some('a')));
        assert!(!punctuation_end(&ctx, Some('('), Some('1')));
        assert!(!punctuation_end(&ctx, Some('('), Some(')')));
    }

    #[test]
    fn test_word_punctuation() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(word_punctuation(&ctx, Some('a'), Some(')')));
        assert!(!word_punctuation(&ctx, Some(' '), Some(')')));
        assert!(!word_punctuation(&ctx, Some('('), None));
        assert!(!word_punctuation(&ctx, Some(')'), None));
        assert!(!word_punctuation(&ctx, Some('('), Some('(')));
        assert!(!word_punctuation(&ctx, Some(')'), Some('(')));
        assert!(!word_punctuation(&ctx, Some('1'), Some('(')));
        assert!(!word_punctuation(&ctx, Some('('), Some('a')));
        assert!(!word_punctuation(&ctx, Some('('), Some('1')));
        assert!(!word_punctuation(&ctx, Some('('), Some(')')));
    }

    #[test]
    fn test_punctuation_word() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Punctuation, &['(', ')']),
            (CharacterClass::Space, &[' ']),
        ]);
        assert!(punctuation_word(&ctx, Some('('), Some('a')));
        assert!(punctuation_word(&ctx, Some(')'), Some('a')));
        assert!(!punctuation_word(&ctx, Some(' '), Some('a')));
        assert!(!punctuation_word(&ctx, Some('('), None));
        assert!(!punctuation_word(&ctx, Some(')'), None));
        assert!(!punctuation_word(&ctx, None, Some('(')));
        assert!(!punctuation_word(&ctx, None, Some(')')));
        assert!(!punctuation_word(&ctx, Some('('), Some('(')));
        assert!(!punctuation_word(&ctx, Some(')'), Some('(')));
        assert!(!punctuation_word(&ctx, Some('1'), Some('(')));
        assert!(!punctuation_word(&ctx, Some('('), Some('1')));
        assert!(!punctuation_word(&ctx, Some('('), Some(')')));
    }
}
