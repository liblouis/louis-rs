use std::collections::HashSet;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Invalid braille {character:?}")]
    InvalidBraille { character: Option<char> },
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum BrailleDot {
    DOT0,
    DOT1,
    DOT2,
    DOT3,
    DOT4,
    DOT5,
    DOT6,
    DOT7,
    DOT8,
    DOT9,
    DOTA,
    DOTB,
    DOTC,
    DOTD,
    DOTE,
    DOTF,
}

pub type BrailleChar = HashSet<BrailleDot>;
pub type BrailleChars = Vec<BrailleChar>;

// fn has_virtual_dots(char: &BrailleChar) -> bool {
//     let virtual_dots = HashSet::from([
// 	BrailleDot::DOT9,
// 	BrailleDot::DOTA,
// 	BrailleDot::DOTB,
// 	BrailleDot::DOTC,
// 	BrailleDot::DOTD,
// 	BrailleDot::DOTE,
// 	BrailleDot::DOTF]);
//     !virtual_dots.intersection(char)
// 	.collect::<HashSet<_>>()
// 	.is_empty()
// }

fn char_to_dot(char: char) -> Result<BrailleDot, ParseError> {
    match char {
        '0' => Ok(BrailleDot::DOT0),
        '1' => Ok(BrailleDot::DOT1),
        '2' => Ok(BrailleDot::DOT2),
        '3' => Ok(BrailleDot::DOT3),
        '4' => Ok(BrailleDot::DOT4),
        '5' => Ok(BrailleDot::DOT5),
        '6' => Ok(BrailleDot::DOT6),
        '7' => Ok(BrailleDot::DOT7),
        '8' => Ok(BrailleDot::DOT8),
        '9' => Ok(BrailleDot::DOT9),
        'a' => Ok(BrailleDot::DOTA),
        'b' => Ok(BrailleDot::DOTB),
        'c' => Ok(BrailleDot::DOTC),
        'd' => Ok(BrailleDot::DOTD),
        'e' => Ok(BrailleDot::DOTE),
        'f' => Ok(BrailleDot::DOTF),
        invalid => Err(ParseError::InvalidBraille { character: invalid }),
    }
}

pub fn chars_to_dots(chars: &str) -> Result<BrailleChar, ParseError> {
    if chars.is_empty() {
        Err(ParseError::InvalidBraille { character: None })
    } else {
        chars.chars().map(char_to_dot).collect()
    }
}

pub fn braille_chars(chars: &str) -> Result<BrailleChars, ParseError> {
    chars.split('-').map(chars_to_dots).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chars_to_dots_test() {
        assert_eq!(
            chars_to_dots("123"),
            Ok(HashSet::from([
                BrailleDot::Dot1,
                BrailleDot::Dot2,
                BrailleDot::Dot3
            ]))
        );
        assert_eq!(
            chars_to_dots("1a"),
            Ok(HashSet::from([BrailleDot::Dot1, BrailleDot::DotA,]))
        );
        assert_eq!(chars_to_dots("a"), Ok(HashSet::from([BrailleDot::DotA,])));
        assert_eq!(
            chars_to_dots("z"),
            Err(ParseError::InvalidBraille {
                character: Some('z')
            })
        );
    }

    #[test]
    fn braille_chars_test() {
        assert_eq!(
            braille_chars("1-1"),
            Ok(vec![
                HashSet::from([BrailleDot::Dot1]),
                HashSet::from([BrailleDot::Dot1])
            ])
        );
        assert_eq!(
            braille_chars("1-"),
            Err(ParseError::InvalidBraille { character: None })
        );
        assert_eq!(
            braille_chars("-1"),
            Err(ParseError::InvalidBraille { character: None })
        );
        assert_eq!(
            braille_chars("-"),
            Err(ParseError::InvalidBraille { character: None })
        );
        assert_eq!(
            braille_chars(""),
            Err(ParseError::InvalidBraille { character: None })
        );
    }
}
