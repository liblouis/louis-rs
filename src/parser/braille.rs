use std::collections::HashSet;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Invalid braille {character:?}")]
    InvalidBraille { character: Option<char> },
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum BrailleDot {
    Dot0,
    Dot1,
    Dot2,
    Dot3,
    Dot4,
    Dot5,
    Dot6,
    Dot7,
    Dot8,
    Dot9,
    DotA,
    DotB,
    DotC,
    DotD,
    DotE,
    DotF,
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
        '0' => Ok(BrailleDot::Dot0),
        '1' => Ok(BrailleDot::Dot1),
        '2' => Ok(BrailleDot::Dot2),
        '3' => Ok(BrailleDot::Dot3),
        '4' => Ok(BrailleDot::Dot4),
        '5' => Ok(BrailleDot::Dot5),
        '6' => Ok(BrailleDot::Dot6),
        '7' => Ok(BrailleDot::Dot7),
        '8' => Ok(BrailleDot::Dot8),
        '9' => Ok(BrailleDot::Dot9),
        'a' => Ok(BrailleDot::DotA),
        'b' => Ok(BrailleDot::DotB),
        'c' => Ok(BrailleDot::DotC),
        'd' => Ok(BrailleDot::DotD),
        'e' => Ok(BrailleDot::DotE),
        'f' => Ok(BrailleDot::DotF),
        invalid => Err(ParseError::InvalidBraille {
            character: Some(invalid),
        }),
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
