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

pub fn is_braille_dot(c: char) -> bool {
    matches!(c, '0'..='9' | 'a'..='f')
}

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

fn dot_to_hex(dot: &BrailleDot) -> u32 {
    match dot {
        BrailleDot::Dot0 => 0x0000,
        BrailleDot::Dot1 => 0x0001,
        BrailleDot::Dot2 => 0x0002,
        BrailleDot::Dot3 => 0x0004,
        BrailleDot::Dot4 => 0x0008,
        BrailleDot::Dot5 => 0x0010,
        BrailleDot::Dot6 => 0x0020,
        BrailleDot::Dot7 => 0x0040,
        BrailleDot::Dot8 => 0x0080,
        BrailleDot::Dot9 => 0x0100,
        BrailleDot::DotA => 0x0200,
        BrailleDot::DotB => 0x0400,
        BrailleDot::DotC => 0x0800,
        BrailleDot::DotD => 0x1000,
        BrailleDot::DotE => 0x2000,
        BrailleDot::DotF => 0x4000,
    }
}

fn has_virtual_dots(char: &BrailleChar) -> bool {
    let virtual_dots = HashSet::from([
        BrailleDot::Dot9,
        BrailleDot::DotA,
        BrailleDot::DotB,
        BrailleDot::DotC,
        BrailleDot::DotD,
        BrailleDot::DotE,
        BrailleDot::DotF,
    ]);
    !virtual_dots
        .intersection(char)
        .collect::<HashSet<_>>()
        .is_empty()
}

// FIXME: the following two functions should be defined as associated
// functions, i.e. inside an impl block for BrailleChar or as an
// implementation of the From trait. Both solutions would probably
// require the newtype pattern as we do not own these types, see
// https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#using-the-newtype-pattern-to-implement-external-traits-on-external-types
fn dot_to_unicode(dot: &BrailleChar) -> char {
    let unicode_plane = if has_virtual_dots(dot) {
        0xF0000 // Unicode Supplementary Private Use Area-A
    } else {
        0x2800 // braille patterns
    };
    let unicode = dot
        .iter()
        .map(dot_to_hex)
        .fold(unicode_plane, |acc, x| acc | x);
    char::from_u32(unicode).unwrap()
}

/// Map `BrailleChars` to a string containing unicode braille
pub fn dots_to_unicode(dots: &BrailleChars) -> String {
    dots.iter().map(dot_to_unicode).collect()
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

    #[test]
    fn dots_to_unicode_test() {
        assert_eq!(
            dots_to_unicode(&vec![HashSet::from([BrailleDot::Dot1, BrailleDot::Dot8])]),
            "⢁".to_string()
        );
        assert_eq!(
            dots_to_unicode(&vec![HashSet::from([BrailleDot::Dot1, BrailleDot::Dot9])]),
            "\u{f0101}".to_string()
        );
    }
}
