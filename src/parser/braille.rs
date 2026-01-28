use std::collections::HashMap;

use enumset::{EnumSet, EnumSetType, enum_set};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Invalid braille {character:?}")]
    InvalidBraille { character: Option<char> },
}

#[derive(EnumSetType, Debug)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrailleChar(EnumSet<BrailleDot>);

impl From<EnumSet<BrailleDot>> for BrailleChar {
    fn from(value: EnumSet<BrailleDot>) -> Self {
        BrailleChar(value)
    }
}

impl BrailleChar {
    fn has_virtual_dots(&self) -> bool {
        let virtual_dots = enum_set!(
            BrailleDot::Dot9 |
            BrailleDot::DotA |
            BrailleDot::DotB |
            BrailleDot::DotC |
                BrailleDot::DotD |
                BrailleDot::DotE |
                BrailleDot::DotF |
        );
        !virtual_dots.intersection(self.0).is_empty()
    }

    pub fn to_unicode(&self) -> char {
        let unicode_plane = if self.has_virtual_dots() {
            0xF0000 // Unicode Supplementary Private Use Area-A
        } else {
            0x2800 // braille patterns
        };
        let unicode = self
            .0
            .iter()
            .map(|dot| dot_to_hex(&dot))
            .fold(unicode_plane, |acc, x| acc | x);
        char::from_u32(unicode).unwrap()
    }
}

impl std::fmt::Display for BrailleChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_unicode())
    }
}

impl FromIterator<BrailleDot> for BrailleChar {
    fn from_iter<T: IntoIterator<Item = BrailleDot>>(iter: T) -> Self {
        BrailleChar(EnumSet::from_iter(iter))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrailleChars(Vec<BrailleChar>);

impl std::ops::Deref for BrailleChars {
    type Target = Vec<BrailleChar>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<BrailleChar>> for BrailleChars {
    fn from(value: Vec<BrailleChar>) -> Self {
        BrailleChars(value)
    }
}

impl std::fmt::Display for BrailleChars {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|b| b.to_unicode()).collect::<String>()
        )
    }
}

impl FromIterator<BrailleChar> for BrailleChars {
    fn from_iter<T: IntoIterator<Item = BrailleChar>>(iter: T) -> Self {
        BrailleChars(iter.into_iter().collect())
    }
}

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

/// Map char to dots according to North American Braille Computer Code (NABCC)
///
/// A fallback mapping for character to braille in case the table does
/// not provide a mapping. This is used as a last resort when printing
/// unicode escapes for undefined characters when the table does not
/// define the character mappings that are needed.
pub fn fallback(ch: char) -> char {
    let north_american_braille_computer_code: HashMap<char, BrailleChar> = HashMap::from([
        (
            '0',
            BrailleChar(BrailleDot::Dot3 | BrailleDot::Dot5 | BrailleDot::Dot6),
        ),
        ('1', BrailleChar(enum_set!(BrailleDot::Dot2))),
        ('2', BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot3)),
        ('3', BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot5)),
        (
            '4',
            BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot5 | BrailleDot::Dot6),
        ),
        ('5', BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot6)),
        (
            '6',
            BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot3 | BrailleDot::Dot5),
        ),
        (
            '7',
            BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot3 | BrailleDot::Dot5 | BrailleDot::Dot6),
        ),
        (
            '8',
            BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot3 | BrailleDot::Dot6),
        ),
        ('9', BrailleChar(BrailleDot::Dot2 | BrailleDot::Dot5)),
        ('a', BrailleChar(enum_set!(BrailleDot::Dot1))),
        ('b', BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot2)),
        ('c', BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot4)),
        (
            'd',
            BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot4 | BrailleDot::Dot5),
        ),
        ('e', BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot5)),
        (
            'f',
            BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot2 | BrailleDot::Dot4),
        ),
        (
            '\\',
            BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot2 | BrailleDot::Dot5 | BrailleDot::Dot6),
        ),
        (
            'x',
            BrailleChar(BrailleDot::Dot1 | BrailleDot::Dot3 | BrailleDot::Dot4 | BrailleDot::Dot6),
        ),
    ]);

    let dots = north_american_braille_computer_code.get(&ch).unwrap();
    dots.to_unicode()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chars_to_dots() {
        assert_eq!(
            chars_to_dots("123"),
            Ok(BrailleChar(enum_set!(
                BrailleDot::Dot1 | BrailleDot::Dot2 | BrailleDot::Dot3
            )))
        );
        assert_eq!(
            chars_to_dots("1a"),
            Ok(BrailleChar(enum_set!(BrailleDot::Dot1 | BrailleDot::DotA)))
        );
        assert_eq!(
            chars_to_dots("a"),
            Ok(BrailleChar(enum_set!(BrailleDot::DotA)))
        );
        assert_eq!(
            chars_to_dots("z"),
            Err(ParseError::InvalidBraille {
                character: Some('z')
            })
        );
    }

    #[test]
    fn test_braille_chars() {
        assert_eq!(
            braille_chars("1-1"),
            Ok(BrailleChars(vec![
                BrailleChar(enum_set!(BrailleDot::Dot1)),
                BrailleChar(enum_set!(BrailleDot::Dot1))
            ]))
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
    fn test_dots_to_unicode() {
        assert_eq!(
            BrailleChars(vec![BrailleChar(enum_set!(
                BrailleDot::Dot1 | BrailleDot::Dot8
            ))])
            .to_string(),
            "⢁".to_string()
        );
        assert_eq!(
            BrailleChars(vec![BrailleChar(enum_set!(
                BrailleDot::Dot1 | BrailleDot::Dot9
            ))])
            .to_string(),
            "\u{f0101}".to_string()
        );
    }
}
